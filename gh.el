;;; gh.el --- Emacs interface for GitHub CLI -*- lexical-binding: t -*-

;; Author: Robert Melton
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, github, git

;;; Commentary:

;; This package provides an Emacs interface to the GitHub CLI (gh).
;; It allows you to view and manage pull requests and issues from within Emacs.

;;; Code:

(require 'json)

(defgroup gh nil
  "Interface to GitHub CLI."
  :group 'tools)

(defcustom gh-command "gh"
  "Path to the gh command-line tool."
  :type 'string
  :group 'gh)

(defcustom gh-default-limit 30
  "Default number of PRs/issues to fetch."
  :type 'integer
  :group 'gh)

(defvar gh-pr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'gh-view-pr-at-point)
    (define-key map (kbd "d") 'gh-pr-diff-at-point)
    (define-key map (kbd "c") 'gh-pr-checks-at-point)
    (define-key map (kbd "C") 'gh-pr-comment-at-point)
    (define-key map (kbd "r") 'gh-pr-review-at-point)
    (define-key map (kbd "m") 'gh-pr-merge-at-point)
    (define-key map (kbd "o") 'gh-open-in-browser-at-point)
    (define-key map (kbd "g") 'gh-refresh)
    (define-key map (kbd "s") 'gh-pr-filter-by-state)
    (define-key map (kbd "a") 'gh-pr-filter-by-author)
    (define-key map (kbd "A") 'gh-pr-filter-by-assignee)
    (define-key map (kbd "M") 'gh-my-prs)
    (define-key map (kbd "i") 'gh-list-issues)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `gh-pr-mode'.")

(defvar gh-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'gh-view-issue-at-point)
    (define-key map (kbd "C") 'gh-issue-comment-at-point)
    (define-key map (kbd "e") 'gh-issue-edit-at-point)
    (define-key map (kbd "o") 'gh-open-in-browser-at-point)
    (define-key map (kbd "g") 'gh-refresh)
    (define-key map (kbd "s") 'gh-issue-filter-by-state)
    (define-key map (kbd "a") 'gh-issue-filter-by-author)
    (define-key map (kbd "A") 'gh-issue-filter-by-assignee)
    (define-key map (kbd "M") 'gh-my-issues)
    (define-key map (kbd "p") 'gh-list-prs)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "P") 'previous-line)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `gh-issue-mode'.")

(defvar-local gh-current-filters nil
  "Current filters applied to the list.")

(defvar-local gh-items-data nil
  "Cached items data for the current buffer.")

(defvar-local gh-list-type nil
  "Type of list: 'pr or 'issue.")

;;; Utility functions

(defun gh--run-command (&rest args)
  "Run gh command with ARGS and return output."
  (with-temp-buffer
    (let ((exit-code (apply 'call-process gh-command nil t nil args)))
      (if (zerop exit-code)
          (buffer-string)
        (error "GitHub CLI command failed: %s" (buffer-string))))))

(defun gh--run-command-json (&rest args)
  "Run gh command with ARGS and parse JSON output."
  (let* ((output (apply 'gh--run-command args)))
    (if (string-empty-p output)
        nil
      (let ((json-array-type 'list))
        (json-read-from-string output)))))

(defun gh--get-item-at-point ()
  "Get the PR/issue data at point."
  (get-text-property (point) 'gh-item))

;;; Emacspeak integration

(defun gh--emacspeak-speak-line ()
  "Custom Emacspeak line speaking for gh."
  (when (and (boundp 'emacspeak-speak-mode) emacspeak-speak-mode)
    (let ((item (gh--get-item-at-point)))
      (when item
        (let* ((number (alist-get 'number item))
               (title (alist-get 'title item))
               (state (alist-get 'state item))
               (author (alist-get 'login (alist-get 'author item)))
               (type (if (eq gh-list-type 'pr) "pull request" "issue"))
               (speech-text (format "%s %d, %s, by %s, %s"
                                    type
                                    number
                                    title
                                    author
                                    state)))
          (dtk-speak speech-text))
        t))))

;;; Display functions

(defun gh--format-pr-line (pr)
  "Format PR for display."
  (let* ((number (alist-get 'number pr))
         (title (alist-get 'title pr))
         (state (alist-get 'state pr))
         (author (alist-get 'login (alist-get 'author pr)))
         (updated (alist-get 'updatedAt pr)))
    (format "#%-5d %-8s %-20s %s"
            number
            state
            (if (> (length author) 20)
                (substring author 0 17)
              author)
            title)))

(defun gh--format-issue-line (issue)
  "Format ISSUE for display."
  (gh--format-pr-line issue))

(defun gh--insert-item (item)
  "Insert ITEM (PR or issue) into the buffer."
  (let ((start (point))
        (line (if (eq gh-list-type 'pr)
                  (gh--format-pr-line item)
                (gh--format-issue-line item))))
    (insert line)
    (put-text-property start (point) 'gh-item item)
    ;; Add Emacspeak-specific spoken text
    (when (featurep 'emacspeak)
      (let* ((number (alist-get 'number item))
             (title (alist-get 'title item))
             (state (alist-get 'state item))
             (author (alist-get 'login (alist-get 'author item)))
             (type (if (eq gh-list-type 'pr) "pull request" "issue"))
             (speech-text (format "%s %d, %s, by %s, %s"
                                  type
                                  number
                                  title
                                  author
                                  state)))
        (put-text-property start (point) 'emacspeak-speak speech-text)))
    (insert "\n")))

(defun gh--display-items (items type title)
  "Display ITEMS of TYPE with TITLE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "%s\n" title))
    (insert (format "Found %d items\n\n" (length items)))
    (if (eq type 'pr)
        (progn
          (insert "Commands: [RET] view  [d] diff  [c] checks  [C] comment  [r] review  [m] merge  [o] browser  [g] refresh  [q] quit\n")
          (insert "Filters:  [s] state  [a] author  [A] assignee  [M] my PRs  [i] issues\n\n"))
      (insert "Commands: [RET] view  [C] comment  [e] edit  [o] browser  [g] refresh  [q] quit\n")
      (insert "Filters:  [s] state  [a] author  [A] assignee  [M] my issues  [p] PRs\n\n"))
    (insert (format "#%-5s %-8s %-20s %s\n" "NUM" "STATE" "AUTHOR" "TITLE"))
    (insert (make-string 80 ?-) "\n")
    (if (null items)
        (insert "No items found.\n")
      (dolist (item items)
        (gh--insert-item item)))
    (message "Loaded %d items" (length items))))

;;; Interactive commands - Pull Requests

;;;###autoload
(defun gh-list-prs (&optional state)
  "List GitHub pull requests with optional STATE filter."
  (interactive)
  (message "Loading pull requests...")
  (let* ((args (list "pr" "list"
                     "--limit" (number-to-string gh-default-limit)
                     "--json" "number,title,author,state,updatedAt"))
         (args (if state (append args (list "--state" state)) args))
         (prs (apply 'gh--run-command-json args)))
    (with-current-buffer (get-buffer-create "*GitHub Pull Requests*")
      (gh-pr-mode)
      (setq gh-list-type 'pr)
      (setq gh-items-data prs)
      (setq gh-current-filters (if state (list "--state" state) nil))
      (gh--display-items prs 'pr "GitHub Pull Requests")
      (goto-char (point-min))
      (forward-line 6)
      (switch-to-buffer (current-buffer)))))

(defun gh-my-prs ()
  "List pull requests created by me."
  (interactive)
  (message "Loading my pull requests...")
  (let* ((args (list "pr" "list"
                     "--author" "@me"
                     "--limit" (number-to-string gh-default-limit)
                     "--json" "number,title,author,state,updatedAt"))
         (prs (apply 'gh--run-command-json args)))
    (with-current-buffer (get-buffer-create "*GitHub Pull Requests (Mine)*")
      (gh-pr-mode)
      (setq gh-list-type 'pr)
      (setq gh-items-data prs)
      (setq gh-current-filters (list "--author" "@me"))
      (gh--display-items prs 'pr "My Pull Requests")
      (goto-char (point-min))
      (forward-line 6)
      (switch-to-buffer (current-buffer)))))

(defun gh-view-pr-at-point ()
  "View the PR at point."
  (interactive)
  (let ((pr (gh--get-item-at-point)))
    (when pr
      (let ((number (alist-get 'number pr)))
        (gh-view-pr number)))))

(defun gh-view-pr (pr-number)
  "View PR with PR-NUMBER."
  (interactive "nPR number: ")
  (message "Loading PR #%d..." pr-number)
  (let ((output (gh--run-command "pr" "view" (number-to-string pr-number))))
    (with-current-buffer (get-buffer-create (format "*GitHub PR #%d*" pr-number))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (goto-char (point-min))
        (view-mode)
        (switch-to-buffer (current-buffer)))
      (message "Loaded PR #%d" pr-number))))

(defun gh-pr-diff-at-point ()
  "Show diff for PR at point."
  (interactive)
  (let ((pr (gh--get-item-at-point)))
    (when pr
      (let* ((number (alist-get 'number pr))
             (output (gh--run-command "pr" "diff" (number-to-string number))))
        (with-current-buffer (get-buffer-create (format "*GitHub PR #%d Diff*" number))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert output)
            (diff-mode)
            (goto-char (point-min))
            (switch-to-buffer (current-buffer))))
        (message "Showing diff for PR #%d" number)))))

(defun gh-pr-checks-at-point ()
  "Show checks for PR at point."
  (interactive)
  (let ((pr (gh--get-item-at-point)))
    (when pr
      (let* ((number (alist-get 'number pr))
             (output (gh--run-command "pr" "checks" (number-to-string number))))
        (message "%s" output)))))

(defun gh-pr-comment-at-point ()
  "Add comment to PR at point."
  (interactive)
  (let ((pr (gh--get-item-at-point)))
    (when pr
      (let ((number (alist-get 'number pr)))
        (gh--run-command "pr" "comment" (number-to-string number))
        (message "Adding comment to PR #%d..." number)))))

(defun gh-pr-review-at-point ()
  "Review PR at point."
  (interactive)
  (let ((pr (gh--get-item-at-point)))
    (when pr
      (let ((number (alist-get 'number pr)))
        (gh--run-command "pr" "review" (number-to-string number))
        (message "Reviewing PR #%d..." number)))))

(defun gh-pr-merge-at-point ()
  "Merge PR at point."
  (interactive)
  (let ((pr (gh--get-item-at-point)))
    (when pr
      (let ((number (alist-get 'number pr)))
        (when (y-or-n-p (format "Merge PR #%d? " number))
          (gh--run-command "pr" "merge" (number-to-string number))
          (message "Merged PR #%d" number)
          (gh-refresh))))))

(defun gh-open-in-browser-at-point ()
  "Open PR/issue at point in browser."
  (interactive)
  (let ((item (gh--get-item-at-point)))
    (when item
      (let ((number (alist-get 'number item)))
        (if (eq gh-list-type 'pr)
            (gh--run-command "pr" "view" "--web" (number-to-string number))
          (gh--run-command "issue" "view" "--web" (number-to-string number)))
        (message "Opening in browser...")))))

(defun gh-pr-filter-by-state (state)
  "Filter PRs by STATE."
  (interactive (list (completing-read "State: " '("open" "closed" "merged" "all") nil t)))
  (setq gh-current-filters (list "--state" state))
  (gh-refresh))

(defun gh-pr-filter-by-author (author)
  "Filter PRs by AUTHOR."
  (interactive "sAuthor (@me for yourself): ")
  (setq gh-current-filters (list "--author" author))
  (gh-refresh))

(defun gh-pr-filter-by-assignee (assignee)
  "Filter PRs by ASSIGNEE."
  (interactive "sAssignee (@me for yourself): ")
  (setq gh-current-filters (list "--assignee" assignee))
  (gh-refresh))

(defun gh-refresh ()
  "Refresh the current buffer."
  (interactive)
  (cond
   ((eq gh-list-type 'pr)
    (message "Refreshing pull requests...")
    (let* ((args (append (list "pr" "list"
                               "--limit" (number-to-string gh-default-limit)
                               "--json" "number,title,author,state,updatedAt")
                         gh-current-filters))
           (prs (apply 'gh--run-command-json args)))
      (setq gh-items-data prs)
      (let ((line (line-number-at-pos)))
        (gh--display-items prs 'pr "GitHub Pull Requests")
        (goto-char (point-min))
        (forward-line (1- line)))))
   ((eq gh-list-type 'issue)
    (message "Refreshing issues...")
    (let* ((args (append (list "issue" "list"
                               "--limit" (number-to-string gh-default-limit)
                               "--json" "number,title,author,state,updatedAt")
                         gh-current-filters))
           (issues (apply 'gh--run-command-json args)))
      (setq gh-items-data issues)
      (let ((line (line-number-at-pos)))
        (gh--display-items issues 'issue "GitHub Issues")
        (goto-char (point-min))
        (forward-line (1- line)))))))

;;; Interactive commands - Issues

;;;###autoload
(defun gh-list-issues (&optional state)
  "List GitHub issues with optional STATE filter."
  (interactive)
  (message "Loading issues...")
  (let* ((args (list "issue" "list"
                     "--limit" (number-to-string gh-default-limit)
                     "--json" "number,title,author,state,updatedAt"))
         (args (if state (append args (list "--state" state)) args))
         (issues (apply 'gh--run-command-json args)))
    (with-current-buffer (get-buffer-create "*GitHub Issues*")
      (gh-issue-mode)
      (setq gh-list-type 'issue)
      (setq gh-items-data issues)
      (setq gh-current-filters (if state (list "--state" state) nil))
      (gh--display-items issues 'issue "GitHub Issues")
      (goto-char (point-min))
      (forward-line 6)
      (switch-to-buffer (current-buffer)))))

(defun gh-my-issues ()
  "List issues created by me."
  (interactive)
  (message "Loading my issues...")
  (let* ((args (list "issue" "list"
                     "--author" "@me"
                     "--limit" (number-to-string gh-default-limit)
                     "--json" "number,title,author,state,updatedAt"))
         (issues (apply 'gh--run-command-json args)))
    (with-current-buffer (get-buffer-create "*GitHub Issues (Mine)*")
      (gh-issue-mode)
      (setq gh-list-type 'issue)
      (setq gh-items-data issues)
      (setq gh-current-filters (list "--author" "@me"))
      (gh--display-items issues 'issue "My Issues")
      (goto-char (point-min))
      (forward-line 6)
      (switch-to-buffer (current-buffer)))))

(defun gh-view-issue-at-point ()
  "View the issue at point."
  (interactive)
  (let ((issue (gh--get-item-at-point)))
    (when issue
      (let ((number (alist-get 'number issue)))
        (gh-view-issue number)))))

(defun gh-view-issue (issue-number)
  "View issue with ISSUE-NUMBER."
  (interactive "nIssue number: ")
  (message "Loading issue #%d..." issue-number)
  (let ((output (gh--run-command "issue" "view" (number-to-string issue-number))))
    (with-current-buffer (get-buffer-create (format "*GitHub Issue #%d*" issue-number))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (goto-char (point-min))
        (view-mode)
        (switch-to-buffer (current-buffer)))
      (message "Loaded issue #%d" issue-number))))

(defun gh-issue-comment-at-point ()
  "Add comment to issue at point."
  (interactive)
  (let ((issue (gh--get-item-at-point)))
    (when issue
      (let ((number (alist-get 'number issue)))
        (gh--run-command "issue" "comment" (number-to-string number))
        (message "Adding comment to issue #%d..." number)))))

(defun gh-issue-edit-at-point ()
  "Edit issue at point."
  (interactive)
  (let ((issue (gh--get-item-at-point)))
    (when issue
      (let ((number (alist-get 'number issue)))
        (gh--run-command "issue" "edit" (number-to-string number))
        (message "Editing issue #%d..." number)
        (gh-refresh)))))

(defun gh-issue-filter-by-state (state)
  "Filter issues by STATE."
  (interactive (list (completing-read "State: " '("open" "closed" "all") nil t)))
  (setq gh-current-filters (list "--state" state))
  (gh-refresh))

(defun gh-issue-filter-by-author (author)
  "Filter issues by AUTHOR."
  (interactive "sAuthor (@me for yourself): ")
  (setq gh-current-filters (list "--author" author))
  (gh-refresh))

(defun gh-issue-filter-by-assignee (assignee)
  "Filter issues by ASSIGNEE."
  (interactive "sAssignee (@me for yourself): ")
  (setq gh-current-filters (list "--assignee" assignee))
  (gh-refresh))

;;; Major modes

(define-derived-mode gh-pr-mode special-mode "GitHub-PR"
  "Major mode for viewing GitHub pull requests.

\\{gh-pr-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  ;; Set up Emacspeak integration
  (when (featurep 'emacspeak)
    (add-hook 'post-command-hook 'gh--emacspeak-post-command nil t)))

(define-derived-mode gh-issue-mode special-mode "GitHub-Issue"
  "Major mode for viewing GitHub issues.

\\{gh-issue-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  ;; Set up Emacspeak integration
  (when (featurep 'emacspeak)
    (add-hook 'post-command-hook 'gh--emacspeak-post-command nil t)))

(defun gh--emacspeak-post-command ()
  "Emacspeak post-command hook for gh mode."
  (when (and (featurep 'emacspeak)
             (memq major-mode '(gh-pr-mode gh-issue-mode))
             (memq this-command '(next-line previous-line)))
    (gh--emacspeak-speak-line)))

;;; Evil mode integration

(with-eval-after-load 'evil
  (evil-set-initial-state 'gh-pr-mode 'emacs)
  (evil-set-initial-state 'gh-issue-mode 'emacs))

;;; Emacspeak advice

(with-eval-after-load 'emacspeak
  ;; Override emacspeak-speak-line for gh modes
  (defadvice emacspeak-speak-line (around gh-mode activate)
    "Use custom line speaking in gh modes."
    (if (and (memq major-mode '(gh-pr-mode gh-issue-mode))
             (get-text-property (point) 'emacspeak-speak))
        (dtk-speak (get-text-property (point) 'emacspeak-speak))
      ad-do-it))

  (defadvice gh-list-prs (after emacspeak activate)
    "Provide auditory feedback when listing PRs."
    (when (ems-interactive-p)
      (emacspeak-icon 'open-object)))

  (defadvice gh-list-issues (after emacspeak activate)
    "Provide auditory feedback when listing issues."
    (when (ems-interactive-p)
      (emacspeak-icon 'open-object)))

  (defadvice gh-pr-merge-at-point (after emacspeak activate)
    "Provide auditory feedback when merging."
    (when (ems-interactive-p)
      (emacspeak-icon 'select-object))))

(provide 'gh)

;;; gh.el ends here
