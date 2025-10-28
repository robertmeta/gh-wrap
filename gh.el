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
    (define-key map (kbd "?") 'gh-help)
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
    (define-key map (kbd "?") 'gh-help)
    map)
  "Keymap for `gh-issue-mode'.")

(defvar gh-pr-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'gh-pr-view-diff)
    (define-key map (kbd "c") 'gh-pr-view-checks)
    (define-key map (kbd "C") 'gh-pr-view-comment)
    (define-key map (kbd "r") 'gh-pr-view-review)
    (define-key map (kbd "a") 'gh-pr-view-approve)
    (define-key map (kbd "m") 'gh-pr-view-merge)
    (define-key map (kbd "o") 'gh-pr-view-open-browser)
    (define-key map (kbd "g") 'gh-pr-view-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "?") 'gh-help)
    map)
  "Keymap for `gh-pr-view-mode'.")

(defvar gh-issue-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C") 'gh-issue-view-comment)
    (define-key map (kbd "e") 'gh-issue-view-edit)
    (define-key map (kbd "o") 'gh-issue-view-open-browser)
    (define-key map (kbd "g") 'gh-issue-view-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "?") 'gh-help)
    map)
  "Keymap for `gh-issue-view-mode'.")

(defvar-local gh-current-filters nil
  "Current filters applied to the list.")

(defvar-local gh-items-data nil
  "Cached items data for the current buffer.")

(defvar-local gh-list-type nil
  "Type of list: \\='pr or \\='issue.")

(defvar-local gh-current-pr-number nil
  "The PR number for the current PR view buffer.")

(defvar-local gh-current-issue-number nil
  "The issue number for the current issue view buffer.")

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
         (author (alist-get 'login (alist-get 'author pr))))
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
  (let ((inhibit-read-only t)
        (evil-enabled (and (boundp 'evil-mode) evil-mode
                          (boundp 'evil-state) (eq evil-state 'normal))))
    (erase-buffer)
    (insert (format "%s\n" title))
    (insert (format "Found %d items\n\n" (length items)))
    (if (eq type 'pr)
        (if evil-enabled
            ;; Evil mode keybindings for PRs
            (progn
              (insert "Commands: [RET] view  [d] diff  [c] checks  [C] comment  [r] review  [m] merge  [o] browser  [R/gr] refresh  [q] quit  [?] help\n")
              (insert "Filters:  [s] state  [a] author  [A] assignee  [M] my PRs  [i] issues  [j/k] move\n\n"))
          ;; Emacs keybindings for PRs
          (progn
            (insert "Commands: [RET] view  [d] diff  [c] checks  [C] comment  [r] review  [m] merge  [o] browser  [g] refresh  [q] quit  [?] help\n")
            (insert "Filters:  [s] state  [a] author  [A] assignee  [M] my PRs  [i] issues\n\n")))
      (if evil-enabled
          ;; Evil mode keybindings for Issues
          (progn
            (insert "Commands: [RET] view  [C] comment  [e] edit  [o] browser  [R/gr] refresh  [q] quit  [?] help\n")
            (insert "Filters:  [s] state  [a] author  [A] assignee  [M] my issues  [p] PRs  [j/k] move\n\n"))
        ;; Emacs keybindings for Issues
        (progn
          (insert "Commands: [RET] view  [C] comment  [e] edit  [o] browser  [g] refresh  [q] quit  [?] help\n")
          (insert "Filters:  [s] state  [a] author  [A] assignee  [M] my issues  [p] PRs\n\n"))))
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
  (let ((output (gh--run-command "pr" "view" (number-to-string pr-number)))
        (comments (gh--run-command "pr" "view" (number-to-string pr-number) "--comments")))
    (with-current-buffer (get-buffer-create (format "*GitHub PR #%d*" pr-number))
      (gh-pr-view-mode)
      (setq gh-current-pr-number pr-number)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        ;; Add comments section
        (when (and comments (not (string-empty-p comments)))
          (goto-char (point-max))
          (insert "\n\n")
          (insert (make-string 80 ?─) "\n")
          (insert "COMMENTS\n")
          (insert (make-string 80 ?─) "\n\n")
          (insert comments))
        ;; Clean up control characters
        (goto-char (point-min))
        (while (re-search-forward "[\r\f]" nil t)
          (replace-match ""))
        (goto-char (point-min)))
      (switch-to-buffer (current-buffer))
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
  "Review PR at point with a choice of review type."
  (interactive)
  (let ((pr (gh--get-item-at-point)))
    (when pr
      (let* ((number (alist-get 'number pr))
             (review-type (completing-read "Review type: "
                                          '("approve" "comment" "request-changes")
                                          nil t)))
        (gh--run-command "pr" "review" (concat "--" review-type) (number-to-string number))
        (message "Submitted %s review for PR #%d" review-type number)
        (gh-refresh)))))

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
  (let ((output (gh--run-command "issue" "view" (number-to-string issue-number)))
        (comments (gh--run-command "issue" "view" (number-to-string issue-number) "--comments")))
    (with-current-buffer (get-buffer-create (format "*GitHub Issue #%d*" issue-number))
      (gh-issue-view-mode)
      (setq gh-current-issue-number issue-number)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        ;; Add comments section
        (when (and comments (not (string-empty-p comments)))
          (goto-char (point-max))
          (insert "\n\n")
          (insert (make-string 80 ?─) "\n")
          (insert "COMMENTS\n")
          (insert (make-string 80 ?─) "\n\n")
          (insert comments))
        ;; Clean up control characters
        (goto-char (point-min))
        (while (re-search-forward "[\r\f]" nil t)
          (replace-match ""))
        (goto-char (point-min)))
      (switch-to-buffer (current-buffer))
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

;;; PR View Commands (for use in gh-pr-view-mode)

(defun gh-pr-view-diff ()
  "Show diff for the current PR."
  (interactive)
  (when gh-current-pr-number
    (let ((output (gh--run-command "pr" "diff" (number-to-string gh-current-pr-number))))
      (with-current-buffer (get-buffer-create (format "*GitHub PR #%d Diff*" gh-current-pr-number))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert output)
          (diff-mode)
          (goto-char (point-min))
          (switch-to-buffer (current-buffer))))
      (message "Showing diff for PR #%d" gh-current-pr-number))))

(defun gh-pr-view-checks ()
  "Show checks for the current PR."
  (interactive)
  (when gh-current-pr-number
    (let ((output (gh--run-command "pr" "checks" (number-to-string gh-current-pr-number))))
      (message "%s" output))))

(defun gh-pr-view-comment ()
  "Add comment to the current PR."
  (interactive)
  (when gh-current-pr-number
    (gh--run-command "pr" "comment" (number-to-string gh-current-pr-number))
    (message "Adding comment to PR #%d..." gh-current-pr-number)))

(defun gh-pr-view-review ()
  "Review the current PR with a choice of review type."
  (interactive)
  (when gh-current-pr-number
    (let ((review-type (completing-read "Review type: "
                                        '("approve" "comment" "request-changes")
                                        nil t)))
      (gh--run-command "pr" "review" (concat "--" review-type) (number-to-string gh-current-pr-number))
      (message "Submitted %s review for PR #%d" review-type gh-current-pr-number)
      (gh-pr-view-refresh))))

(defun gh-pr-view-approve ()
  "Approve the current PR."
  (interactive)
  (when gh-current-pr-number
    (when (y-or-n-p (format "Approve PR #%d? " gh-current-pr-number))
      (gh--run-command "pr" "review" "--approve" (number-to-string gh-current-pr-number))
      (message "Approved PR #%d" gh-current-pr-number)
      (gh-pr-view-refresh))))

(defun gh-pr-view-merge ()
  "Merge the current PR."
  (interactive)
  (when gh-current-pr-number
    (when (y-or-n-p (format "Merge PR #%d? " gh-current-pr-number))
      (gh--run-command "pr" "merge" (number-to-string gh-current-pr-number))
      (message "Merged PR #%d" gh-current-pr-number)
      (gh-pr-view-refresh))))

(defun gh-pr-view-open-browser ()
  "Open the current PR in browser."
  (interactive)
  (when gh-current-pr-number
    (gh--run-command "pr" "view" "--web" (number-to-string gh-current-pr-number))
    (message "Opening PR #%d in browser..." gh-current-pr-number)))

(defun gh-pr-view-refresh ()
  "Refresh the current PR view."
  (interactive)
  (when gh-current-pr-number
    (gh-view-pr gh-current-pr-number)))

;;; Issue View Commands (for use in gh-issue-view-mode)

(defun gh-issue-view-comment ()
  "Add comment to the current issue."
  (interactive)
  (when gh-current-issue-number
    (gh--run-command "issue" "comment" (number-to-string gh-current-issue-number))
    (message "Adding comment to issue #%d..." gh-current-issue-number)))

(defun gh-issue-view-edit ()
  "Edit the current issue."
  (interactive)
  (when gh-current-issue-number
    (gh--run-command "issue" "edit" (number-to-string gh-current-issue-number))
    (message "Editing issue #%d..." gh-current-issue-number)
    (gh-issue-view-refresh)))

(defun gh-issue-view-open-browser ()
  "Open the current issue in browser."
  (interactive)
  (when gh-current-issue-number
    (gh--run-command "issue" "view" "--web" (number-to-string gh-current-issue-number))
    (message "Opening issue #%d in browser..." gh-current-issue-number)))

(defun gh-issue-view-refresh ()
  "Refresh the current issue view."
  (interactive)
  (when gh-current-issue-number
    (gh-view-issue gh-current-issue-number)))

;;; Help function

(defun gh-help ()
  "Display help for GitHub mode keybindings."
  (interactive)
  (let ((evil-enabled (and (boundp 'evil-mode) evil-mode)))
    (with-current-buffer (get-buffer-create "*GitHub Help*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "GitHub Mode Help\n" 'face 'bold))
        (insert (make-string 80 ?=) "\n\n")

        ;; Features section
        (insert (propertize "FEATURES:\n" 'face 'bold))
        (insert "  • View and manage GitHub pull requests and issues\n")
        (insert "  • View PR diffs, checks, and comments\n")
        (insert "  • Review and merge pull requests\n")
        (insert "  • Filter by state, author, and assignee\n")
        (insert "  • Open items in web browser\n")
        (insert "  • Full Emacspeak integration for screen readers\n")
        (insert "  • Evil mode support with vim-like keybindings\n\n")

        ;; PR Mode keybindings
        (insert (propertize "PULL REQUEST LIST MODE\n" 'face 'bold))
        (insert (make-string 80 ?-) "\n\n")

        (insert (propertize "Emacs Keybindings:\n" 'face 'bold))
        (insert "  RET     - View PR details\n")
        (insert "  d       - Show PR diff\n")
        (insert "  c       - Show PR checks\n")
        (insert "  C       - Add comment\n")
        (insert "  r       - Review PR (approve/comment/request-changes)\n")
        (insert "  m       - Merge PR\n")
        (insert "  o       - Open PR in browser\n")
        (insert "  g       - Refresh\n")
        (insert "  s       - Filter by state\n")
        (insert "  a       - Filter by author\n")
        (insert "  A       - Filter by assignee\n")
        (insert "  M       - Show my PRs\n")
        (insert "  i       - Switch to issues\n")
        (insert "  n       - Next line\n")
        (insert "  p       - Previous line\n")
        (insert "  q       - Quit window\n")
        (insert "  ?       - Show this help\n\n")

        (when evil-enabled
          (insert (propertize "Evil Mode Keybindings:\n" 'face 'bold))
          (insert "  RET     - View PR details\n")
          (insert "  d       - Show PR diff\n")
          (insert "  c       - Show PR checks\n")
          (insert "  C       - Add comment\n")
          (insert "  r       - Review PR\n")
          (insert "  R       - Refresh (uppercase variant)\n")
          (insert "  m       - Merge PR\n")
          (insert "  M       - Show my PRs\n")
          (insert "  o       - Open in browser\n")
          (insert "  s       - Filter by state\n")
          (insert "  a       - Filter by author\n")
          (insert "  A       - Filter by assignee\n")
          (insert "  i       - Switch to issues\n")
          (insert "  j, k    - Move down/up\n")
          (insert "  gg      - Jump to top\n")
          (insert "  G       - Jump to bottom\n")
          (insert "  gr      - Refresh (Vim-style)\n")
          (insert "  q       - Quit window\n")
          (insert "  ZZ, ZQ  - Quit window (Vim-style)\n")
          (insert "  ?       - Show this help\n\n"))

        ;; Issue Mode keybindings
        (insert (propertize "ISSUE LIST MODE\n" 'face 'bold))
        (insert (make-string 80 ?-) "\n\n")

        (insert (propertize "Emacs Keybindings:\n" 'face 'bold))
        (insert "  RET     - View issue details\n")
        (insert "  C       - Add comment\n")
        (insert "  e       - Edit issue\n")
        (insert "  o       - Open issue in browser\n")
        (insert "  g       - Refresh\n")
        (insert "  s       - Filter by state\n")
        (insert "  a       - Filter by author\n")
        (insert "  A       - Filter by assignee\n")
        (insert "  M       - Show my issues\n")
        (insert "  p       - Switch to PRs\n")
        (insert "  n       - Next line\n")
        (insert "  P       - Previous line\n")
        (insert "  q       - Quit window\n")
        (insert "  ?       - Show this help\n\n")

        (when evil-enabled
          (insert (propertize "Evil Mode Keybindings:\n" 'face 'bold))
          (insert "  RET     - View issue details\n")
          (insert "  C       - Add comment\n")
          (insert "  e, E    - Edit issue\n")
          (insert "  o       - Open in browser\n")
          (insert "  R       - Refresh (uppercase variant)\n")
          (insert "  s       - Filter by state\n")
          (insert "  a       - Filter by author\n")
          (insert "  A       - Filter by assignee\n")
          (insert "  M       - Show my issues\n")
          (insert "  p       - Switch to PRs\n")
          (insert "  j, k    - Move down/up\n")
          (insert "  gg      - Jump to top\n")
          (insert "  G       - Jump to bottom\n")
          (insert "  gr      - Refresh (Vim-style)\n")
          (insert "  q       - Quit window\n")
          (insert "  ZZ, ZQ  - Quit window (Vim-style)\n")
          (insert "  ?       - Show this help\n\n"))

        ;; PR View Mode
        (insert (propertize "PULL REQUEST DETAIL VIEW MODE\n" 'face 'bold))
        (insert (make-string 80 ?-) "\n\n")

        (insert (propertize "Emacs Keybindings:\n" 'face 'bold))
        (insert "  d       - Show diff\n")
        (insert "  c       - Show checks\n")
        (insert "  C       - Add comment\n")
        (insert "  r       - Review PR\n")
        (insert "  a       - Approve PR\n")
        (insert "  m       - Merge PR\n")
        (insert "  o       - Open in browser\n")
        (insert "  g       - Refresh\n")
        (insert "  q       - Quit window\n")
        (insert "  ?       - Show this help\n\n")

        (when evil-enabled
          (insert (propertize "Evil Mode Keybindings:\n" 'face 'bold))
          (insert "  d       - Show diff\n")
          (insert "  c       - Show checks\n")
          (insert "  C       - Add comment\n")
          (insert "  r       - Review PR\n")
          (insert "  R       - Refresh (uppercase variant)\n")
          (insert "  a       - Approve PR\n")
          (insert "  m       - Merge PR\n")
          (insert "  o       - Open in browser\n")
          (insert "  j, k    - Move down/up\n")
          (insert "  gg      - Jump to top\n")
          (insert "  G       - Jump to bottom\n")
          (insert "  gr      - Refresh (Vim-style)\n")
          (insert "  q       - Quit window\n")
          (insert "  ZZ, ZQ  - Quit window (Vim-style)\n")
          (insert "  ?       - Show this help\n\n"))

        ;; Issue View Mode
        (insert (propertize "ISSUE DETAIL VIEW MODE\n" 'face 'bold))
        (insert (make-string 80 ?-) "\n\n")

        (insert (propertize "Emacs Keybindings:\n" 'face 'bold))
        (insert "  C       - Add comment\n")
        (insert "  e       - Edit issue\n")
        (insert "  o       - Open in browser\n")
        (insert "  g       - Refresh\n")
        (insert "  q       - Quit window\n")
        (insert "  ?       - Show this help\n\n")

        (when evil-enabled
          (insert (propertize "Evil Mode Keybindings:\n" 'face 'bold))
          (insert "  C       - Add comment\n")
          (insert "  e, E    - Edit issue\n")
          (insert "  o       - Open in browser\n")
          (insert "  R       - Refresh (uppercase variant)\n")
          (insert "  j, k    - Move down/up\n")
          (insert "  gg      - Jump to top\n")
          (insert "  G       - Jump to bottom\n")
          (insert "  gr      - Refresh (Vim-style)\n")
          (insert "  q       - Quit window\n")
          (insert "  ZZ, ZQ  - Quit window (Vim-style)\n")
          (insert "  ?       - Show this help\n\n"))

        ;; Evil mode notice
        (when evil-enabled
          (insert (make-string 80 ?=) "\n")
          (insert (propertize "Evil Mode Active\n" 'face 'bold))
          (insert "Vim-style keybindings are enabled with:\n")
          (insert "  • Standard vim navigation (j/k for up/down)\n")
          (insert "  • gg/G for jump to top/bottom\n")
          (insert "  • r/R for refresh operations\n")
          (insert "  • gr for vim-style refresh\n")
          (insert "  • ZZ/ZQ for quit\n")
          (insert "  • Uppercase variants for common actions (E, R, etc.)\n\n"))

        (insert (make-string 80 ?=) "\n")
        (insert "Press 'q' to close this help buffer.\n")
        (goto-char (point-min))
        (view-mode)
        (switch-to-buffer (current-buffer))))))

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

(define-derived-mode gh-pr-view-mode special-mode "GitHub-PR-View"
  "Major mode for viewing a single GitHub pull request.

\\{gh-pr-view-mode-map}"
  (setq truncate-lines nil)
  (setq buffer-read-only t)
  (setq mode-line-format
        '("%e" mode-line-front-space
          mode-line-buffer-identification
          "  "
          "PR #" (:eval (when gh-current-pr-number (number-to-string gh-current-pr-number)))
          "  "
          mode-line-end-spaces)))

(define-derived-mode gh-issue-view-mode special-mode "GitHub-Issue-View"
  "Major mode for viewing a single GitHub issue.

\\{gh-issue-view-mode-map}"
  (setq truncate-lines nil)
  (setq buffer-read-only t)
  (setq mode-line-format
        '("%e" mode-line-front-space
          mode-line-buffer-identification
          "  "
          "Issue #" (:eval (when gh-current-issue-number (number-to-string gh-current-issue-number)))
          "  "
          mode-line-end-spaces)))

(defun gh--emacspeak-post-command ()
  "Emacspeak post-command hook for gh mode."
  (when (and (featurep 'emacspeak)
             (memq major-mode '(gh-pr-mode gh-issue-mode))
             (memq this-command '(next-line previous-line)))
    (gh--emacspeak-speak-line)))

;;; Evil mode integration

(with-eval-after-load 'evil
  ;; Use normal state for vim-like bindings
  (evil-set-initial-state 'gh-pr-mode 'normal)
  (evil-set-initial-state 'gh-issue-mode 'normal)
  (evil-set-initial-state 'gh-pr-view-mode 'normal)
  (evil-set-initial-state 'gh-issue-view-mode 'normal)

  ;; Define Evil keybindings for gh-pr-mode
  (evil-define-key 'normal gh-pr-mode-map
    (kbd "RET") 'gh-view-pr-at-point
    "j" 'next-line
    "k" 'previous-line
    "d" 'gh-pr-diff-at-point
    "c" 'gh-pr-checks-at-point
    "C" 'gh-pr-comment-at-point
    "r" 'gh-pr-review-at-point
    "R" 'gh-refresh                          ; Uppercase variant for refresh
    "m" 'gh-pr-merge-at-point
    "M" 'gh-my-prs                           ; Keep M for my PRs
    "o" 'gh-open-in-browser-at-point
    "g" nil                                  ; Prefix key for 'gr' and 'gg'
    "gr" 'gh-refresh                         ; Vim-style refresh
    "gg" 'evil-goto-first-line
    "G" 'evil-goto-line
    "s" 'gh-pr-filter-by-state
    "a" 'gh-pr-filter-by-author
    "A" 'gh-pr-filter-by-assignee
    "i" 'gh-list-issues
    "q" 'quit-window
    "ZZ" 'quit-window                        ; Vim-style quit
    "ZQ" 'quit-window                        ; Vim-style force quit
    "?" 'gh-help)

  ;; Define Evil keybindings for gh-issue-mode
  (evil-define-key 'normal gh-issue-mode-map
    (kbd "RET") 'gh-view-issue-at-point
    "j" 'next-line
    "k" 'previous-line
    "C" 'gh-issue-comment-at-point
    "e" 'gh-issue-edit-at-point
    "E" 'gh-issue-edit-at-point              ; Uppercase variant
    "o" 'gh-open-in-browser-at-point
    "g" nil                                  ; Prefix key for 'gr' and 'gg'
    "gr" 'gh-refresh                         ; Vim-style refresh
    "R" 'gh-refresh                          ; Uppercase variant for refresh
    "gg" 'evil-goto-first-line
    "G" 'evil-goto-line
    "s" 'gh-issue-filter-by-state
    "a" 'gh-issue-filter-by-author
    "A" 'gh-issue-filter-by-assignee
    "M" 'gh-my-issues
    "p" 'gh-list-prs
    "q" 'quit-window
    "ZZ" 'quit-window                        ; Vim-style quit
    "ZQ" 'quit-window                        ; Vim-style force quit
    "?" 'gh-help)

  ;; Define Evil keybindings for gh-pr-view-mode
  (evil-define-key 'normal gh-pr-view-mode-map
    "d" 'gh-pr-view-diff
    "c" 'gh-pr-view-checks
    "C" 'gh-pr-view-comment
    "r" 'gh-pr-view-review
    "R" 'gh-pr-view-refresh                  ; Uppercase variant for refresh
    "a" 'gh-pr-view-approve
    "m" 'gh-pr-view-merge
    "o" 'gh-pr-view-open-browser
    "g" nil                                  ; Prefix key for 'gr' and 'gg'
    "gr" 'gh-pr-view-refresh                 ; Vim-style refresh
    "gg" 'evil-goto-first-line
    "G" 'evil-goto-line
    "j" 'next-line
    "k" 'previous-line
    "q" 'quit-window
    "ZZ" 'quit-window                        ; Vim-style quit
    "ZQ" 'quit-window                        ; Vim-style force quit
    "?" 'gh-help)

  ;; Define Evil keybindings for gh-issue-view-mode
  (evil-define-key 'normal gh-issue-view-mode-map
    "C" 'gh-issue-view-comment
    "e" 'gh-issue-view-edit
    "E" 'gh-issue-view-edit                  ; Uppercase variant
    "o" 'gh-issue-view-open-browser
    "g" nil                                  ; Prefix key for 'gr' and 'gg'
    "gr" 'gh-issue-view-refresh              ; Vim-style refresh
    "R" 'gh-issue-view-refresh               ; Uppercase variant for refresh
    "gg" 'evil-goto-first-line
    "G" 'evil-goto-line
    "j" 'next-line
    "k" 'previous-line
    "q" 'quit-window
    "ZZ" 'quit-window                        ; Vim-style quit
    "ZQ" 'quit-window                        ; Vim-style force quit
    "?" 'gh-help))

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
