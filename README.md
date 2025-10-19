# gh.el - Emacs Interface for GitHub CLI

An Emacs package providing a full-featured interface to the GitHub CLI (gh), with enhanced accessibility support for Emacspeak users.

## Features

- **View and manage pull requests** in an interactive list
- **View and manage issues** in an interactive list
- **View PR/issue details** with formatted display
- **Show PR diffs** in diff-mode
- **Check PR status** (CI checks)
- **Comment on PRs/issues**
- **Review PRs** with approval/changes workflow
- **Merge PRs** directly from Emacs
- **Filter by state, author, assignee**
- **"My PRs" and "My Issues"** quick views
- **Open in browser** for web-based actions
- **Enhanced Emacspeak support** with custom line reading and auditory icons
- **Evil mode integration** (starts in Emacs state)

## Requirements

- Emacs 27.1 or later
- [GitHub CLI](https://cli.github.com/) installed and authenticated
- Optional: Emacspeak for enhanced audio feedback

## Installation

### Install and Configure GitHub CLI

```bash
# Install gh
brew install gh

# Authenticate
gh auth login
```

### Install the Emacs package

#### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/robertmeta/gh-wrap.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (use-package gh
     :ensure nil
     :load-path "~/path/to/gh-wrap"
     :commands (gh-list-prs gh-list-issues gh-my-prs gh-my-issues)
     :init
     (setq gh-default-limit 30)
     :bind (("s-G p" . gh-list-prs)
            ("s-G i" . gh-list-issues)
            ("s-G m" . gh-my-prs)))
   ```

## Usage

### Commands

- `M-x gh-list-prs` - List pull requests (or `s-G p`)
- `M-x gh-my-prs` - List my pull requests (or `s-G m`)
- `M-x gh-list-issues` - List issues (or `s-G i`)
- `M-x gh-my-issues` - List my issues

### Keybindings in PR List (gh-pr-mode)

| Key   | Command                          |
|-------|----------------------------------|
| `RET` | View PR details                  |
| `d`   | Show diff                        |
| `c`   | Show CI checks                   |
| `C`   | Add comment                      |
| `r`   | Review PR                        |
| `m`   | Merge PR                         |
| `o`   | Open in browser                  |
| `g`   | Refresh view                     |
| `s`   | Filter by state                  |
| `a`   | Filter by author                 |
| `A`   | Filter by assignee               |
| `M`   | Show my PRs                      |
| `i`   | Switch to issues                 |
| `n/p` | Next/previous line               |
| `q`   | Quit window                      |

### Keybindings in Issue List (gh-issue-mode)

| Key   | Command                          |
|-------|----------------------------------|
| `RET` | View issue details               |
| `C`   | Add comment                      |
| `e`   | Edit issue                       |
| `o`   | Open in browser                  |
| `g`   | Refresh view                     |
| `s`   | Filter by state                  |
| `a`   | Filter by author                 |
| `A`   | Filter by assignee               |
| `M`   | Show my issues                   |
| `p`   | Switch to PRs                    |
| `n/P` | Next/previous line               |
| `q`   | Quit window                      |

### Display Format

Pull requests and issues are displayed in a table format:

```
GitHub Pull Requests
Found 15 items

Commands: [RET] view  [d] diff  [c] checks  [C] comment  [r] review  [m] merge  [o] browser  [g] refresh  [q] quit
Filters:  [s] state  [a] author  [A] assignee  [M] my PRs  [i] issues

#NUM   STATE    AUTHOR               TITLE
--------------------------------------------------------------------------------
#301   OPEN     robertmeta           Fix session cookie maxAge calculation
#299   MERGED   jane-smith           Add user authentication
#297   OPEN     john-doe             Update documentation
```

## Emacspeak Support

This package includes comprehensive Emacspeak integration:

### Custom Line Reading

When navigating with Emacspeak:
- **Visual**: `#301   OPEN     robertmeta           Fix session cookie maxAge calculation`
- **Spoken**: "pull request 301, Fix session cookie maxAge calculation, by robertmeta, OPEN"

The spoken format:
- Starts with type (pull request/issue) and number
- Includes title (most important)
- States the author
- Announces state (open/closed/merged)
- Skips visual formatting

### Auditory Icons

- **List PRs/issues**: "open-object" sound
- **Merge PR**: "select-object" sound

### Voice Personalities

Future enhancement: Different personalities for different PR/issue states.

## Configuration

### Customization Variables

```elisp
;; Default number of items to fetch
(setq gh-default-limit 30)

;; Path to gh command (if not in PATH)
(setq gh-command "/opt/homebrew/bin/gh")
```

### Example Configuration

```elisp
(use-package gh
  :ensure nil
  :load-path "~/projects/robertmeta/gh-wrap"
  :commands (gh-list-prs gh-list-issues gh-my-prs gh-my-issues)
  :init
  (setq gh-default-limit 50)
  :bind (("s-G p" . gh-list-prs)
         ("s-G i" . gh-list-issues)
         ("s-G m" . gh-my-prs)
         ("s-G M" . gh-my-issues)))
```

## Workflow Examples

### Review pull requests

1. `M-x gh-list-prs` - List all open PRs
2. Navigate with `n`/`p`
3. Press `d` to see the diff
4. Press `c` to check CI status
5. Press `r` to start a review
6. Press `m` to merge when approved

### Triage issues

1. `M-x gh-list-issues` - List all issues
2. Press `s` - Filter by state (open/closed)
3. Press `a` - Filter by author
4. Press `RET` to view details
5. Press `C` to add a comment
6. Press `e` to edit issue

### Check your work

1. `M-x gh-my-prs` - See all your PRs
2. Check which are open vs merged
3. Press `c` to see if checks are passing
4. Press `o` to open in browser for detailed review

## Development

### Project Structure

```
gh-wrap/
├── README.md           # This file
├── gh.el              # Main package file
└── .gitignore         # Git ignore patterns
```

### Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

### Known Limitations

- PR/issue creation uses gh CLI's interactive mode (not pure Emacs forms)
- Review comments open external editor
- No inline diff editing
- Repository must be set via current directory or -R flag

### Roadmap

- [ ] Pure Emacs forms for PR/issue creation
- [ ] Inline diff viewing with navigation
- [ ] Repository switcher
- [ ] PR/issue templates
- [ ] Label management
- [ ] Milestone management
- [ ] Project board integration
- [ ] Org-mode integration for issue tracking

## License

MIT License - see LICENSE file for details.

## Acknowledgments

- Built for the [GitHub CLI](https://cli.github.com/) by GitHub
- Emacspeak integration inspired by T.V. Raman's Emacspeak project
- Created as an accessibility-first interface to GitHub

## See Also

- [GitHub CLI](https://cli.github.com/) - The underlying CLI tool
- [Emacspeak](https://github.com/tvraman/emacspeak) - The complete audio desktop
- [reminders-wrap](https://github.com/robertmeta/reminders-wrap) - Emacs wrapper for macOS Reminders
- [jira-wrap](https://github.com/robertmeta/jira-wrap) - Emacs wrapper for Jira CLI
- [confluence-wrap](https://github.com/robertmeta/confluence-wrap) - Emacs wrapper for Confluence CLI
