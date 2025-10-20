# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs package (`gh.el`) that provides a full-featured interface to the GitHub CLI (`gh`). It allows users to view and manage pull requests and issues from within Emacs, with enhanced accessibility support for Emacspeak users.

## Architecture

### Core Components

**Command Execution Layer** (`gh--run-command`, `gh--run-command-json` in /Users/rmelton/projects/robertmeta/gh-wrap/gh.el:81-95)
- Wraps the `gh` CLI command with error handling
- Parses JSON output from `gh` commands
- Returns Emacs Lisp data structures (alists)

**Display Layer** (`gh--display-items`, `gh--insert-item`, formatting functions in /Users/rmelton/projects/robertmeta/gh-wrap/gh.el:122-185)
- Renders PRs and issues in tabular format
- Attaches metadata to text via text properties (`gh-item` property stores full item data)
- Each line has the complete PR/issue data attached for interactive operations

**Interactive Layer** (various `gh-*-at-point` functions)
- Extracts item data from point using `get-text-property`
- Calls `gh` CLI for operations (view, diff, comment, review, merge)
- Uses interactive buffers or external editor for complex operations

**Two Major Modes**
- `gh-pr-mode` (/Users/rmelton/projects/robertmeta/gh-wrap/gh.el:464-472) for pull request lists
- `gh-issue-mode` (/Users/rmelton/projects/robertmeta/gh-wrap/gh.el:474-482) for issue lists
- Both derive from `special-mode` (read-only buffers with quit-window support)

### Buffer-Local State

Three key buffer-local variables track state:
- `gh-list-type`: Either `'pr` or `'issue`
- `gh-current-filters`: List of filter args (e.g., `("--state" "open")`)
- `gh-items-data`: Cached JSON data from last `gh` command

This enables refresh operations and filter toggling without re-parsing the UI.

### Emacspeak Integration

**Custom Line Reading** (/Users/rmelton/projects/robertmeta/gh-wrap/gh.el:103-120, 484-489)
- Text properties store `emacspeak-speak` formatted speech text
- Post-command hook intercepts navigation to speak custom line format
- Format: "pull request 301, Fix bug, by author, OPEN" (prioritizes title over visual layout)

**Advice-based Auditory Icons** (/Users/rmelton/projects/robertmeta/gh-wrap/gh.el:498-521)
- `defadvice` adds sound feedback to key operations
- Only active when Emacspeak is loaded

### Evil Mode Integration

Explicitly sets both modes to start in Emacs state (/Users/rmelton/projects/robertmeta/gh-wrap/gh.el:493-495), allowing keybindings like `n`/`p` to work without conflicts.

## Development Commands

### Byte-Compilation

Byte-compile the package to check for warnings and errors:
```bash
emacs --batch -f batch-byte-compile gh.el
```

This will create `gh.elc` and show any compilation warnings.

### Linting

Check documentation and code style with checkdoc:
```bash
emacs --batch --eval "(checkdoc-file \"gh.el\")"
```

For more comprehensive linting, use package-lint if available:
```bash
emacs --batch --eval "(progn (require 'package-lint) (package-lint-batch-and-exit))" gh.el
```

### Interactive Testing

Load the package in a running Emacs session:
```elisp
(load-file "gh.el")
```

Or evaluate the buffer: `M-x eval-buffer` while visiting gh.el

### Testing in a Clean Environment

Test with minimal configuration to avoid conflicts:
```bash
emacs -Q -l gh.el --eval "(gh-list-prs)"
```

### Dependencies

- Requires `gh` CLI installed and authenticated (`gh auth login`)
- Emacs 27.1+ (uses `lexical-binding`)
- Runtime dependencies: `json.el` (built-in)
- Optional: Emacspeak for audio features
- Optional: Evil mode for Evil integration

### Entry Points

Four main interactive commands:
- `gh-list-prs` - List all PRs
- `gh-my-prs` - List PRs by @me
- `gh-list-issues` - List all issues
- `gh-my-issues` - List issues by @me

All other commands work on items at point in these list buffers.

## Key Design Patterns

### Text Properties for Data Binding

Each line in the list buffer has a `gh-item` property containing the full alist from `gh` JSON output. This enables any command at point to access complete item data without re-parsing or re-fetching.

### Filter State Management

`gh-current-filters` accumulates filter arguments. The `gh-refresh` function re-runs the last command with current filters, preserving cursor position via `line-number-at-pos`.

### Interactive vs Programmatic Operations

Some operations (comment, review, edit) call `gh` without capturing output, relying on `gh` CLI's interactive mode or external editor integration. Others (diff, checks, view) capture and display output in Emacs buffers.

## Testing Approach

Since this is an Emacs package wrapping an external CLI:
- Test in a real Emacs session with a test repository
- Verify keybindings work in both modes
- Test with and without Emacspeak loaded
- Check Evil mode integration if available
- Verify text properties are correctly attached to lines
