# Python LSP Migration Guide

## Overview

This guide helps you migrate from the old `anaconda-mode` setup to a modern LSP-based Python development environment that supports Python 3.13.

## What Changed

### Before (anaconda-mode)
- Used `anaconda-mode` for Python IDE features
- Required older Python packages (jedi, autopep8, flake8, etc.)
- **Did not support Python 3.13**
- Configuration in `init-python.el`

### After (LSP-mode)
- Uses `lsp-mode` with `python-lsp-server` (pylsp)
- Modern Python language server with plugins
- **Full Python 3.13 support**
- Better performance and more features
- Configuration in `init-python-lsp.el`

## Migration Steps

### 1. Install the Dependencies

Run the updated installation script:

```bash
cd ~/.emacs.d
./installation/install_python_deps.sh
```

This will install:
- `python-lsp-server[all]` - Main LSP server
- `python-lsp-black` - Black formatter integration
- `python-lsp-ruff` - Ruff linter (faster than flake8)
- `pylsp-mypy` - Type checking integration
- Legacy packages for compatibility

### 2. Install Emacs Packages

In Emacs, install the required packages:

```emacs-lisp
M-x package-refresh-contents
M-x package-install RET lsp-mode RET
M-x package-install RET lsp-ui RET
```

### 3. Configuration is Already Updated

The configuration has been automatically updated:
- `init.el` now loads `init-python-lsp.el` instead of `init-python.el`
- Old configuration backed up to `init-python-old.el`

### 4. Restart Emacs

Restart Emacs completely to load the new configuration.

## New Features & Keybindings

### LSP Features
- **Real-time error checking** - See errors as you type
- **Auto-completion** - Intelligent code completion
- **Go to definition** - `M-.` or `C-c l g d`
- **Find references** - `M-?` or `C-c l g r`
- **Rename symbols** - `C-c l r r`
- **Documentation** - `C-c C-d` or `C-c l h h`
- **Code actions** - `C-c l a a`
- **Format document** - `C-c l f f`

### LSP UI Features
- **Hover documentation** - Automatic doc popups
- **Sideline diagnostics** - Errors/warnings in margin
- **Peek definitions** - Inline definition preview
- **Breadcrumb navigation** - Shows current scope

## Troubleshooting

### LSP Server Not Starting

If you see "LSP server not found" errors:

1. Check if pylsp is installed:
   ```bash
   ~/.emacs.d/python-venv/bin/pylsp --version
   ```

2. If not installed, run the installation script again:
   ```bash
   ./installation/install_python_deps.sh
   ```

### Python Version Issues

The new setup automatically detects your Python 3.13 environment in `~/.emacs.d/python-venv/`.

### Performance Issues

If LSP feels slow, you can adjust settings in `init-python-lsp.el`:

```emacs-lisp
(setq lsp-idle-delay 1.0)  ; Increase delay
(setq lsp-log-io nil)      ; Disable logging
```

## Reverting to Old Setup

If you need to revert to the old anaconda-mode setup:

1. Edit `init.el`:
   ```emacs-lisp
   ;; Comment out:
   ;; (require 'init-python-lsp)
   
   ;; Uncomment:
   (require 'init-python)
   ```

2. Restart Emacs

## Benefits of the New Setup

1. **Python 3.13 Support** - Works with the latest Python
2. **Better Performance** - LSP is generally faster than anaconda-mode
3. **More Features** - Advanced refactoring, type checking, etc.
4. **Active Development** - LSP ecosystem is actively maintained
5. **Consistent Experience** - Same LSP protocol used across editors
6. **Better Error Reporting** - More detailed error messages

## Getting Help

- **LSP Mode Documentation**: https://emacs-lsp.github.io/lsp-mode/
- **Python LSP Server**: https://github.com/python-lsp/python-lsp-server
- **LSP UI**: https://emacs-lsp.github.io/lsp-ui/

## Quick Reference

| Action | Old (anaconda-mode) | New (LSP) |
|--------|-------------------|-----------|
| Go to definition | `M-.` | `M-.` or `C-c l g d` |
| Find references | `M-?` | `M-?` or `C-c l g r` |
| Documentation | `C-c C-d` | `C-c C-d` or `C-c l h h` |
| Rename | Not available | `C-c l r r` |
| Format code | py-autopep8 | `C-c l f f` |
| Show errors | flycheck | LSP + flycheck |

The migration preserves most of your familiar keybindings while adding new LSP-specific ones. 