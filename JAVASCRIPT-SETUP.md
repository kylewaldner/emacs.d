# Modern JavaScript Development Setup for Emacs

This document describes the updated JavaScript/TypeScript configuration that replaces the old setup with modern best practices.

## What's New

### Major Changes from Old Configuration

**Replaced deprecated packages:**
- ❌ `tern` → ✅ `lsp-mode` with TypeScript Language Server
- ❌ `xref-js2` → ✅ Built-in LSP navigation
- ❌ `js-comint` → ✅ `nodejs-repl` (kept, but enhanced)
- ❌ `coffee-mode` → ✅ Removed (CoffeeScript is deprecated)

**Added modern features:**
- **Tree-sitter support** (Emacs 29+) for superior syntax highlighting
- **LSP integration** for IDE-like features
- **Modern formatting** with `apheleia` + Prettier
- **Enhanced completion** with Company + LSP
- **Better project management** with updated Projectile integration
- **TypeScript support** with proper TSX handling

## Quick Start

### 1. Run the Setup Script

```bash
cd ~/.emacs.d
./install.sh javascript
```

This script will:
- Install required npm packages globally (TypeScript Language Server, Prettier, ESLint)
- Set up development tools
- **Note**: Does NOT create project files in your Emacs directory

### 2. Restart Emacs

```bash
# If Emacs is running, restart it to load the new configuration
pkill emacs && emacs &
```

### 3. Install Tree-sitter Grammars (Emacs 29+)

In Emacs, run:
```
M-x javascript-install-tree-sitter-grammars
```

### 4. Create a Sample Project (Optional)

To create a new JavaScript/TypeScript project with all configuration files:

```bash
cd ~/.emacs.d
./installation/create-javascript-project.sh my-awesome-project
cd my-awesome-project
npm install
```

This creates a complete project with:
- `package.json` with useful scripts
- `tsconfig.json` for TypeScript
- `.prettierrc` and `.eslintrc.js` for formatting/linting
- Sample `src/index.ts` file
- `.gitignore` and `README.md`

## Required Dependencies

### Core Tools (installed by setup script)
- **Node.js** (v16+)
- **TypeScript Language Server** (`typescript-language-server`)
- **Prettier** (code formatting)
- **ESLint** (linting)

### Emacs Packages (auto-installed)
- `lsp-mode` - Language Server Protocol client
- `lsp-ui` - Enhanced LSP interface
- `company` - Completion framework
- `flycheck` - Syntax checking
- `apheleia` - Code formatting (modern alternative to `prettier-js`)
- `web-mode` - Web development support
- `nodejs-repl` - Node.js REPL integration

## Features

### Language Server Protocol (LSP)
- **Go to definition** (`M-.`)
- **Find references** (`M-?`)
- **Show documentation** (`C-c C-d`)
- **Rename symbol** (`C-c C-r`)
- **Code actions** (via LSP UI)
- **Error checking** with TypeScript diagnostics

### Tree-sitter Support (Emacs 29+)
- **Superior syntax highlighting**
- **Structural editing** with `combobulate`
- **Better indentation**
- **Semantic understanding** of code structure

### File Support
- `.js` - JavaScript (tree-sitter: `js-ts-mode`, fallback: `js2-mode`)
- `.mjs`, `.cjs` - ES modules and CommonJS
- `.jsx` - React JSX (`rjsx-mode`)
- `.ts` - TypeScript (tree-sitter: `typescript-ts-mode`, fallback: `typescript-mode`)
- `.tsx` - TypeScript JSX (tree-sitter: `tsx-ts-mode`, fallback: `rjsx-mode`)
- `.json` - JSON (tree-sitter: `json-ts-mode`, fallback: `json-mode`)
- `.vue` - Vue.js (`web-mode`)
- `.svelte` - Svelte (`web-mode`)

### Code Formatting
- **Automatic formatting** on save with Prettier
- **Consistent style** across all JavaScript/TypeScript files
- **Configurable** via `.prettierrc`

### Project Integration
- **npm/yarn project detection**
- **Automatic node_modules/.bin PATH setup**
- **Project-specific linting and formatting**

## Key Bindings

### LSP Commands (prefix: `C-c l`)
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c l g g` | `lsp-find-definition` | Go to definition |
| `C-c l g r` | `lsp-find-references` | Find references |
| `C-c l h h` | `lsp-describe-thing-at-point` | Show documentation |
| `C-c l r r` | `lsp-rename` | Rename symbol |
| `C-c l a a` | `lsp-execute-code-action` | Execute code action |
| `C-c l = =` | `lsp-format-buffer` | Format buffer |

### Navigation
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `M-.` | `lsp-find-definition` | Go to definition |
| `M-?` | `lsp-find-references` | Find references |
| `M-,` | `xref-go-back` | Go back |

### Documentation
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c d` | `js-doc-insert-function-doc` | Insert JSDoc comment |
| `C-c @` | `js-doc-insert-tag` | Insert JSDoc tag |
| `C-c C-d` | `lsp-describe-thing-at-point` | Show hover info |

### REPL Integration
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-x C-e` | `nodejs-repl-send-last-expression` | Evaluate expression |
| `C-c C-j` | `nodejs-repl-send-line` | Send line to REPL |
| `C-c C-r` | `nodejs-repl-send-region` | Send region to REPL |
| `C-c C-c` | `nodejs-repl-send-buffer` | Send buffer to REPL |
| `C-c C-z` | `nodejs-repl-switch-to-repl` | Switch to REPL |

## Creating Projects

### Using the Project Template Script

```bash
cd ~/.emacs.d
./installation/create-javascript-project.sh my-project-name
cd my-project-name
npm install
```

### Manual Project Setup

```bash
mkdir my-project && cd my-project
npm init -y
npm install --save-dev typescript @types/node
npm install --save-dev prettier eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin
```

## Customization

### Disable Tree-sitter (use traditional modes)
```elisp
;; In your init-local.el or custom configuration
(setq treesit-available-p nil)
```

### Custom LSP Settings
```elisp
;; Customize LSP behavior
(after-load 'lsp-mode
  (setq lsp-idle-delay 0.1  ; Faster response
        lsp-headerline-breadcrumb-enable nil))  ; Disable breadcrumbs
```

### Custom Formatting Settings
```elisp
;; Disable automatic formatting
(remove-hook 'js-ts-mode-hook 'apheleia-mode)

;; Or customize prettier settings
(after-load 'apheleia
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--tab-width=4" "--stdin-filepath" filepath)))
```

## Troubleshooting

### LSP Not Working
1. Verify TypeScript Language Server is installed:
   ```bash
   typescript-language-server --version
   ```
2. Check if LSP is running: `M-x lsp-doctor`
3. Restart LSP: `M-x lsp-restart-workspace`

### Tree-sitter Issues
1. Check Emacs version: `M-x emacs-version` (requires 29+)
2. Install grammars: `M-x javascript-install-tree-sitter-grammars`
3. Check grammar status: `M-x treesit-language-available-p`

### Formatting Not Working
1. Verify Prettier is installed: `prettier --version`
2. Check if Apheleia is active: `M-x apheleia-mode`
3. Format manually: `M-x apheleia-format-buffer`

### Performance Issues
```elisp
;; Add to your configuration for better performance
(setq lsp-enable-file-watchers nil
      lsp-file-watch-threshold 5000
      lsp-idle-delay 0.5)
```

## Learning Resources

### LSP Features
- [LSP Mode Documentation](https://emacs-lsp.github.io/lsp-mode/)
- [TypeScript Language Server](https://github.com/typescript-language-server/typescript-language-server)

### Tree-sitter
- [Tree-sitter for Emacs](https://tree-sitter.github.io/tree-sitter/)
- [Combobulate Documentation](https://github.com/mickeynp/combobulate)

### JavaScript/TypeScript Best Practices
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
- [ESLint Rules](https://eslint.org/docs/rules/)
- [Prettier Configuration](https://prettier.io/docs/en/configuration.html)

## Migration from Old Setup

### Removed Features
- **Tern support** - Replaced with LSP
- **CoffeeScript support** - Removed (deprecated)
- **js2-mode exclusive features** - Now optional fallback

### Equivalent Commands
| Old Command | New Command | Notes |
|-------------|-------------|-------|
| `tern-find-definition` | `lsp-find-definition` | More accurate with LSP |
| `xref-js2-xref-backend` | Built-in LSP xref | Automatic with LSP |
| `js2-mode-show-node` | `lsp-describe-thing-at-point` | Enhanced with type info |

### Configuration Updates
If you had custom `js2-mode` or `tern` configurations, you may need to adapt them for LSP. Most functionality is now handled automatically by the Language Server Protocol.

---

## Tips for Productive Development

1. **Use projectile** - `C-c p f` to quickly find files in your project
2. **Leverage LSP** - `C-c l` to access all LSP features
3. **Format on save** - Code is automatically formatted with Prettier
4. **Use the REPL** - `C-c C-z` for interactive development
5. **Check diagnostics** - Flycheck shows errors and warnings inline
6. **Tree-sitter modes** - Much faster and more accurate than traditional modes

Enjoy your modern JavaScript development environment!