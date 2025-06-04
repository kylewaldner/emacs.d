#!/bin/bash

# Modern JavaScript Development Environment Setup for Emacs
# This script installs the tools needed for the updated init-javascript.el configuration

set -e

echo "🚀 Setting up modern JavaScript development environment for Emacs"
echo "================================================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_command() {
    if command -v "$1" >/dev/null 2>&1; then
        print_success "$1 is already installed"
        return 0
    else
        return 1
    fi
}

# Check if Node.js is installed
print_status "Checking Node.js installation..."
if check_command "node"; then
    NODE_VERSION=$(node --version)
    print_success "Node.js version: $NODE_VERSION"
else
    print_error "Node.js is not installed. Please install Node.js first:"
    echo "  - Visit: https://nodejs.org/"
    echo "  - Or use a version manager like nvm, fnm, or volta"
    exit 1
fi

# Check if npm is installed
print_status "Checking npm installation..."
if check_command "npm"; then
    NPM_VERSION=$(npm --version)
    print_success "npm version: $NPM_VERSION"
else
    print_error "npm is not installed. It should come with Node.js."
    exit 1
fi

# Install global npm packages for development
print_status "Installing global npm packages..."

GLOBAL_PACKAGES=(
    "typescript"                    # TypeScript compiler
    "typescript-language-server"    # LSP server for TypeScript/JavaScript
    "@vscode/vscode-langservers-extracted"  # Additional language servers
    "prettier"                      # Code formatter
    "eslint"                       # Linter
    "@typescript-eslint/eslint-plugin"  # TypeScript ESLint rules
    "@typescript-eslint/parser"     # TypeScript parser for ESLint
    "npm-check-updates"            # Update package.json dependencies
)

for package in "${GLOBAL_PACKAGES[@]}"; do
    print_status "Installing $package..."
    if npm list -g "$package" >/dev/null 2>&1; then
        print_warning "$package is already installed globally"
    else
        npm install -g "$package"
        print_success "Installed $package"
    fi
done

# Verify TypeScript Language Server
print_status "Verifying TypeScript Language Server..."
if check_command "typescript-language-server"; then
    TSLS_VERSION=$(typescript-language-server --version 2>/dev/null || echo "unknown")
    print_success "TypeScript Language Server version: $TSLS_VERSION"
else
    print_error "TypeScript Language Server installation failed"
fi

# Check for optional tools
print_status "Checking for optional development tools..."

# Check for yarn
if check_command "yarn"; then
    YARN_VERSION=$(yarn --version)
    print_success "Yarn version: $YARN_VERSION"
else
    print_warning "Yarn is not installed. You can install it with: npm install -g yarn"
fi

# Check for pnpm
if check_command "pnpm"; then
    PNPM_VERSION=$(pnpm --version)
    print_success "pnpm version: $PNPM_VERSION"
else
    print_warning "pnpm is not installed. You can install it with: npm install -g pnpm"
fi

# Print emacs configuration instructions
echo ""
echo "🎉 JavaScript development environment setup complete!"
echo "=================================================="
echo ""
echo "📝 Next steps for Emacs:"
echo "1. Restart Emacs to reload the updated init-javascript.el configuration"
echo "2. Open a JavaScript/TypeScript file to test the setup"
echo "3. Use 'M-x javascript-install-tree-sitter-grammars' to install tree-sitter grammars"
echo "4. Use 'C-c l' for LSP commands when in a JS/TS file"
echo ""
echo "🔧 Create a new JavaScript/TypeScript project:"
echo "- mkdir my-project && cd my-project"
echo "- npm init -y"
echo "- npm install --save-dev typescript @types/node"
echo "- npm install --save-dev prettier eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin"
echo ""
echo "📚 Emacs key bindings (in JS/TS files):"
echo "- C-c l       # LSP command prefix"
echo "- M-.         # Go to definition"
echo "- M-?         # Find references"
echo "- C-c C-d     # Show documentation"
echo "- C-c C-r     # Rename symbol"
echo "- C-c d       # Insert JSDoc comment"
echo "- C-c C-z     # Switch to Node.js REPL"
echo ""
echo "🌳 Tree-sitter support:"
if command -v emacs >/dev/null 2>&1; then
    EMACS_VERSION=$(emacs --version | head -n1 | cut -d' ' -f3)
    print_success "Emacs version: $EMACS_VERSION"
    if [[ "$EMACS_VERSION" > "29" ]] || [[ "$EMACS_VERSION" == "29"* ]]; then
        print_success "Tree-sitter support is available!"
        echo "  Use 'M-x javascript-install-tree-sitter-grammars' in Emacs"
    else
        print_warning "Tree-sitter requires Emacs 29+. You have $EMACS_VERSION"
        echo "  Traditional modes (js2-mode, typescript-mode) will be used"
    fi
else
    print_warning "Emacs not found in PATH"
fi

echo ""
print_success "Setup complete! Happy coding! 🚀" 