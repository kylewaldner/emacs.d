#!/bin/bash

# install_go_deps.sh - Install essential Go development tools
# Usage: ./install_go_deps.sh

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if Go is installed
check_go_installation() {
    log_info "Checking Go installation..."
    
    if ! command -v go &> /dev/null; then
        log_error "Go is not installed or not in PATH"
        log_info "Please install Go from https://golang.org/dl/"
        exit 1
    fi
    
    local go_version=$(go version)
    log_success "Found Go: $go_version"
    
    # Check GOPATH and GOBIN
    local gopath=$(go env GOPATH)
    local gobin=$(go env GOBIN)
    
    log_info "GOPATH: $gopath"
    if [[ -n "$gobin" ]]; then
        log_info "GOBIN: $gobin"
    else
        log_info "GOBIN: $gopath/bin (default)"
        gobin="$gopath/bin"
    fi
    
    # Check if GOBIN is in PATH
    if [[ ":$PATH:" != *":$gobin:"* ]]; then
        log_warning "GOBIN ($gobin) is not in your PATH"
        log_info "Automatically adding to PATH..."
        
        # Detect shell and add to appropriate profile
        local shell_profile=""
        if [[ -n "$BASH_VERSION" ]] || [[ "$SHELL" == *"bash"* ]]; then
            shell_profile="$HOME/.bashrc"
        elif [[ -n "$ZSH_VERSION" ]] || [[ "$SHELL" == *"zsh"* ]]; then
            shell_profile="$HOME/.zshrc"
        elif [[ "$SHELL" == *"fish"* ]]; then
            # Fish uses a different syntax
            log_info "Fish shell detected, adding to fish config..."
            mkdir -p "$HOME/.config/fish"
            echo "set -gx PATH \$PATH $gobin" >> "$HOME/.config/fish/config.fish"
            log_success "Added to Fish config: $HOME/.config/fish/config.fish"
            log_info "Restart your shell or run: source ~/.config/fish/config.fish"
            return 0
        else
            # Default to .profile for unknown shells
            shell_profile="$HOME/.profile"
        fi
        
        # Check if the PATH export already exists
        local path_export="export PATH=\"\$PATH:$gobin\""
        if [[ -f "$shell_profile" ]] && grep -q "PATH.*$gobin" "$shell_profile"; then
            log_info "Go bin directory already configured in $shell_profile"
        else
            # Add to shell profile
            echo "" >> "$shell_profile"
            echo "# Go development tools" >> "$shell_profile"
            echo "$path_export" >> "$shell_profile"
            log_success "Added Go bin to PATH in: $shell_profile"
            log_info "Restart your shell or run: source $shell_profile"
        fi
        
        # Also export for current session
        export PATH="$PATH:$gobin"
        log_success "Go bin directory added to current session PATH"
    else
        log_success "Go bin directory is already in PATH"
    fi
}

# Install a Go tool
install_go_tool() {
    local tool_package="$1"
    local tool_name="$2"
    
    log_info "Installing $tool_name..."
    
    if go install "$tool_package" 2>/dev/null; then
        log_success "Successfully installed $tool_name"
        return 0
    else
        log_error "Failed to install $tool_name"
        return 1
    fi
}

# Verify tool installation
verify_tool() {
    local tool_name="$1"
    local expected_path="$2"
    
    if command -v "$tool_name" &> /dev/null; then
        local tool_path=$(which "$tool_name")
        log_success "$tool_name is available at: $tool_path"
        return 0
    else
        log_warning "$tool_name not found in PATH"
        if [[ -f "$expected_path" ]]; then
            log_info "Tool exists at: $expected_path"
            log_warning "Make sure $(dirname "$expected_path") is in your PATH"
        fi
        return 1
    fi
}

# Main installation function
install_go_tools() {
    log_info "Starting Go tools installation..."
    echo "======================================="
    
    # Get Go paths
    local gopath=$(go env GOPATH)
    local gobin=$(go env GOBIN)
    if [[ -z "$gobin" ]]; then
        gobin="$gopath/bin"
    fi
    
    # List of essential Go tools
    declare -A tools=(
        ["golang.org/x/tools/gopls@latest"]="gopls"
        ["github.com/go-delve/delve/cmd/dlv@latest"]="dlv"
        ["golang.org/x/tools/cmd/goimports@latest"]="goimports"
        ["mvdan.cc/gofumpt@latest"]="gofumpt"
        ["github.com/golangci/golangci-lint/cmd/golangci-lint@latest"]="golangci-lint"
        ["github.com/josharian/impl@latest"]="impl"
        ["honnef.co/go/tools/cmd/staticcheck@latest"]="staticcheck"
        ["github.com/fatih/gomodifytags@latest"]="gomodifytags"
        ["github.com/cweill/gotests/gotests@latest"]="gotests"
    )
    
    local failed_tools=()
    local success_count=0
    
    # Install each tool
    for package in "${!tools[@]}"; do
        tool_name="${tools[$package]}"
        if install_go_tool "$package" "$tool_name"; then
            ((success_count++))
        else
            failed_tools+=("$tool_name")
        fi
        echo ""
    done
    
    echo "======================================="
    log_info "Installation Summary"
    log_success "Successfully installed $success_count tools"
    
    if [[ ${#failed_tools[@]} -gt 0 ]]; then
        log_warning "Failed to install: ${failed_tools[*]}"
    fi
    
    echo ""
    log_info "Verifying installations..."
    echo "======================================="
    
    # Verify installations
    local verification_failed=()
    for package in "${!tools[@]}"; do
        tool_name="${tools[$package]}"
        expected_path="$gobin/$tool_name"
        if ! verify_tool "$tool_name" "$expected_path"; then
            verification_failed+=("$tool_name")
        fi
    done
    
    echo ""
    echo "======================================="
    
    if [[ ${#verification_failed[@]} -eq 0 ]]; then
        log_success "All tools are properly installed and accessible!"
    else
        log_warning "Some tools are not accessible in PATH: ${verification_failed[*]}"
        echo ""
        log_info "To fix PATH issues, add this to your shell profile:"
        echo "export PATH=\"\$PATH:$gobin\""
        echo ""
        log_info "Then restart your shell or run: source ~/.bashrc (or ~/.zshrc)"
    fi
}

# Update Go modules in current directory (if applicable)
update_go_modules() {
    if [[ -f "go.mod" ]]; then
        log_info "Found go.mod, updating modules..."
        if go mod tidy; then
            log_success "Successfully updated Go modules"
        else
            log_warning "Failed to update Go modules"
        fi
    fi
}

# Show tool descriptions
show_tool_info() {
    log_info "Go Development Tools Information"
    echo "======================================="
    cat << 'EOF'
gopls           - Official Go language server (LSP)
dlv (delve)     - Go debugger
goimports       - Auto-format Go code and manage imports
gofumpt         - Stricter gofmt alternative
golangci-lint   - Fast Go linters runner
impl            - Generate method stubs for interfaces
staticcheck     - Advanced Go static analysis
gomodifytags    - Modify struct tags
gotests         - Generate Go tests
EOF
    echo "======================================="
}

# Main execution
main() {
    echo "Go Development Tools Installer"
    echo "=============================="
    echo ""
    
    # Parse command line arguments
    case "${1:-install}" in
        "info"|"--info")
            show_tool_info
            exit 0
            ;;
        "check"|"--check")
            check_go_installation
            exit 0
            ;;
        "install"|"--install"|"")
            check_go_installation
            echo ""
            install_go_tools
            echo ""
            update_go_modules
            ;;
        "help"|"--help"|"-h")
            echo "Usage: $0 [command]"
            echo ""
            echo "Commands:"
            echo "  install (default)  Install all Go development tools"
            echo "  check             Check Go installation and PATH"
            echo "  info              Show information about tools"
            echo "  help              Show this help message"
            exit 0
            ;;
        *)
            log_error "Unknown command: $1"
            echo "Run '$0 help' for usage information"
            exit 1
            ;;
    esac
    
    echo ""
    log_info "Installation complete!"
    log_info "Restart Emacs to use the new tools"
}

# Run main function with all arguments
main "$@" 