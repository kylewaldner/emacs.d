#!/bin/bash

# install_protobuf_deps.sh - Install protobuf language servers and tools
# Usage: ./install_protobuf_deps.sh [options]
# Options:
#   --server <name>    Install specific server (protobuf-language-server, pbls, buf)
#   --all              Install all available servers (default)
#   --help             Show this help message

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
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

log_header() {
    echo -e "${CYAN}$1${NC}"
}

# Show usage information
show_help() {
    log_header "Protobuf Language Server Installation Script"
    echo ""
    echo "Usage: $0 [options]"
    echo ""
    echo "Options:"
    echo "  --server <name>    Install specific server:"
    echo "                     - protobuf-language-server (Go-based, most features)"
    echo "                     - pbls (Rust-based, fast and lightweight)"
    echo "                     - buf (Official Buf CLI with experimental LSP)"
    echo "  --all              Install all available servers (default)"
    echo "  --help             Show this help message"
    echo ""
    echo "Supported distributions:"
    echo "  - Manjaro, Arch, EndeavourOS, ArcoLinux"
    echo "  - Ubuntu, Debian, Linux Mint, Elementary OS"
    echo "  - Fedora, CentOS, RHEL, Rocky Linux"
    echo "  - openSUSE"
    echo ""
}

# Function to detect distribution
detect_distro() {
    if [[ -f /etc/os-release ]]; then
        source /etc/os-release
        DISTRO=$ID
        VERSION=${VERSION_ID:-"unknown"}
        DISTRO_NAME=${NAME:-$ID}
    elif [[ -f /etc/redhat-release ]]; then
        DISTRO="rhel"
        DISTRO_NAME="Red Hat Enterprise Linux"
    elif [[ -f /etc/debian_version ]]; then
        DISTRO="debian"
        DISTRO_NAME="Debian"
    else
        DISTRO="unknown"
        DISTRO_NAME="Unknown"
    fi

    log_info "Detected distribution: $DISTRO_NAME ($DISTRO)"
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to install system packages based on distribution
install_system_packages() {
    local packages=("$@")

    log_info "Installing system packages: ${packages[*]}"

    case $DISTRO in
        "manjaro"|"arch"|"endeavouros"|"artix"|"arcolinux")
            log_info "Using pacman package manager"
            if command_exists pacman; then
                sudo pacman -Sy --noconfirm --needed "${packages[@]}"
            else
                log_error "pacman not found"
                return 1
            fi
            ;;
        "ubuntu"|"debian"|"linuxmint"|"mint"|"elementary"|"zorin"|"kali"|"parrot"|"pop")
            log_info "Using apt package manager"
            if command_exists apt; then
                sudo apt update
                sudo apt install -y "${packages[@]}"
            elif command_exists apt-get; then
                sudo apt-get update
                sudo apt-get install -y "${packages[@]}"
            else
                log_error "apt/apt-get not found"
                return 1
            fi
            ;;
        "fedora"|"centos"|"rhel"|"rocky"|"almalinux"|"ol")
            log_info "Using dnf/yum package manager"
            if command_exists dnf; then
                sudo dnf install -y "${packages[@]}"
            elif command_exists yum; then
                sudo yum install -y "${packages[@]}"
            else
                log_error "dnf/yum not found"
                return 1
            fi
            ;;
        "opensuse"|"suse"|"opensuse-leap"|"opensuse-tumbleweed")
            log_info "Using zypper package manager"
            if command_exists zypper; then
                sudo zypper install -y "${packages[@]}"
            else
                log_error "zypper not found"
                return 1
            fi
            ;;
        *)
            log_warning "Unknown distribution: $DISTRO"
            log_warning "Please install these packages manually: ${packages[*]}"
            return 1
            ;;
    esac
}

# Check and install Go if needed
setup_go() {
    log_info "Checking Go installation..."

    if command_exists go; then
        local go_version=$(go version | cut -d' ' -f3)
        log_success "Found Go: $go_version"
        return 0
    fi

    log_warning "Go not found, installing..."

    # Install Go via system package manager
    case $DISTRO in
        "manjaro"|"arch"|"endeavouros"|"artix"|"arcolinux")
            install_system_packages "go"
            ;;
        "ubuntu"|"debian"|"linuxmint"|"mint"|"elementary"|"zorin"|"kali"|"parrot"|"pop")
            install_system_packages "golang-go"
            ;;
        "fedora"|"centos"|"rhel"|"rocky"|"almalinux"|"ol")
            install_system_packages "golang"
            ;;
        "opensuse"|"suse"|"opensuse-leap"|"opensuse-tumbleweed")
            install_system_packages "go"
            ;;
        *)
            log_error "Cannot install Go automatically for $DISTRO"
            log_info "Please install Go manually from https://golang.org/dl/"
            return 1
            ;;
    esac

    # Verify installation
    if command_exists go; then
        local go_version=$(go version | cut -d' ' -f3)
        log_success "Successfully installed Go: $go_version"

        # Setup Go environment
        setup_go_environment
    else
        log_error "Go installation failed"
        return 1
    fi
}

# Setup Go environment
setup_go_environment() {
    local gopath=$(go env GOPATH)
    local gobin=$(go env GOBIN)

    if [[ -z "$gobin" ]]; then
        gobin="$gopath/bin"
    fi

    log_info "GOPATH: $gopath"
    log_info "GOBIN: $gobin"

    # Check if GOBIN is in PATH
    if [[ ":$PATH:" != *":$gobin:"* ]]; then
        log_warning "GOBIN ($gobin) is not in your PATH"
        add_to_path "$gobin" "Go development tools"
    else
        log_success "Go bin directory is already in PATH"
    fi
}

# Check and install Rust if needed
setup_rust() {
    log_info "Checking Rust installation..."

    if command_exists cargo; then
        local rust_version=$(rustc --version | cut -d' ' -f2)
        log_success "Found Rust: $rust_version"
        return 0
    fi

    log_warning "Rust not found, installing..."

    # Try system package manager first
    case $DISTRO in
        "manjaro"|"arch"|"endeavouros"|"artix"|"arcolinux")
            install_system_packages "rust"
            ;;
        "ubuntu"|"debian"|"linuxmint"|"mint"|"elementary"|"zorin"|"kali"|"parrot"|"pop")
            install_system_packages "rustc" "cargo"
            ;;
        "fedora"|"centos"|"rhel"|"rocky"|"almalinux"|"ol")
            install_system_packages "rust" "cargo"
            ;;
        "opensuse"|"suse"|"opensuse-leap"|"opensuse-tumbleweed")
            install_system_packages "rust"
            ;;
        *)
            # Fallback to rustup installation
            log_info "Installing Rust via rustup..."
            curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
            source "$HOME/.cargo/env"
            ;;
    esac

    # Verify installation
    if command_exists cargo; then
        local rust_version=$(rustc --version | cut -d' ' -f2)
        log_success "Successfully installed Rust: $rust_version"

        # Setup Rust environment
        setup_rust_environment
    else
        log_error "Rust installation failed"
        return 1
    fi
}

# Setup Rust environment
setup_rust_environment() {
    local cargo_bin="$HOME/.cargo/bin"

    if [[ ":$PATH:" != *":$cargo_bin:"* ]]; then
        log_warning "Cargo bin directory ($cargo_bin) is not in your PATH"
        add_to_path "$cargo_bin" "Rust development tools"
    else
        log_success "Cargo bin directory is already in PATH"
    fi
}

# Add directory to PATH
add_to_path() {
    local bin_dir="$1"
    local description="$2"

    log_info "Adding $description to PATH..."

    # Detect shell and add to appropriate profile
    local shell_profile=""
    if [[ -n "${BASH_VERSION:-}" ]] || [[ "${SHELL:-}" == *"bash"* ]]; then
        shell_profile="$HOME/.bashrc"
    elif [[ -n "${ZSH_VERSION:-}" ]] || [[ "${SHELL:-}" == *"zsh"* ]]; then
        shell_profile="$HOME/.zshrc"
    elif [[ "${SHELL:-}" == *"fish"* ]]; then
        # Fish uses a different syntax
        log_info "Fish shell detected, adding to fish config..."
        mkdir -p "$HOME/.config/fish"
        echo "set -gx PATH \$PATH $bin_dir" >> "$HOME/.config/fish/config.fish"
        log_success "Added to Fish config: $HOME/.config/fish/config.fish"
        return 0
    else
        # Default to .profile for unknown shells
        shell_profile="$HOME/.profile"
    fi

    # Check if the PATH export already exists
    local path_export="export PATH=\"\$PATH:$bin_dir\""
    if [[ -f "$shell_profile" ]] && grep -q "PATH.*$bin_dir" "$shell_profile"; then
        log_info "$description already configured in $shell_profile"
    else
        # Add to shell profile
        echo "" >> "$shell_profile"
        echo "# $description" >> "$shell_profile"
        echo "$path_export" >> "$shell_profile"
        log_success "Added to PATH in: $shell_profile"
    fi

    # Also export for current session
    export PATH="$PATH:$bin_dir"
    log_success "$description added to current session PATH"
}

# Install protobuf-language-server (Go-based)
install_protobuf_language_server() {
    log_header "Installing protobuf-language-server..."

    if ! command_exists go; then
        log_error "Go not found. Installing Go first..."
        setup_go || return 1
    fi

    log_info "Installing protobuf-language-server via go install..."
    if go install github.com/lasorda/protobuf-language-server@master; then
        log_success "Successfully installed protobuf-language-server"

        # Verify installation
        if command_exists protobuf-language-server; then
            log_success "protobuf-language-server is available in PATH"
        else
            log_warning "protobuf-language-server installed but not in PATH"
            log_info "You may need to restart your shell or run: source ~/.bashrc"
        fi
        return 0
    else
        log_error "Failed to install protobuf-language-server"
        return 1
    fi
}

# Install pbls (Rust-based)
install_pbls() {
    log_header "Installing pbls (Protobuf Language Server)..."

    if ! command_exists cargo; then
        log_error "Cargo not found. Installing Rust first..."
        setup_rust || return 1
    fi

    # Install protoc first (required by pbls)
    log_info "Installing protoc (Protocol Buffers compiler)..."
    case $DISTRO in
        "manjaro"|"arch"|"endeavouros"|"artix"|"arcolinux")
            install_system_packages "protobuf"
            ;;
        "ubuntu"|"debian"|"linuxmint"|"mint"|"elementary"|"zorin"|"kali"|"parrot"|"pop")
            install_system_packages "protobuf-compiler"
            ;;
        "fedora"|"centos"|"rhel"|"rocky"|"almalinux"|"ol")
            install_system_packages "protobuf-compiler"
            ;;
        "opensuse"|"suse"|"opensuse-leap"|"opensuse-tumbleweed")
            install_system_packages "protobuf-devel"
            ;;
        *)
            log_warning "Cannot install protoc automatically for $DISTRO"
            log_info "Please install Protocol Buffers compiler manually"
            ;;
    esac

    log_info "Installing pbls via cargo install..."
    if cargo install --git https://github.com/rcorre/pbls; then
        log_success "Successfully installed pbls"

        # Verify installation
        if command_exists pbls; then
            log_success "pbls is available in PATH"
        else
            log_warning "pbls installed but not in PATH"
            log_info "You may need to restart your shell or run: source ~/.bashrc"
        fi
        return 0
    else
        log_error "Failed to install pbls"
        return 1
    fi
}

# Install buf CLI
install_buf() {
    log_header "Installing Buf CLI..."

    # Check if buf is available via package manager
    case $DISTRO in
        "manjaro"|"arch"|"endeavouros"|"artix"|"arcolinux")
            # Buf is available in AUR
            if command_exists yay; then
                log_info "Installing buf via yay (AUR)..."
                yay -S --noconfirm buf
            elif command_exists paru; then
                log_info "Installing buf via paru (AUR)..."
                paru -S --noconfirm buf
            else
                log_warning "No AUR helper found, falling back to manual installation"
                install_buf_manual
            fi
            ;;
        *)
            # For other distributions, use manual installation
            install_buf_manual
            ;;
    esac

    # Verify installation
    if command_exists buf; then
        local buf_version=$(buf --version)
        log_success "Successfully installed buf: $buf_version"

        # Check for LSP support (experimental)
        if buf --help | grep -q "beta"; then
            log_info "Buf CLI supports beta features (may include LSP)"
        else
            log_warning "This version of buf may not support LSP yet"
        fi
        return 0
    else
        log_error "Failed to install buf"
        return 1
    fi
}

# Install buf manually
install_buf_manual() {
    log_info "Installing buf via direct download..."

    # Detect architecture
    local arch=$(uname -m)
    local os="linux"

    case $arch in
        x86_64) arch="x86_64" ;;
        aarch64|arm64) arch="aarch_64" ;;
        armv7l) arch="armv7" ;;
        *)
            log_error "Unsupported architecture: $arch"
            return 1
            ;;
    esac

    # Download and install
    local download_url="https://github.com/bufbuild/buf/releases/latest/download/buf-${os}-${arch}"
    local install_dir="$HOME/.local/bin"

    mkdir -p "$install_dir"

    log_info "Downloading buf from $download_url"
    if curl -sSL "$download_url" -o "$install_dir/buf"; then
        chmod +x "$install_dir/buf"
        log_success "Downloaded buf to $install_dir/buf"

        # Add to PATH if needed
        if [[ ":$PATH:" != *":$install_dir:"* ]]; then
            add_to_path "$install_dir" "Local binaries"
        fi
    else
        log_error "Failed to download buf"
        return 1
    fi
}

# Verify language server installations
verify_installations() {
    log_header "Verifying installations..."

    local servers_found=()
    local servers_missing=()

    # Check protobuf-language-server
    if command_exists protobuf-language-server; then
        servers_found+=("protobuf-language-server")
        log_success "✓ protobuf-language-server: $(which protobuf-language-server)"
    else
        servers_missing+=("protobuf-language-server")
    fi

    # Check pbls
    if command_exists pbls; then
        servers_found+=("pbls")
        log_success "✓ pbls: $(which pbls)"
    else
        servers_missing+=("pbls")
    fi

    # Check buf
    if command_exists buf; then
        servers_found+=("buf")
        local buf_version=$(buf --version 2>/dev/null || echo "unknown version")
        log_success "✓ buf: $(which buf) ($buf_version)"
    else
        servers_missing+=("buf")
    fi

    echo ""
    log_info "Installation Summary:"

    if [[ ${#servers_found[@]} -gt 0 ]]; then
        log_success "Installed servers: ${servers_found[*]}"
    fi

    if [[ ${#servers_missing[@]} -gt 0 ]]; then
        log_warning "Missing servers: ${servers_missing[*]}"
    fi

    if [[ ${#servers_found[@]} -eq 0 ]]; then
        log_error "No protobuf language servers were successfully installed"
        return 1
    fi

    # Show configuration info
    echo ""
    log_info "Emacs Configuration:"
    log_info "Your init-proto.el is already configured to use these servers automatically."
    log_info "Priority order: protobuf-language-server > pbls > buf"
    echo ""
    log_info "To test the installation:"
    log_info "1. Restart Emacs or reload your configuration"
    log_info "2. Open a .proto file"
    log_info "3. LSP should start automatically"
    log_info "4. Use M-x protobuf-show-lsp-info for more details"

    return 0
}

# Create post-installation info file
create_info_file() {
    local info_file="$(dirname "${BASH_SOURCE[0]}")/../protobuf-lsp-info.txt"

    cat > "$info_file" << EOF
Protobuf Language Server Installation

Installation completed on: $(date)
Distribution: $DISTRO_NAME ($DISTRO)

Installed Language Servers:
$(if command_exists protobuf-language-server; then echo "  ✓ protobuf-language-server: $(which protobuf-language-server)"; fi)
$(if command_exists pbls; then echo "  ✓ pbls: $(which pbls)"; fi)
$(if command_exists buf; then echo "  ✓ buf: $(which buf)"; fi)

Available Commands in Emacs:
  C-c l         - LSP command prefix
  M-.           - Go to definition
  M-?           - Find references
  C-c C-f       - Format buffer (buf format)
  C-c C-l       - Lint buffer (buf lint)

Additional Tools:
$(if command_exists protoc; then echo "  ✓ protoc: $(protoc --version 2>/dev/null || echo 'installed')"; fi)

Installation Notes:
- LSP will start automatically when opening .proto files
- Use M-x protobuf-show-lsp-info for more help
- Check *lsp-log* buffer if you encounter issues

Manual Installation Commands:
  Go-based: go install github.com/lasorda/protobuf-language-server@master
  Rust-based: cargo install --git https://github.com/rcorre/pbls
  Buf CLI: See https://buf.build/docs/installation

EOF

    log_info "Created installation info file: $info_file"
}

# Main installation function
main() {
    # Parse command line arguments
    local install_all=true
    local specific_server=""

    while [[ $# -gt 0 ]]; do
        case $1 in
            --server)
                specific_server="$2"
                install_all=false
                shift 2
                ;;
            --all)
                install_all=true
                shift
                ;;
            --help)
                show_help
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done

    # Header
    log_header "=========================================="
    log_header "Protobuf Language Server Installation"
    log_header "=========================================="
    echo ""

    # Detect distribution
    detect_distro
    echo ""

    # Install specific server or all servers
    local success=true

    if [[ "$install_all" == "true" ]]; then
        log_info "Installing all available protobuf language servers..."
        echo ""

        install_protobuf_language_server || log_warning "protobuf-language-server installation failed"
        echo ""
        install_pbls || log_warning "pbls installation failed"
        echo ""
        install_buf || log_warning "buf installation failed"
        echo ""
    else
        case "$specific_server" in
            "protobuf-language-server")
                install_protobuf_language_server || success=false
                ;;
            "pbls")
                install_pbls || success=false
                ;;
            "buf")
                install_buf || success=false
                ;;
            *)
                log_error "Unknown server: $specific_server"
                log_info "Available servers: protobuf-language-server, pbls, buf"
                exit 1
                ;;
        esac
        echo ""
    fi

    # Verify installations
    verify_installations

    # Create info file
    create_info_file

    echo ""
    log_header "=========================================="
    if [[ "$success" == "true" ]]; then
        log_success "Installation completed successfully!"
    else
        log_warning "Installation completed with some issues"
    fi
    log_header "=========================================="

    echo ""
    log_info "Next steps:"
    log_info "1. Restart your shell or run: source ~/.bashrc"
    log_info "2. Restart Emacs or reload your configuration"
    log_info "3. Open a .proto file to test LSP functionality"
    log_info "4. Use M-x protobuf-show-lsp-info for more information"
}

# Run main function with all arguments
main "$@"
