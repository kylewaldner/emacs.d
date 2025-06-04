#!/bin/bash

# Script to install Python dependencies for Emacs with modern LSP support
# Detects Linux distribution and uses appropriate package manager
# Updated to support Python 3.13 with python-lsp-server instead of anaconda-mode

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
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

# Function to detect distribution
detect_distro() {
    if [[ -f /etc/os-release ]]; then
        source /etc/os-release
        DISTRO=$ID
        VERSION=$VERSION_ID
    elif [[ -f /etc/redhat-release ]]; then
        DISTRO="rhel"
    elif [[ -f /etc/debian_version ]]; then
        DISTRO="debian"
    else
        DISTRO="unknown"
    fi
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to install packages based on distribution
install_packages() {
    local packages=("$@")

    case $DISTRO in
        "manjaro"|"arch"|"endeavouros"|"artix")
            print_status "Detected Arch-based distribution: $DISTRO"
            print_status "Installing packages with pacman: ${packages[*]}"
            if command_exists pacman; then
                sudo pacman -Sy --noconfirm "${packages[@]}"
            else
                print_error "pacman not found"
                return 1
            fi
            ;;
        "ubuntu"|"debian"|"mint"|"elementary"|"zorin"|"kali"|"parrot")
            print_status "Detected Debian-based distribution: $DISTRO"
            print_status "Installing packages with apt: ${packages[*]}"
            if command_exists apt; then
                sudo apt update
                sudo apt install -y "${packages[@]}"
            elif command_exists apt-get; then
                sudo apt-get update
                sudo apt-get install -y "${packages[@]}"
            else
                print_error "apt/apt-get not found"
                return 1
            fi
            ;;
        "fedora"|"centos"|"rhel"|"rocky"|"almalinux")
            print_status "Detected Red Hat-based distribution: $DISTRO"
            print_status "Installing packages with dnf/yum: ${packages[*]}"
            if command_exists dnf; then
                sudo dnf install -y "${packages[@]}"
            elif command_exists yum; then
                sudo yum install -y "${packages[@]}"
            else
                print_error "dnf/yum not found"
                return 1
            fi
            ;;
        "opensuse"|"suse"|"opensuse-leap"|"opensuse-tumbleweed")
            print_status "Detected openSUSE distribution: $DISTRO"
            print_status "Installing packages with zypper: ${packages[*]}"
            if command_exists zypper; then
                sudo zypper install -y "${packages[@]}"
            else
                print_error "zypper not found"
                return 1
            fi
            ;;
        "alpine")
            print_status "Detected Alpine Linux"
            print_status "Installing packages with apk: ${packages[*]}"
            if command_exists apk; then
                sudo apk add "${packages[@]}"
            else
                print_error "apk not found"
                return 1
            fi
            ;;
        "void")
            print_status "Detected Void Linux"
            print_status "Installing packages with xbps: ${packages[*]}"
            if command_exists xbps-install; then
                sudo xbps-install -Sy "${packages[@]}"
            else
                print_error "xbps-install not found"
                return 1
            fi
            ;;
        *)
            print_warning "Unknown or unsupported distribution: $DISTRO"
            print_warning "Falling back to pip installation"
            return 1
            ;;
    esac
}

# Function to install via pip as fallback
install_via_pip() {
    print_status "Installing Python packages via pip (modern LSP setup)"

    # Check if python3 and venv are available
    if ! command_exists python3; then
        print_error "python3 not found. Please install Python 3 first."
        return 1
    fi

    # Create a virtual environment in .emacs.d if it doesn't exist
    local venv_dir="$(dirname "$(dirname "${BASH_SOURCE[0]}")")/python-venv"

    if [[ ! -d "$venv_dir" ]]; then
        print_status "Creating virtual environment at $venv_dir"
        python3 -m venv "$venv_dir"
    fi

    # Activate virtual environment and install packages
    print_status "Activating virtual environment"
    source "$venv_dir/bin/activate"

    # Upgrade pip first
    pip install --upgrade pip

    # Install modern LSP packages (replaces anaconda-mode)
    local lsp_packages=(
        "python-lsp-server[all]"  # Main LSP server with all plugins
        "python-lsp-black"        # Black formatting plugin
        "python-lsp-ruff"         # Ruff linting plugin (modern alternative to flake8)
        "pylsp-mypy"              # MyPy type checking plugin
    )
    
    # Legacy packages for backward compatibility
    local legacy_packages=(
        "jedi"          # Still used by some tools
        "autopep8"      # Code formatting
        "flake8"        # Linting (fallback)
        "importmagic"   # Import management
        "yapf"          # Alternative formatter
        "rope"          # Refactoring
    )

    print_status "Installing LSP packages: ${lsp_packages[*]}"
    pip install "${lsp_packages[@]}"
    
    print_status "Installing legacy packages for compatibility: ${legacy_packages[*]}"
    pip install "${legacy_packages[@]}"

    # Create a note about the virtual environment
    cat > "$(dirname "$venv_dir")/python-venv-info.txt" << EOF
Python Virtual Environment for Emacs

This directory contains a Python virtual environment with packages needed for modern Emacs Python development using LSP.

Virtual environment location: $venv_dir

To use this environment manually:
  source $venv_dir/bin/activate

Modern LSP packages installed:
  - python-lsp-server[all] - Main Language Server Protocol implementation
  - python-lsp-black - Black code formatter integration
  - python-lsp-ruff - Ruff linter integration (fast alternative to flake8)
  - pylsp-mypy - MyPy type checking integration

Legacy packages (for compatibility):
  - jedi, autopep8, flake8, importmagic, yapf, rope

Full package list:
$(pip list)

Configuration:
  - This setup replaces anaconda-mode with modern LSP-based Python support
  - Supports Python 3.13 and newer versions
  - Use init-python-lsp.el instead of init-python.el for modern setup

Created on: $(date)
EOF

    print_success "Virtual environment created and packages installed"
    print_status "Virtual environment location: $venv_dir"
    print_status "Note: Modern LSP setup installed - supports Python 3.13!"
}

# Function to verify installation
verify_installation() {
    print_status "Verifying Python packages installation..."

    # Modern LSP packages
    local lsp_packages=("pylsp" "black" "ruff")
    # Legacy packages
    local legacy_packages=("jedi" "autopep8" "flake8" "rope")
    
    local failed=()
    local venv_dir="$(dirname "$(dirname "${BASH_SOURCE[0]}")")/python-venv"

    # Check if pylsp (main LSP server) is available
    if command_exists pylsp; then
        print_success "pylsp (Python LSP Server) found in system PATH"
    elif [[ -f "$venv_dir/bin/pylsp" ]]; then
        print_success "pylsp found in virtual environment"
    else
        print_error "pylsp (Python LSP Server) not found"
        failed+=("pylsp")
    fi

    # First try to check system packages
    for package in "${legacy_packages[@]}"; do
        if python3 -c "import $package" 2>/dev/null; then
            print_success "$package found in system Python"
        else
            failed+=("$package")
        fi
    done

    # If system packages failed and venv exists, check venv packages
    if [[ ${#failed[@]} -gt 0 && -d "$venv_dir" ]]; then
        print_status "Checking virtual environment packages..."
        source "$venv_dir/bin/activate"

        local venv_failed=()
        for package in "${failed[@]}"; do
            if [[ "$package" == "pylsp" ]]; then
                if command_exists pylsp; then
                    print_success "pylsp found in virtual environment"
                else
                    venv_failed+=("$package")
                fi
            elif python -c "import $package" 2>/dev/null; then
                print_success "$package found in virtual environment"
            else
                venv_failed+=("$package")
            fi
        done

        # Update failed list to only include packages not found in either location
        failed=("${venv_failed[@]}")

        # Create a note about using the venv
        if [[ ${#venv_failed[@]} -lt ${#legacy_packages[@]} ]]; then
            print_status "Note: Packages are installed in virtual environment at $venv_dir"
            print_status "Emacs is configured to use this Python environment automatically"
        fi
    fi

    if [[ ${#failed[@]} -eq 0 ]]; then
        print_success "All packages installed successfully!"
        print_success "Modern LSP setup ready - supports Python 3.13!"
        return 0
    else
        print_error "Failed to install: ${failed[*]}"
        return 1
    fi
}

# Main function
main() {
    print_status "Starting Python dependencies installation for modern Emacs LSP setup"
    print_status "This replaces anaconda-mode with python-lsp-server for Python 3.13 support"

    # Check if Python 3 is installed
    if ! command_exists python3; then
        print_error "Python 3 is not installed. Please install Python 3 first."
        exit 1
    fi

    print_status "Python version: $(python3 --version)"

    # Detect distribution
    detect_distro

    # For modern LSP setup, we primarily use pip since most distributions
    # don't have up-to-date python-lsp-server packages
    print_status "Using pip installation for modern LSP packages..."
    install_via_pip

    # Verify installation
    if verify_installation; then
        print_success "Installation completed successfully!"
        print_success "Modern Python LSP setup is ready!"
        print_status "To use the new setup:"
        print_status "1. Load init-python-lsp.el instead of init-python.el in your init.el"
        print_status "2. Install LSP packages in Emacs: M-x package-install RET lsp-mode RET"
        print_status "3. Restart Emacs and open a Python file"
        print_status "4. The LSP server will start automatically and provide modern IDE features"
    else
        print_error "Installation verification failed. You may need to install packages manually."
        exit 1
    fi
}

# Run main function
main "$@"
