#!/bin/bash

# Script to install Scala development dependencies for Emacs
# Installs Java, SBT, Scalafmt, Coursier, and other tools needed for modern Scala development
# Supports tree-sitter, LSP (Metals), and modern formatting

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
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

print_header() {
    echo -e "${BOLD}${CYAN}$1${NC}"
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

# Function to check Java version
check_java() {
    if command_exists java; then
        local java_version
        java_version=$(java -version 2>&1 | head -n1 | awk -F '"' '{print $2}')
        local major_version
        major_version=$(echo "$java_version" | awk -F. '{print $1}')
        
        if [[ $major_version -ge 17 ]]; then
            print_success "Java $java_version detected (requirement: Java 17+)"
            return 0
        else
            print_warning "Java $java_version detected, but Java 17+ is required for Metals and modern Scala development"
            return 1
        fi
    else
        print_warning "Java not found"
        return 1
    fi
}

# Function to install Java based on distribution
install_java() {
    # Check if we have Java but wrong version
    if command_exists java; then
        local java_version
        java_version=$(java -version 2>&1 | head -n1 | awk -F '"' '{print $2}')
        print_status "Found existing Java $java_version, but Java 17+ is required"
        print_status "Installing Java 17+ alongside existing installation..."
    else
        print_status "No Java installation found, installing Java 17+..."
    fi

    local packages=()

    case $DISTRO in
        "manjaro"|"arch"|"endeavouros"|"artix")
            packages=("jdk17-openjdk")
            ;;
        "ubuntu"|"debian"|"mint"|"elementary"|"zorin"|"kali"|"parrot")
            packages=("openjdk-17-jdk" "openjdk-17-jre")
            ;;
        "fedora"|"centos"|"rhel"|"rocky"|"almalinux")
            packages=("java-17-openjdk" "java-17-openjdk-devel")
            ;;
        "opensuse"|"suse"|"opensuse-leap"|"opensuse-tumbleweed")
            packages=("java-17-openjdk" "java-17-openjdk-devel")
            ;;
        "alpine")
            packages=("openjdk17-jdk")
            ;;
        *)
            print_error "Unsupported distribution for automatic Java installation: $DISTRO"
            print_status "Please install Java 17+ manually and run this script again"
            return 1
            ;;
    esac

    print_status "Installing Java 17+ packages: ${packages[*]}"
    if ! install_packages "${packages[@]}"; then
        print_error "Failed to install Java packages"
        return 1
    fi

    # After installation, we may need to set Java 17 as default
    print_status "Checking if Java 17+ is now available..."
    
    # On Arch-based systems, we might need to use archlinux-java to set the default
    if [[ "$DISTRO" == "manjaro" || "$DISTRO" == "arch" || "$DISTRO" == "endeavouros" || "$DISTRO" == "artix" ]]; then
        if command_exists archlinux-java; then
            print_status "Setting Java 17 as default using archlinux-java..."
            local java17_path
            java17_path=$(archlinux-java status | grep "java-17-openjdk" | head -1 | awk '{print $1}')
            if [[ -n "$java17_path" ]]; then
                sudo archlinux-java set "$java17_path"
                print_success "Set $java17_path as default Java"
            else
                print_warning "Could not find Java 17 installation to set as default"
                print_status "Available Java versions:"
                archlinux-java status || true
            fi
        fi
    fi

    # Verify that Java 17+ is now available
    if command_exists java; then
        local new_java_version
        new_java_version=$(java -version 2>&1 | head -n1 | awk -F '"' '{print $2}')
        local major_version
        major_version=$(echo "$new_java_version" | awk -F. '{print $1}')
        
        if [[ $major_version -ge 17 ]]; then
            print_success "Java $new_java_version is now available and compatible"
        else
            print_warning "Java $new_java_version is active, but Java 17+ is preferred"
            print_status "You may need to configure your system to use Java 17 by default"
            
            # Provide distribution-specific guidance
            case $DISTRO in
                "manjaro"|"arch"|"endeavouros"|"artix")
                    print_status "Try: sudo archlinux-java set java-17-openjdk"
                    ;;
                "ubuntu"|"debian"|"mint"|"elementary"|"zorin"|"kali"|"parrot")
                    print_status "Try: sudo update-alternatives --config java"
                    ;;
                "fedora"|"centos"|"rhel"|"rocky"|"almalinux")
                    print_status "Try: sudo alternatives --config java"
                    ;;
            esac
        fi
    else
        print_error "Java still not found after installation"
        return 1
    fi

    return 0
}

# Function to install packages based on distribution
install_packages() {
    local packages=("$@")

    case $DISTRO in
        "manjaro"|"arch"|"endeavouros"|"artix")
            print_status "Detected Arch-based distribution: $DISTRO"
            if command_exists pacman; then
                sudo pacman -Sy --noconfirm "${packages[@]}"
            else
                print_error "pacman not found"
                return 1
            fi
            ;;
        "ubuntu"|"debian"|"mint"|"elementary"|"zorin"|"kali"|"parrot")
            print_status "Detected Debian-based distribution: $DISTRO"
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
            if command_exists zypper; then
                sudo zypper install -y "${packages[@]}"
            else
                print_error "zypper not found"
                return 1
            fi
            ;;
        "alpine")
            print_status "Detected Alpine Linux"
            if command_exists apk; then
                sudo apk add "${packages[@]}"
            else
                print_error "apk not found"
                return 1
            fi
            ;;
        "void")
            print_status "Detected Void Linux"
            if command_exists xbps-install; then
                sudo xbps-install -Sy "${packages[@]}"
            else
                print_error "xbps-install not found"
                return 1
            fi
            ;;
        *)
            print_warning "Unknown distribution: $DISTRO"
            print_warning "Please install packages manually: ${packages[*]}"
            return 1
            ;;
    esac
}

# Function to install SBT
install_sbt() {
    if command_exists sbt; then
        local sbt_version
        sbt_version=$(sbt --version 2>&1 | grep "sbt version" | awk '{print $4}')
        print_success "SBT $sbt_version already installed"
        return 0
    fi

    print_status "Installing SBT (Scala Build Tool)..."

    case $DISTRO in
        "manjaro"|"arch"|"endeavouros"|"artix")
            install_packages "sbt"
            ;;
        "ubuntu"|"debian"|"mint"|"elementary"|"zorin"|"kali"|"parrot")
            # Add SBT repository
            print_status "Adding SBT repository..."
            curl -fsSL https://www.scala-sbt.org/sbt-deb.gpg | sudo gpg --dearmor -o /usr/share/keyrings/sbt-archive-keyring.gpg
            echo "deb [signed-by=/usr/share/keyrings/sbt-archive-keyring.gpg] https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
            sudo apt update
            install_packages "sbt"
            ;;
        "fedora"|"centos"|"rhel"|"rocky"|"almalinux")
            # Add SBT repository
            print_status "Adding SBT repository..."
            sudo rm -f /etc/yum.repos.d/bintray-rpm.repo || true
            curl -fsSL https://www.scala-sbt.org/sbt-rpm.gpg | sudo gpg --import
            curl -fsSL https://www.scala-sbt.org/sbt-rpm.repo | sudo tee /etc/yum.repos.d/sbt-rpm.repo
            install_packages "sbt"
            ;;
        *)
            # Fallback: install via Coursier
            print_status "Installing SBT via Coursier..."
            if command_exists cs; then
                cs install sbt
            else
                print_error "Cannot install SBT automatically on this distribution"
                print_status "Please install SBT manually from https://www.scala-sbt.org/download.html"
                return 1
            fi
            ;;
    esac
}

# Function to install Coursier
install_coursier() {
    if command_exists cs; then
        print_success "Coursier already installed"
        return 0
    fi

    print_status "Installing Coursier (Scala application launcher)..."

    # Install Coursier using the official installer
    curl -fLo cs https://git.io/coursier-cli-"$(uname | tr '[:upper:]' '[:lower:]')"
    chmod +x cs
    
    # Move to user local bin
    local install_dir="$HOME/.local/bin"
    mkdir -p "$install_dir"
    mv cs "$install_dir/"
    
    # Add to PATH if not already there
    if ! echo "$PATH" | grep -q "$install_dir"; then
        print_status "Adding $install_dir to PATH in ~/.bashrc"
        echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
        export PATH="$HOME/.local/bin:$PATH"
    fi
    
    print_success "Coursier installed to $install_dir/cs"
}

# Function to install Scala tools via Coursier
install_scala_tools() {
    if ! command_exists cs; then
        print_error "Coursier not found. Cannot install Scala tools."
        return 1
    fi

    # Ensure ~/.local/bin is in PATH and exists
    local install_dir="$HOME/.local/bin"
    mkdir -p "$install_dir"
    
    # Add to PATH if not already there
    if ! echo "$PATH" | grep -q "$install_dir"; then
        print_status "Adding $install_dir to PATH"
        export PATH="$install_dir:$PATH"
        # Also add to bashrc for future sessions
        if ! grep -q 'export PATH="$HOME/.local/bin:$PATH"' ~/.bashrc; then
            echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
        fi
    fi

    # Check Java version for Metals compatibility
    local java_version
    java_version=$(java -version 2>&1 | head -n1 | awk -F '"' '{print $2}')
    local major_version
    major_version=$(echo "$java_version" | awk -F. '{print $1}')
    
    if [[ $major_version -lt 17 ]]; then
        print_error "Current Java version is $java_version, but Metals requires Java 17+"
        print_error "Please ensure Java 17+ is installed before continuing"
        return 1
    fi

    print_status "Installing essential Scala development tools via Coursier..."
    print_status "Using Java $java_version (compatible with Metals)"

    # Install Scalafmt (code formatter) - important for development
    if ! command_exists scalafmt; then
        print_status "Installing Scalafmt..."
        if cs install --install-dir "$install_dir" scalafmt; then
            print_success "Scalafmt installed successfully"
        else
            print_warning "Scalafmt installation failed, but continuing..."
        fi
    else
        print_success "Scalafmt already installed"
    fi

    # Install Scalafix (refactoring tool) - optional but useful
    if ! command_exists scalafix; then
        print_status "Installing Scalafix..."
        if cs install --install-dir "$install_dir" scalafix; then
            print_success "Scalafix installed successfully"
        else
            print_warning "Scalafix installation failed, but continuing..."
        fi
    else
        print_success "Scalafix already installed"
    fi

    # Install Metals (LSP server) - this is critical
    print_status "Installing Metals LSP server (critical for Emacs)..."
    
    if cs install --install-dir "$install_dir" --force metals; then
        print_success "Metals LSP server installed successfully"
    else
        print_error "Failed to install Metals LSP server"
        print_error "This will prevent LSP functionality in Emacs"
        return 1
    fi

    # Refresh PATH for current session
    export PATH="$install_dir:$PATH"
    
    # Small delay to allow filesystem to sync
    sleep 1
    
    # Verify Metals installation
    if command_exists metals; then
        local metals_version
        metals_version=$(metals --version 2>/dev/null | head -1 || echo 'version check failed')
        print_success "Metals verification successful: $metals_version"
    elif [[ -f "$install_dir/metals" ]]; then
        print_success "Metals binary found at $install_dir/metals"
        chmod +x "$install_dir/metals"
        print_status "You may need to restart your shell or run: source ~/.bashrc"
    else
        print_error "Metals installation verification failed"
        return 1
    fi
    
    print_success "Essential Scala tools installation completed"
    print_status "Tools installed in: $install_dir"
    
    # Show what was installed
    print_status "Installed tools:"
    [[ -f "$install_dir/metals" ]] && echo "  ✓ Metals LSP server (critical)"
    [[ -f "$install_dir/scalafmt" ]] && echo "  ✓ Scalafmt (code formatter)"
    [[ -f "$install_dir/scalafix" ]] && echo "  ✓ Scalafix (refactoring tool)"
}

# Function to install tree-sitter (optional, for Emacs 29+)
install_tree_sitter() {
    if command_exists tree-sitter; then
        print_success "Tree-sitter already installed"
        return 0
    fi

    print_status "Checking tree-sitter support (for Emacs 29+)..."
    if command_exists tree-sitter; then
        print_success "Tree-sitter found - enhanced syntax highlighting available"
    else
        print_status "Tree-sitter not found - installing if available in package manager..."
        case $DISTRO in
            "manjaro"|"arch"|"endeavouros"|"artix")
                if install_packages "tree-sitter"; then
                    print_success "Tree-sitter installed successfully"
                else
                    print_warning "Tree-sitter installation failed"
                fi
                ;;
            *)
                print_status "Tree-sitter not available in package manager for $DISTRO"
                ;;
        esac
    fi
    
    print_status "Note: Modern Emacs (29+) can install tree-sitter grammars automatically"
    print_status "The Scala grammar will be installed when you first open a .scala file"
}

# Function to setup .scalafmt.conf example
setup_scalafmt_config() {
    local config_file="$HOME/.scalafmt.conf"
    
    if [[ ! -f "$config_file" ]]; then
        print_status "Creating example .scalafmt.conf in home directory..."
        cat > "$config_file" << 'EOF'
version = "3.7.17"

# Basic formatting settings
maxColumn = 100
indent.main = 2
indent.significant = 2
indent.callSite = 2
indent.ctrlSite = 2
indent.defnSite = 2
indent.caseSite = 2

# Alignment
align.preset = most
align.multiline = false

# Line breaks
newlines.beforeCurlyLambdaParams = multilineWithCaseOnly
newlines.afterCurlyLambdaParams = never
newlines.implicitParamListModifierPrefer = before
newlines.sometimesBeforeColonInMethodReturnType = true

# Rewrite rules
rewrite.rules = [RedundantBraces, RedundantParens, SortModifiers]
rewrite.redundantBraces.methodBodies = false
rewrite.redundantBraces.includeUnitMethods = true
rewrite.redundantBraces.stringInterpolation = false
rewrite.redundantBraces.parensForOneLineApply = true

# Import organization
rewrite.imports.sort = ascii
rewrite.imports.groups = [
  ["javax?\\..*"],
  ["scala\\..*"],
  [".*"]
]

# Comments
docstrings.style = Asterisk
docstrings.oneline = keep

# Other settings
assumeStandardLibraryStripMargin = true
includeNoParensInSelectChains = false
EOF
        print_success "Created example .scalafmt.conf in $config_file"
        print_status "You can customize this file for your projects"
    else
        print_success ".scalafmt.conf already exists in home directory"
    fi
}

# Function to create Scala development info file
create_info_file() {
    local info_file="$(dirname "$(dirname "${BASH_SOURCE[0]}")")/scala-dev-info.txt"
    
    cat > "$info_file" << EOF
Scala Development Environment Setup for Emacs

This document describes the tools installed for modern Scala development with Emacs.

=== Core Dependencies ===

1. Java 17+ (OpenJDK)
   - Required for running Scala, SBT, and Metals LSP server
   - Minimum version: Java 17 (updated requirement for modern tooling)
   - Verify: java -version

2. SBT (Scala Build Tool)
   - Standard build tool for Scala projects
   - Verify: sbt --version

3. Coursier
   - Scala application launcher and dependency resolver
   - Location: ~/.local/bin/cs
   - Verify: cs --version

=== Development Tools ===

4. Scalafmt
   - Code formatter for Scala
   - Integrated with Emacs via apheleia
   - Config: ~/.scalafmt.conf
   - Verify: scalafmt --version

5. Scalafix
   - Refactoring and linting tool
   - Verify: scalafix --version

6. Scala CLI
   - Modern Scala scripting tool
   - Verify: scala-cli --version

7. Metals LSP Server
   - Language Server Protocol implementation for Scala
   - Provides IDE features in Emacs via lsp-mode
   - Automatically configured with lsp-mode
   - Verify: metals --version

=== Tree-sitter Support (Emacs 29+) ===

8. Tree-sitter
   - Fast parsing library for syntax highlighting
   - Enables scala-ts-mode in Emacs
   - Verify: tree-sitter --version

9. Scala Tree-sitter Grammar
   - Automatically installed during setup
   - Located in ~/.emacs.d/tree-sitter/
   - Provides enhanced syntax highlighting and structural editing
   - Check status in Emacs: M-x scala-treesitter-status

=== Emacs Integration ===

The following Emacs packages work with these tools:

- scala-mode / scala-ts-mode: Major mode for Scala files
- lsp-mode + lsp-metals: IDE features (completion, go-to-definition, etc.)
- sbt-mode: SBT integration
- apheleia: Automatic formatting with Scalafmt
- flycheck: Error checking
- combobulate: Structural editing (tree-sitter)

=== Quick Start ===

1. Open a Scala file in Emacs (if using Emacs 29+, scala-ts-mode will be used automatically)
2. LSP will prompt to import the build - accept it
3. Use C-c l for LSP commands
4. Use C-c C-c to compile via SBT
5. Files are automatically formatted on save
6. Tree-sitter provides enhanced syntax highlighting and structural editing

=== Troubleshooting ===

If you encounter issues:

1. Check Java version: java -version (should be 17+)
2. Verify PATH includes ~/.local/bin
3. Run: source ~/.bashrc to reload PATH
4. For tree-sitter issues: M-x scala-treesitter-status in Emacs
5. To reinstall grammar: M-x install-scala-treesitter-grammar in Emacs
6. For LSP issues: M-x lsp-doctor in Emacs
7. If Metals is not found: Check ~/.local/bin/metals exists and is executable

For more information, see:
- Metals documentation: https://scalameta.org/metals/
- Tree-sitter setup: https://tree-sitter.github.io/tree-sitter/
- SBT documentation: https://www.scala-sbt.org/

Installation completed on: $(date)
EOF

    print_success "Created development info file: $info_file"
}

# Main installation function
main() {
    print_header "Scala Development Environment Installer for Emacs"
    print_status "This script will install all dependencies needed for modern Scala development"
    echo

    # Detect distribution
    detect_distro
    print_status "Detected distribution: $DISTRO"
    echo

    # Check and install Java
    print_header "1. Checking Java installation..."
    if ! check_java; then
        print_status "Installing Java 17+ (required for Metals and modern Scala development)..."
        if ! install_java; then
            print_error "Failed to install Java. Please install Java 17+ manually."
            exit 1
        fi
        
        # Check again after installation
        if ! check_java; then
            print_error "Java installation verification failed"
            print_status "Please ensure Java 17+ is properly installed and try again"
            exit 1
        fi
    fi
    echo

    # Install Coursier
    print_header "2. Installing Coursier..."
    if ! install_coursier; then
        print_error "Failed to install Coursier"
        exit 1
    fi
    echo

    # Install SBT
    print_header "3. Installing SBT..."
    if ! install_sbt; then
        print_error "Failed to install SBT"
        exit 1
    fi
    echo

    # Install Scala tools
    print_header "4. Installing Scala development tools..."
    if ! install_scala_tools; then
        print_error "Critical Scala tools installation failed"
        print_status "The script will continue, but LSP functionality may not work"
        print_status "You can try running the script again later"
    fi
    echo

    # Install tree-sitter (optional, for Emacs 29+)
    print_header "5. Checking tree-sitter support (for Emacs 29+)..."
    if command_exists tree-sitter; then
        local ts_version
        ts_version=$(tree-sitter --version 2>&1 || echo "unknown")
        print_success "✓ Tree-sitter installed ($ts_version)"
        print_status "  Scala grammar will be installed automatically by Emacs when first needed"
    else
        print_status "○ Tree-sitter not found (optional for Emacs 29+)"
        print_status "  You can still use scala-mode without tree-sitter"
    fi

    # Setup configuration files
    print_header "6. Setting up configuration..."
    setup_scalafmt_config
    create_info_file
    echo

    # Final verification
    print_header "7. Verifying installation..."
    local success=true
    
    if command_exists java; then
        local java_version
        java_version=$(java -version 2>&1 | head -n1 | awk -F '"' '{print $2}')
        local major_version
        major_version=$(echo "$java_version" | awk -F. '{print $1}')
        
        if [[ $major_version -ge 17 ]]; then
            print_success "✓ Java $java_version installed (Java 17+ required)"
        else
            print_error "✗ Java $java_version found, but Java 17+ is required"
            success=false
        fi
    else
        print_error "✗ Java not found"
        success=false
    fi
    
    if command_exists sbt; then
        local sbt_version
        sbt_version=$(sbt --version 2>&1 | grep "sbt version" | awk '{print $4}' || echo "unknown")
        print_success "✓ SBT $sbt_version installed"
    else
        print_error "✗ SBT not found"
        success=false
    fi
    
    if command_exists cs; then
        local cs_version
        cs_version=$(cs --version 2>&1 | head -1 || echo "unknown")
        print_success "✓ Coursier installed ($cs_version)"
    else
        print_error "✗ Coursier not found"
        success=false
    fi
    
    if command_exists scalafmt; then
        local scalafmt_version
        scalafmt_version=$(scalafmt --version 2>&1 || echo "unknown")
        print_success "✓ Scalafmt $scalafmt_version installed"
    else
        print_warning "⚠ Scalafmt not found in PATH"
    fi
    
    if command_exists metals; then
        local metals_version
        metals_version=$(metals --version 2>/dev/null | head -1 || echo "version check failed")
        print_success "✓ Metals LSP server installed ($metals_version)"
    else
        # Check if the binary exists even if not in PATH
        if [[ -f "$HOME/.local/bin/metals" ]]; then
            print_warning "⚠ Metals installed at ~/.local/bin/metals but not in current PATH"
            print_status "  Run: source ~/.bashrc or restart your terminal"
        else
            print_error "✗ Metals not found - this is required for LSP functionality"
            success=false
        fi
    fi
    
    echo
    
    if $success; then
        print_header "🎉 Installation completed successfully!"
        echo
        print_status "Next steps:"
        echo "1. Restart your terminal or run: source ~/.bashrc"
        echo "2. Open Emacs and edit a Scala file"
        echo "3. When prompted, import your SBT build"
        echo "4. Start coding! Tree-sitter and LSP should work automatically"
        echo
        print_status "For troubleshooting, see: $(dirname "$(dirname "${BASH_SOURCE[0]}")")/scala-dev-info.txt"
    else
        print_error "Installation completed with errors. Please check the output above."
        exit 1
    fi
}

# Run main function
main "$@" 