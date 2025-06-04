#!/bin/bash

# General installation script for Emacs configuration
# Provides menu-driven installation of various dependencies

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSTALLATION_DIR="$SCRIPT_DIR/installation"

# Function to print colored output
print_header() {
    echo -e "${BOLD}${CYAN}$1${NC}"
}

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

print_menu_item() {
    echo -e "${YELLOW}$1${NC} $2"
}

# Function to show banner
show_banner() {
    echo -e "${BOLD}${CYAN}"
    cat << "EOF"
 _____ __  __    _    ____ ____    ____             __ _
| ____|  \/  |  / \  / ___/ ___|  / ___|___  _ __  / _(_) __ _
|  _| | |\/| | / _ \| |   \___ \ | |   / _ \| '_ \| |_| |/ _` |
| |___| |  | |/ ___ \ |___ ___) || |__| (_) | | | |  _| | (_| |
|_____|_|  |_/_/   \_\____|____/  \____\___/|_| |_|_| |_|\__, |
                                                        |___/
    ___           _        _ _       _   _
   |_ _|_ __  ___| |_ __ _| | | __ _| |_(_) ___  _ __
    | || '_ \/ __| __/ _` | | |/ _` | __| |/ _ \| '_ \
    | || | | \__ \ || (_| | | | (_| | |_| | (_) | | | |
   |___|_| |_|___/\__\__,_|_|_|\__,_|\__|_|\___/|_| |_|

EOF
    echo -e "${NC}"
}

# Function to check if installation directory exists
check_installation_dir() {
    if [[ ! -d "$INSTALLATION_DIR" ]]; then
        print_error "Installation directory not found: $INSTALLATION_DIR"
        print_status "Creating installation directory..."
        mkdir -p "$INSTALLATION_DIR"
    fi
}

# Function to list available installers
list_installers() {
    local installers=()

    if [[ -f "$INSTALLATION_DIR/install_python_deps.sh" ]]; then
        installers+=("python" "Python Dependencies (jedi, autopep8, flake8, etc.)")
    fi

    if [[ -f "$INSTALLATION_DIR/install_scala_deps.sh" ]]; then
        installers+=("scala" "Scala Dependencies (Java, SBT, Metals, Scalafmt, etc.)")
    fi

    # Add more installers here as they are created
    # Example:
    # if [[ -f "$INSTALLATION_DIR/install_node_deps.sh" ]]; then
    #     installers+=("node" "Node.js Dependencies")
    # fi

    printf '%s\n' "${installers[@]}"
}

# Function to run specific installer
run_installer() {
    local installer_type="$1"

    case "$installer_type" in
        "python")
            if [[ -f "$INSTALLATION_DIR/install_python_deps.sh" ]]; then
                print_status "Running Python dependencies installer..."
                bash "$INSTALLATION_DIR/install_python_deps.sh"
            else
                print_error "Python installer not found!"
                return 1
            fi
            ;;
        "scala")
            if [[ -f "$INSTALLATION_DIR/install_scala_deps.sh" ]]; then
                print_status "Running Scala dependencies installer..."
                bash "$INSTALLATION_DIR/install_scala_deps.sh"
            else
                print_error "Scala installer not found!"
                return 1
            fi
            ;;
        "all")
            print_status "Running all available installers..."
            local success_count=0
            local total_count=0

            if [[ -f "$INSTALLATION_DIR/install_python_deps.sh" ]]; then
                ((total_count++))
                print_header "Installing Python Dependencies..."
                if bash "$INSTALLATION_DIR/install_python_deps.sh"; then
                    ((success_count++))
                    print_success "Python dependencies installed successfully"
                else
                    print_error "Python dependencies installation failed"
                fi
                echo
            fi

            if [[ -f "$INSTALLATION_DIR/install_scala_deps.sh" ]]; then
                ((total_count++))
                print_header "Installing Scala Dependencies..."
                if bash "$INSTALLATION_DIR/install_scala_deps.sh"; then
                    ((success_count++))
                    print_success "Scala dependencies installed successfully"
                else
                    print_error "Scala dependencies installation failed"
                fi
                echo
            fi

            # Add more installers here

            print_status "Installation summary: $success_count/$total_count installers completed successfully"
            if [[ $success_count -eq $total_count ]]; then
                print_success "All installations completed successfully!"
            else
                print_warning "Some installations failed. Check the output above for details."
            fi
            ;;
        *)
            print_error "Unknown installer type: $installer_type"
            return 1
            ;;
    esac
}

# Function to show menu
show_menu() {
    echo
    print_header "=== Available Installers ==="
    echo

    # Check if any installers exist
    local has_installers=false
    if [[ -f "$INSTALLATION_DIR/install_python_deps.sh" ]] || [[ -f "$INSTALLATION_DIR/install_scala_deps.sh" ]]; then
        has_installers=true
    fi

    if ! $has_installers; then
        print_warning "No installers found in $INSTALLATION_DIR"
        return 1
    fi

    echo "Select an option:"
}

# Function to get user choice - simplified
get_user_choice() {
    local options=()

    # Build options array
    if [[ -f "$INSTALLATION_DIR/install_python_deps.sh" ]]; then
        options+=("Install Python Dependencies")
    fi

    if [[ -f "$INSTALLATION_DIR/install_scala_deps.sh" ]]; then
        options+=("Install Scala Dependencies")
    fi

    options+=("Install All Dependencies" "Quit")

    select choice in "${options[@]}"; do
        case "$choice" in
            "Install Python Dependencies")
                echo "python"
                return 0
                ;;
            "Install Scala Dependencies")
                echo "scala"
                return 0
                ;;
            "Install All Dependencies")
                echo "all"
                return 0
                ;;
            "Quit")
                echo "quit"
                return 0
                ;;
            *)
                echo "Invalid selection. Please try again."
                ;;
        esac
    done
}

# Function to show usage
show_usage() {
    echo "Usage: $0 [OPTION]"
    echo
    echo "Options:"
    echo "  -h, --help     Show this help message"
    echo "  -l, --list     List available installers"
    echo "  python         Install Python dependencies"
    echo "  scala          Install Scala dependencies"
    echo "  all            Install all dependencies"
    echo
    echo "If no option is provided, interactive menu will be shown."
}

# Main function
main() {
    local arg="${1:-}"

    case "$arg" in
        "-h"|"--help")
            show_usage
            exit 0
            ;;
        "-l"|"--list")
            check_installation_dir
            echo "Available installers:"
            local installers_output
            installers_output=$(list_installers)

            # Read installers into array
            local installers=()
            while IFS= read -r line; do
                installers+=("$line")
            done <<< "$installers_output"

            for ((i=0; i<${#installers[@]}; i+=2)); do
                local key="${installers[i]}"
                local desc="${installers[i+1]}"
                echo "  $key - $desc"
            done
            exit 0
            ;;
        "python"|"scala"|"all")
            check_installation_dir
            run_installer "$arg"
            exit $?
            ;;
        "")
            # Interactive mode
            show_banner
            print_header "Welcome to the Emacs Configuration Installer!"
            print_status "This script helps you install various dependencies for your Emacs setup."

            check_installation_dir

            while true; do
                show_menu
                if ! choice=$(get_user_choice); then
                    continue
                fi

                if [[ "$choice" == "quit" ]]; then
                    print_status "Goodbye!"
                    exit 0
                fi

                echo
                print_header "Running installer: $choice"
                echo

                if run_installer "$choice"; then
                    echo
                    print_success "Installation completed!"
                    echo
                    read -p "Press Enter to continue..."
                else
                    echo
                    print_error "Installation failed!"
                    echo
                    read -p "Press Enter to continue..."
                fi
            done
            ;;
        *)
            print_error "Unknown option: $arg"
            show_usage
            exit 1
            ;;
    esac
}

# Run main function
main "$@"
