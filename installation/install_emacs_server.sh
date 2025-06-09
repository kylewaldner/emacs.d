#!/bin/bash

# Emacs Server Setup Installation Script
# Sets up systemd service and shell aliases for Emacs server/client mode

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

# Script directory detection
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EMACS_SERVER_CONFIG_DIR="$SCRIPT_DIR/emacs-server-config"

# Function to get default Emacs path from service file template
get_default_emacs_path() {
    echo "$HOME/emacs/server_emacs"
}

# Non-interactive mode flag
NON_INTERACTIVE=false

# Function to check if systemctl is available
check_systemctl() {
    if ! command -v systemctl &> /dev/null; then
        print_error "systemctl not found. This script requires systemd."
        return 1
    fi
    return 0
}

# Function to detect user's shell
detect_shell() {
    local user_shell
    user_shell=$(basename "$SHELL")

    case "$user_shell" in
        "bash")
            echo "bash"
            ;;
        "zsh")
            echo "zsh"
            ;;
        "fish")
            echo "fish"
            ;;
        *)
            echo "bash" # Default to bash
            ;;
    esac
}

# Function to get shell RC file
get_shell_rc_file() {
    local shell_type="$1"

    case "$shell_type" in
        "bash")
            if [[ -f "$HOME/.bashrc" ]]; then
                echo "$HOME/.bashrc"
            else
                echo "$HOME/.bash_profile"
            fi
            ;;
        "zsh")
            echo "$HOME/.zshrc"
            ;;
        "fish")
            echo "$HOME/.config/fish/config.fish"
            ;;
        *)
            echo "$HOME/.bashrc"
            ;;
    esac
}

# Function to get user input for Emacs path
get_emacs_path() {
    local emacs_path
    local default_path="$HOME/emacs/server_emacs"
    local current_emacs_server_path

    # Check if emacsclientrc.sh exists and extract current EMACS_SERVER_PATH
    local emacsclientrc_file="$HOME/.emacsclient_bash_aliases"
    if [[ -f "$emacsclientrc_file" ]]; then
        current_emacs_server_path=$(grep "^export EMACS_SERVER_PATH=" "$emacsclientrc_file" 2>/dev/null | cut -d'"' -f2)
        if [[ -n "$current_emacs_server_path" ]]; then
            # Remove /bin suffix if present to get the base path
            current_emacs_server_path="${current_emacs_server_path%/bin}"
            print_status "Found existing EMACS_SERVER_PATH: $current_emacs_server_path/bin" >&2
            default_path="$current_emacs_server_path"
        fi
    fi

    if [[ "$NON_INTERACTIVE" == "true" ]]; then
        print_status "Non-interactive mode: using default Emacs path: $default_path" >&2
        echo "$default_path"
        return 0
    fi

    # Check if we're in an interactive shell
    if [[ ! -t 0 ]]; then
        print_warning "Non-interactive shell detected, using default path: $default_path" >&2
        echo "$default_path"
        return 0
    fi

    echo >&2
    print_status "Please specify the path to your server version of Emacs installation." >&2
    if [[ -n "$current_emacs_server_path" ]]; then
        echo -e "Current EMACS_SERVER_PATH: ${YELLOW}$current_emacs_server_path/bin${NC}" >&2
    fi
    echo -e "Default base path: ${YELLOW}$default_path${NC}" >&2
    echo -e "EMACS_SERVER_PATH will be set to: ${YELLOW}$default_path/bin${NC}" >&2
    echo -e "Press Enter to use default, or type a different base path:" >&2
    echo -n "> " >&2

    # Read user input with a reasonable timeout
    if read -t 30 -r emacs_path; then
        # User provided input or just pressed Enter
        if [[ -z "$emacs_path" ]]; then
            emacs_path="$default_path"
        fi
    else
        print_warning "No input received in 30 seconds, using default path" >&2
        emacs_path="$default_path"
    fi

    # Expand tilde if present
    emacs_path="${emacs_path/#\~/$HOME}"

    # Always return a valid path - use default if empty
    if [[ -z "$emacs_path" ]]; then
        emacs_path="$default_path"
        print_warning "Empty path detected, using default: $default_path" >&2
    fi

    echo "$emacs_path"
}

# Function to create systemd service file
create_systemd_service() {
    local emacs_path="$1"
    local service_file="$HOME/.config/systemd/user/emacs.service"
    local template_file="$EMACS_SERVER_CONFIG_DIR/emacs.service"
    local default_path="$HOME/emacs/server_emacs"

    print_status "Creating systemd user service file..."

    # Check if template file exists
    if [[ ! -f "$template_file" ]]; then
        print_error "Template service file not found: $template_file"
        return 1
    fi

    # Create user systemd directory if it doesn't exist
    mkdir -p "$HOME/.config/systemd/user"

    # Read the template file into a variable
    local service_content
    if ! service_content=$(cat "$template_file"); then
        print_error "Failed to read template file: $template_file"
        return 1
    fi

    print_status "Processing service file template..."

    # Check what paths are currently in the template
    local template_exec_start_path
    local template_exec_stop_path

    # Extract paths from template
    template_exec_start_path=$(echo "$service_content" | grep "^ExecStart=" | sed 's/ExecStart=\(.*\)\/bin\/emacs.*/\1/')
    template_exec_stop_path=$(echo "$service_content" | grep "^ExecStop=" | sed 's/ExecStop=\(.*\)\/bin\/emacsclient.*/\1/')

    # Convert %h to actual home path for comparison
    template_exec_start_path="${template_exec_start_path//%h/$HOME}"
    template_exec_stop_path="${template_exec_stop_path//%h/$HOME}"

    print_status "Template ExecStart path: $template_exec_start_path"
    print_status "Template ExecStop path: $template_exec_stop_path"
    print_status "User specified path: $emacs_path"

    # Check if user specified path is different from template paths
    local needs_update=false
    if [[ -n "$emacs_path" && "$emacs_path" != "$template_exec_start_path" ]]; then
        needs_update=true
        print_status "User path differs from template - update needed"
    else
        print_status "User path matches template - keeping original paths"
    fi

    # Update paths if necessary
    if [[ "$needs_update" == "true" ]]; then
        print_status "Custom Emacs path detected: $emacs_path"
        print_status "Updating ExecStart and ExecStop paths in service file..."

        # Replace ExecStart path - handle both %h format and absolute paths
        if [[ "$service_content" == *"ExecStart=%h/emacs/server_emacs/bin/emacs"* ]]; then
            service_content=$(echo "$service_content" | sed "s|ExecStart=%h/emacs/server_emacs/bin/emacs|ExecStart=${emacs_path}/bin/emacs|g")
        else
            # Handle other potential formats
            service_content=$(echo "$service_content" | sed "s|ExecStart=\(.*\)/bin/emacs|ExecStart=${emacs_path}/bin/emacs|g")
        fi

        # Replace ExecStop path - handle both %h format and absolute paths
        if [[ "$service_content" == *"ExecStop=%h/emacs/server_emacs/bin/emacsclient"* ]]; then
            service_content=$(echo "$service_content" | sed "s|ExecStop=%h/emacs/server_emacs/bin/emacsclient|ExecStop=${emacs_path}/bin/emacsclient|g")
        else
            # Handle other potential formats
            service_content=$(echo "$service_content" | sed "s|ExecStop=\(.*\)/bin/emacsclient|ExecStop=${emacs_path}/bin/emacsclient|g")
        fi

        print_success "Updated service file paths to use: $emacs_path"
    else
        print_status "Using template paths as-is (keeping systemd specifiers if present)"
    fi

    # Write the processed content to the service file
    if ! echo "$service_content" > "$service_file"; then
        print_error "Failed to write service file: $service_file"
        return 1
    fi

    print_success "Created systemd service file: $service_file"

    # Show what was actually written to the service file
    print_status "Service file contents:"
    echo "----------------------------------------"
    cat "$service_file"
    echo "----------------------------------------"

    # Reload systemd and enable service
    print_status "Enabling Emacs service..."
    systemctl --user daemon-reload
    systemctl --user enable emacs.service

    print_success "Emacs service enabled for user mode"

    # Ask if user wants to start the service now
    if [[ "$NON_INTERACTIVE" == "true" ]]; then
        print_status "Non-interactive mode: starting Emacs service automatically"
        REPLY="y"
    else
        echo -n "Do you want to start the Emacs service now? (Y/n): "
        if read -t 15 -n 1 -r REPLY; then
            echo
        else
            print_warning "No input received in 15 seconds, defaulting to Yes"
            REPLY="y"
        fi
    fi

    if [[ $REPLY =~ ^[Nn]$ ]]; then
        print_status "Service not started. You can start it later with: systemctl --user start emacs.service"
    else
        print_status "Starting Emacs service..."
        if systemctl --user start emacs.service; then
            print_success "Emacs service started successfully"
        else
            print_error "Failed to start Emacs service"
            print_status "You can check the status with: systemctl --user status emacs.service"
        fi
    fi
}

# Function to install shell aliases
install_shell_aliases() {
    local shell_type
    local rc_file
    local emacs_path="$1"
    local emacsclientrc_file="$HOME/.emacsclient_bash_aliases"
    local template_file="$EMACS_SERVER_CONFIG_DIR/emacsclientrc.sh"
    local default_path="$HOME/emacs/server_emacs"

    print_status "Setting up shell configuration..."

    # Check if template file exists
    if [[ ! -f "$template_file" ]]; then
        print_error "Template file not found: $template_file"
        return 1
    fi

    # Read the template file
    local template_content
    if ! template_content=$(cat "$template_file"); then
        print_error "Failed to read template file: $template_file"
        return 1
    fi

    # Update EMACS_SERVER_PATH if user specified a different path
    local final_content="$template_content"
    if [[ -n "$emacs_path" && "$emacs_path" != "$default_path" ]]; then
        print_status "Custom Emacs path detected: $emacs_path"
        print_status "Updating EMACS_SERVER_PATH in configuration..."

        # Replace the EMACS_SERVER_PATH with user's specified path
        final_content=$(echo "$template_content" | sed "s|export EMACS_SERVER_PATH=\"\$HOME/emacs/server_emacs/bin\"|export EMACS_SERVER_PATH=\"${emacs_path}/bin\"|g")
    else
        print_status "Using default path, keeping original configuration"
    fi

    # Write the processed content to the emacsclientrc file
    if ! echo "$final_content" > "$emacsclientrc_file"; then
        print_error "Failed to write configuration file: $emacsclientrc_file"
        return 1
    fi

    print_success "Created emacsclient configuration: $emacsclientrc_file"

    # Show what was written
    print_status "Configuration file contents:"
    echo "----------------------------------------"
    cat "$emacsclientrc_file"
    echo "----------------------------------------"

    # Detect shell and get RC file
    shell_type=$(detect_shell)
    rc_file=$(get_shell_rc_file "$shell_type")

    print_status "Detected shell: $shell_type"
    print_status "RC file: $rc_file"

    # Prepare source line based on shell type
    local source_line
    case "$shell_type" in
        "fish")
            # Fish shell uses different syntax
            source_line="source $emacsclientrc_file"
            print_status "Using Fish shell syntax for sourcing"
            ;;
        "zsh"|"bash"|*)
            # Bash/Zsh compatible shells
            source_line="source \"$emacsclientrc_file\""
            print_status "Using Bash/Zsh syntax for sourcing"
            ;;
    esac

    # Check if configuration is already sourced
    if [[ -f "$rc_file" ]] && grep -Fq "$emacsclientrc_file" "$rc_file"; then
        print_warning "Configuration file already sourced in $rc_file"
        return 0
    fi

    # Add source line to RC file
    echo "" >> "$rc_file"
    echo "# Emacs client configuration" >> "$rc_file"
    echo "$source_line" >> "$rc_file"

    print_success "Added configuration source line to $rc_file"
    print_status "Restart your shell or run 'source $rc_file' to use the new configuration"

    # For non-bash shells, provide additional instructions
    if [[ "$shell_type" == "fish" ]]; then
        print_status "Note: Fish shell detected. The alias syntax has been configured appropriately."
    elif [[ "$shell_type" != "bash" && "$shell_type" != "zsh" ]]; then
        print_warning "Detected shell: $shell_type. Please verify the configuration works correctly."
        print_status "You may need to manually adjust the syntax in $emacsclientrc_file for your shell."
    fi
}

# Function to show service management commands
show_service_commands() {
    print_header "Useful systemd commands for managing Emacs service:"
    echo
    echo "  Start service:    systemctl --user start emacs.service"
    echo "  Stop service:     systemctl --user stop emacs.service"
    echo "  Restart service:  systemctl --user restart emacs.service"
    echo "  Check status:     systemctl --user status emacs.service"
    echo "  View logs:        journalctl --user -u emacs.service"
    echo "  Disable service:  systemctl --user disable emacs.service"
    echo
}

# Main installation function
main() {
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --non-interactive|-y)
                NON_INTERACTIVE=true
                shift
                ;;
            --help|-h)
                echo "Usage: $0 [OPTIONS]"
                echo "Options:"
                echo "  --non-interactive, -y   Run in non-interactive mode with defaults"
                echo "  --help, -h             Show this help message"
                exit 0
                ;;
            *)
                print_warning "Unknown option: $1"
                shift
                ;;
        esac
    done

    print_header "Emacs Server Setup Installation"
    echo

    if [[ "$NON_INTERACTIVE" == "true" ]]; then
        print_status "Running in non-interactive mode with default settings"
        echo
    fi

    # Check prerequisites
    if ! check_systemctl; then
        exit 1
    fi

    # Check if emacs-server-config directory exists
    if [[ ! -d "$EMACS_SERVER_CONFIG_DIR" ]]; then
        print_error "Emacs server config directory not found: $EMACS_SERVER_CONFIG_DIR"
        exit 1
    fi
    print_status "Found emacs-server-config directory: $EMACS_SERVER_CONFIG_DIR"

    # Get Emacs installation path
    local emacs_path
    print_status "Getting Emacs installation path..."
    emacs_path=$(get_emacs_path)
    print_success "Using Emacs path: $emacs_path"
    echo

    # Install systemd service
    print_status "Setting up systemd service..."
    create_systemd_service "$emacs_path"
    echo

    # Install shell aliases
    print_status "Installing shell aliases..."
    install_shell_aliases "$emacs_path"
    echo

    # Show service management commands
    show_service_commands

    print_success "Emacs server setup installation completed!"
    print_status "Your Emacs server is now configured to run as a user service."
    print_status "Use 'emacsclient' (which now runs 'emacsclient -c') to connect to the server."
}

# Run main function
main "$@"
