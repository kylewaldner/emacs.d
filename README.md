[this repo is a fork](https://github.com/purcell/emacs.d)


# important notes
install ag (silversearcher) on system to use ag for search
install tern and eslint (using npm) for nodejs development
`npm i -g eslint eslint-plugin-import eslint-config-airbnb-base eslint-plugin-json tern`


# Languages


* Haskell / Purescript / Elm
* Ruby / Ruby on Rails
* CSS / LESS / SASS / SCSS
* Javascript / Typescript / Coffeescript
* HTML / HAML / Markdown / Textile / ERB
* Rust
* Python
* Clojure (with Cider and nRepl)
* Common Lisp (with Slime)
* PHP
* Erlang

In particular, there's a nice config for *autocompletion* with
[company](https://company-mode.github.io/), and
[flycheck](http://www.flycheck.org) is used to immediately highlight
syntax errors in Ruby, Python, Javascript, Haskell and a number of
other languages.

## Supported Emacs versions

The config should run on Emacs 24.4 or greater and is designed to
degrade smoothly - see the Travis build - but note that much newer
versions are required for an increasing number of key packages, so to
get full functionality you should use the latest Emacs version
available to you.

Some Windows users might need to follow
[these instructions](http://xn--9dbdkw.se/diary/how_to_enable_GnuTLS_for_Emacs_24_on_Windows/index.en.html)
to get TLS (ie. SSL) support included in their Emacs.

## Other requirements

To make the most of the programming language-specific support in this
config, further programs will likely be required, particularly those
that [flycheck](https://github.com/flycheck/flycheck) uses to provide
on-the-fly syntax checking.

## Installation

To install, clone this repo to `~/.emacs.d`, i.e. ensure that the
`init.el` contained in this repo ends up at `~/.emacs.d/init.el`:

```
git clone https://github.com/purcell/emacs.d.git ~/.emacs.d
```

Upon starting up Emacs for the first time, further third-party
packages will be automatically downloaded and installed. If you
encounter any errors at that stage, try restarting Emacs, and possibly
running `M-x package-refresh-contents` before doing so.


## Updates

Update the config with `git pull`. You'll probably also want/need to update
the third-party packages regularly too:

<kbd>M-x package-list-packages</kbd>, then <kbd>U</kbd> followed by <kbd>x</kbd>.

You should usually restart Emacs after pulling changes or updating
packages so that they can take effect. Emacs should usually restore
your working buffers when you restart due to this configuration's use
of the `desktop` and `session` packages.

## Changing themes and adding your own customization

To add your own customization, use <kbd>M-x customize</kbd>, <kbd>M-x
customize-themes</kbd> etc. and/or create a file
`~/.emacs.d/lisp/init-local.el` which looks like this:

```el
... your code here ...

(provide 'init-local)
```

If you need initialisation code which executes earlier in the startup process,
you can also create an `~/.emacs.d/lisp/init-preload-local.el` file.

pIf you plan to customize things more extensively, you should probably
just fork the repo and hack away at the config to make it your own!
Remember to regularly merge in changes from this repo, so that your
config remains compatible with the latest package and Emacs versions.

*Please note that I cannot provide support for customised versions of
this configuration.*

## Similar configs

You might also want to check out `emacs-starter-kit` and `prelude`.

# Emacs Configuration Dependencies

## Quick Start

Run the installer:

```bash
./install.sh
```

The script will show you a menu of available installers and handle everything automatically.

## Command Line Options

If you prefer not to use the interactive menu:

```bash
# Install Python dependencies only
./install.sh python

# Install everything
./install.sh all

# List available installers
./install.sh --list

# Show help
./install.sh --help
```

## What This Fixes

### Python/Anaconda-mode Issues

If you're seeing this error when opening Python files:
```
Server error: InvalidPythonEnvironment('Could not get version information for '/usr/bin/python3'...)
```

The Python installer will fix it by installing the missing dependencies that anaconda-mode needs.

## Supported Distributions

The installers automatically detect your Linux distribution and use the right package manager:

- **Arch-based**: Manjaro, Arch, EndeavourOS, Artix (uses `pacman`)
- **Debian-based**: Ubuntu, Debian, Mint, Elementary (uses `apt`)
- **Red Hat-based**: Fedora, CentOS, RHEL, Rocky (uses `dnf`/`yum`)
- **openSUSE**: All variants (uses `zypper`)
- **Alpine**: Uses `apk`
- **Void**: Uses `xbps-install`

If your distribution isn't recognized, the installer falls back to `pip`.

## What Gets Installed

### Python Dependencies
- **jedi**: Code completion and analysis
- **autopep8**: Code formatting
- **flake8**: Code linting
- **rope**: Refactoring tools
- **importmagic**: Import organization
- **yapf**: Alternative code formatter

These are the packages that Emacs anaconda-mode and elpy expect to find.

## Adding New Installers

Want to add support for other dependencies? Just drop a new installer script in the `installation/` directory and update the `list_installers()` function in `install.sh`.

The installer scripts should:
- Be executable (`chmod +x`)
- Handle their own distribution detection
- Exit with proper return codes (0 for success, non-zero for failure)
- Provide user feedback

## Troubleshooting

**Permission denied**: Make sure the script is executable:
```bash
chmod +x install.sh
```

**Package not found**: Some older distributions might not have all packages. The installer will try `pip` as a fallback.

**Still getting anaconda-mode errors**: Try restarting Emacs completely after installation.

## Manual Installation

If the automatic installer doesn't work for your setup, you can install the Python dependencies manually:

### Using your package manager
```bash
# Arch/Manjaro
sudo pacman -S python-jedi python-autopep8 python-flake8 python-rope

# Ubuntu/Debian
sudo apt install python3-jedi python3-autopep8 python3-flake8 python3-rope

# Fedora
sudo dnf install python3-jedi python3-autopep8 python3-flake8 python3-rope
```

### Using pip
```bash
pip3 install --user jedi autopep8 flake8 rope importmagic yapf
```
