[this repo is a fork](https://github.com/purcell/emacs.d)


# important notes

TODO: I need to clean up this section and maybe add an installation script for it
install ag (silversearcher) on system to use ag for search
install tern and eslint (using npm) for nodejs development
`npm i -g eslint eslint-plugin-import eslint-config-airbnb-base eslint-plugin-json tern`


# Languages

* Scala
* some other ones idk

## Supported Emacs versions

I build this to work with a custom compiled version of emacs 30.1.90

It will probably work with other versions, but it is untested.

#### How I installed emacs

to use all the functionality, build emacs from source.

##### building normal emacs

clone the emacs repo

```
git clone git@git.sv.gnu.org:emacs.git
```

Then run

```
# tree sitter is required for full tree sitter support
# the rest are optional improvements
./configure \
  --with-tree-sitter \
  --with-native-compilation \
  --with-json \
  --with-modules \
  --with-x-toolkit=gtk3 \
  --with-cairo \
  --with-rsvg \
  --with-webp \
  --with-imagemagick
make -j$(nproc)
sudo make install
```

## building emacs server:

on some flavors of linux, you need to use a different x toolkit for emacs server to work with systemd.

I build it with this:

```
# you should install it into a different dir if you want to use gtk3 for non server emacs
# the x toolkit lucid option is needed for emacs server to work
# tree sitter is needed for tree sitter support
# the rest of these are optional improvements
mkdir -p $HOME/emacs/server_emacs
./configure --prefix=$HOME/emacs/server_emacs \
  --with-x-toolkit=lucid \
  --with-tree-sitter \
  --with-native-compilation \
  --with-json \
  --with-modules \
  --with-cairo \
  --with-rsvg \
  --with-webp \
  --with-imagemagick
make -j$(nproc)
make install
```

## Installation

After building emacs as described above, do the following

To install, clone this repo to `~/.emacs.d`, i.e. ensure that the
`init.el` contained in this repo ends up at `~/.emacs.d/init.el`:

```
git clone git@github.com:kylewaldner/emacs.d.git ~/.emacs.d

git://git.sv.gnu.org/emacs.git
```

### Additional external tools

some config uses software outside of emacs.

run the `./install.sh` script to get an interactive installer to help you install these.

I only tested it on manjaro linux, so you can read the script to see what you need to manually install if something does not work for you.

If you prefer not to use the interactive menu:

```bash
# Install Python dependencies only. There are also other installers for other languages
./install.sh python

# Install everything
./install.sh all

# List available installers
./install.sh --list

# Show help
./install.sh --help
```


#### Supported Distributions

The installers automatically detect your Linux distribution and use the right package manager:

- **Arch-based**: Manjaro, Arch, EndeavourOS, Artix (uses `pacman`)
- **Debian-based**: Ubuntu, Debian, Mint, Elementary (uses `apt`)
- **Red Hat-based**: Fedora, CentOS, RHEL, Rocky (uses `dnf`/`yum`)
- **openSUSE**: All variants (uses `zypper`)
- **Alpine**: Uses `apk`
- **Void**: Uses `xbps-install`

If your distribution isn't recognized, the installer falls back to `pip`.


##### Adding New Installers

To support more dependencies, add a new installer script in the `installation/` directory and update the `list_installers()` function in `install.sh`.

The installer scripts should:
- Be executable (`chmod +x`)
- Handle their own distribution detection
- Exit with proper return codes (0 for success, non-zero for failure)
- Provide user feedback

## Updates

Update the config with `git pull`. You'll probably also want/need to update
the third-party packages regularly too:

<kbd>M-x package-list-packages</kbd>, then <kbd>U</kbd> followed by <kbd>x</kbd>.

You should usually restart Emacs after pulling changes or updating
packages so that they can take effect. Emacs should usually restore
your working buffers when you restart due to this configuration's use
of the `desktop` and `session` packages.
