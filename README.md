[this repo is a fork](https://github.com/purcell/emacs.d)

# important notes
install ag (silversurfer) on system to use ag for search
install tern (using npm) for nodejs development


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
