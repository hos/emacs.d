# @hos' emacs configuration

This is my everchanging emacs configuration. I come by
every now and then for various micro optimizations.
The configuration itself is supposed to be self documenting.

#### Downloading and Installation

    git clone --recursive https://github.com/hos/emacs.d
    mv emacs.d ~/.emacs.d

*Be careful not to clobber your own config.*

##### Updating

    git pull --recurse-submodules

#### Package Management

I use `el-get` for package management. The packages I use are listed in
`init.el` in the `hos-packages` variable.

#### Files

##### `site-lisp/enhancements.el`

Enhancement functions to generic Emacs functionality.

##### `site-lisp/keybindings.el`

Personal keybindings, additionally to those of Emacs.

##### `site-lisp/appearance.el`

Font and theme settings.

##### `site-lisp/init-*.el`

Settings or overrides for programming language major modes.

##### `site-lisp/sane-defaults.el`

Settings that should be default in a boilerplate configuration.
