# nrs's emacs configuration

This is my everchanging emacs configuration. I come by to this repository
every now and then for micro optimizations that will make you
doubt my sanity. Don't worry, it's just a hobby.

The configuration itself is supposed to be self documenting. Looking into
`keybindings.el` and `enhancements.el` will reveal my text editing habits.

#### Downloading and Installation

    git clone --recursive https://github.com/nrs/emacs.d
    mv emacs.d ~/.emacs.d

*Of course, be careful not to clobber your own config.*

##### Updating

    git pull --recurse-submodules

#### Package Management

I use `el-get` for package management. The packages I use are listed in
`init.el` in the `nrs-packages` variable.
The package management system I selected allows me
to be able to install the same configuration on any machine in seconds.

As a personal decision, I use GNU Emacs, and do not care about backwards
compatibility or compatibility with other emacsen. I always install the latest
stable version.

#### Files
##### `enhancements.el`

This package contains features that surprised me when I heard emacs does not
have, partly after I got used to vim. For example the function `copy-line`
copies the current line without having to mark the region or kill the line.
The vanilla emacs way
of achieving this could be `C-a C-SPC C-e M-w` or `C-a C-k C-y` or `C-a C-k C-_`
or other solutions which involve too many keystrokes IMO. Vim does this just by
typing `yy` in normal mode, and also easier to learn and remember. Marking and
killing in Emacs have long been hardwired into my muscle memory, but I also like
to minimize the keystrokes whenever I can, when it makes sense. To this end, I
have defined and gleaned useful functions from all over the net, which
specifically enhance Emacs's editing behavior only, and deserve to be in this
package. Some of the functions are `copy-line`, `whack-whitespace`,
`comment-or-uncomment-region-or-line`, `move-line`, `insert-line-before` and so
on.

##### `keybindings.el`

My personal keybindings, additionally to those of emacs.

#### How I rediscovered emacs

I used emacs for two years immediately after I started programming, quite
inefficiently. I then discovered vim, which charmed me with its modal
capabilities. vim was so fast and efficient that I lost faith in emacs :'(, thinking
that the problem was in the editor rather than my configuration.

I used vim for another two years, until I discovered @magnars's *emacsrocks*
series. I realized the problem was with my configuration, and decided to create
this repository.

I now cherish both of them, and use either in different contexts. I use emacs while typing
plain text or TeX, because modality can slow down the train of thought. I
generally use vim or emacs with evil for programming where modality wins over.

It really took me 4 years to truly understand what emacs is all about. After
all, emacs is yet another text editor; that being said, it is more fun and extensible
than any other editor I had the chance to observe.

People that use other editors enjoy configuring them. Emacs, however, configures you.
