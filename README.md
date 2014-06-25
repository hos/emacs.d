# nrs's emacs configuration

This is my everchanging emacs configuration. I try to look into this
configuration every now and then for micro optimizations that will make you
doubt my sanity. Don't worry, it's just a hobby.


The configuration itself is supposed to be self documenting. Looking into
`keybindings.el` and `enhancements.el` will reveal my text editing habits. 

#### Downloading and Installation

    git clone --recursive https://github.com/nrs/emacs.d
    mv emacs.d ~/.emacs.d

*Of course, be careful not to clobber your own config.*

##### Updating

    git pull --recurse-submodules

#### How I rediscovered emacs

I used emacs for two years immediately after I started programming, quite
inefficiently. I then discovered vim, which charmed me with its modal
capabilities. vim was so fast and efficient that I lost faith in emacs, thinking
that the problem was in the editor rather than my configuration.

I used vim for another two years, until I discovered @magnars's *emacsrocks*
series. I realized the problem was with my configuration, and decided to create
this repository. 

I now cherish both of them, and use either in different contexts. I use emacs while typing
plain text or TeX, because modality can slow down the train of thought. I
generally use vim or emacs with evil for programming where modality wins over.
