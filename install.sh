#!/bin/zsh

typeset -A filenames

#array with filenames "key value key value" etc.
#key = filename in repo
#value = filename as seen from $HOME.

filenames=(.screenrc .screenrc .Xdefaults .Xdefaults .Xmodmap .Xmodmap "bin/xmonad.start" "bin/xmonad.start" "zsh/zshrc" .zshrc)
for file in ${(k)filenames}
do
    if [ ! -e $HOME/$filenames[$file] ]
    then
        ln -s $PWD/$file $HOME/$filenames[$file]
    else
        echo "$HOME/$filenames[$file] exists - skipping."
    fi
done


