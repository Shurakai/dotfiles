#!/bin/zsh

typeset -A filenames

#array with filenames "key value key value" etc.
#key = filename in repo
#value = filename as seen from $HOME.

if [ ! -d $HOME/bin ]
  then
     mkdir $HOME/bin
fi

filenames=(.screenrc .screenrc .gitconfig .gitconfig .Xdefaults .Xdefaults .Xmodmap .Xmodmap "xmonad/bin/xmonad.start" "bin/xmonad.start" "xmonad/" ".xmonad" "xmonad/xmobarrc" ".xmobarrc" "zsh/zshrc" .zshrc)
for file in ${(k)filenames}
do
    if [ ! -e $HOME/$filenames[$file] ]
    then
        ln -s $PWD/$file $HOME/$filenames[$file]
	echo "$HOME/$filenames[$file] - created." 
    else
        echo "$HOME/$filenames[$file] exists - skipping."
    fi
done


