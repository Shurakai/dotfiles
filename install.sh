#!/bin/zsh

typeset -A filenames

#array with filenames "key value key value" etc.
#key = filename in repo
#value = filename as seen from $HOME.

if [ ! -d $HOME/bin ]
  then
     mkdir $HOME/bin
fi

filenames=("cgvg/cgvgrc" .cgvgrc gpg.conf ../.gnupg/gpg.conf "pentadactyl/pentadactylrc" .pentadactylrc "screen/screenrc" .screenrc "git/gitconfig" .gitconfig "most/.mostrc" ".mostrc" "X11/Xresources" .Xresources "X11/Xmodmap" .Xmodmap "xmonad/xsession" ".xsession" "xmonad/" ".xmonad" "xmonad/xmobarrc" ".xmobarrc" "zsh/zshrc" .zshrc "zsh/zshenv" .zshenv)
for file in ${(k)filenames}
do
    if [[ ! -e $HOME/$filenames[$file] ]]
    then
        ln -s $PWD/$file $HOME/$filenames[$file]
	echo "$HOME/$filenames[$file] - created."
    else
        echo "$HOME/$filenames[$file] exists - skipping."
    fi
done


