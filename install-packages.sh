#!/bin/zsh

typeset -A packages
# suckless-tools = dmenu
# xloadimage = background-image and fill borders with background-color
# xscreensaver = lock workspace
packages=(xmonad 'xmonad xmobar suckless-tools trayer xloadimage xscreensaver' screen "screen" "rxvt-unicode-256color" "rxvt-unicode-256color" git "git" "evolution" "evolution" keepassx keepassx vim vim exuberant-ctags exuberant-ctags)

for package in ${(k)packages}
do
    # This actually does not work if package was installed before
    # and then removed... Should consider something else!
    dpkg -s $package > /dev/null &> /dev/null
    if [ $? -gt 0 ]
      then
        echo -n "$package is not installed. Do you want to install $package? [y/N]"
        read install

        if [ $install = "y" ]
          then
            # Install xmonad tiling window manager
            sudo apt-get install ${(z)packages[$package]}
          else
            echo -n "Skipping $package.\n"
        fi
    fi
    if [[ $? -eq 0 || $install -eq "y" ]]
      then
        # setup dotfiles here for $package ...
    fi
done
