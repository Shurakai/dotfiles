#!/usr/bin/env zsh
# On login, we need to autostart some programs...

# Load background image
#xloadimage -onroot -border black -fullscreen /home/heinrich/Pictures/wallpapers/phi.jpg
xloadimage -onroot -border black -fullscreen /home/heinrich/Pictures/wallpapers/03974_morningstarpeak_3200x1800.jpg

# Load evolution-notification program - will notify of appointments, events
/usr/lib/evolution/evolution-alarm-notify &

# Set up an icon tray

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype percent --width 8 --tint black --heighttype pixel --height 19 --transparent true --alpha 0 &

nm-applet --sm-disable &

gnome-sound-applet &

#xscreensaver -no-splash &
gnome-screensaver &

xmodmap ~/.Xmodmap

exec xmonad
