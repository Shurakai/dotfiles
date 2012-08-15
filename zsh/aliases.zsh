alias ls='ls --color=auto -h'
alias l='ls -al'
alias df='df -h'
alias mkdir='mkdir -p -v'
alias rm='rm -i'
alias which='type -p'
alias du='du -h'
alias dh='dirs -v' # Shows nice directory history. See autopushd!
alias top='htop'
alias grep='/bin/grep -i -n --color=always'
export EDITOR=/usr/bin/vim
export CFLAGS=' -O2 -mtune=core2 -march=core2 -D_SMP'
export CONCURRENCY_LEVEL=4

# apt-get shortcuts
alias agi='sudo apt-get install'
alias agu='sudo apt-get update && sudo apt-get upgrade'
alias acse='apt-cache search'
alias acsh='apt-cache show'

alias be=~/bin/be # BugsEverywhere bugtracker
alias lessc=/var/lib/gems/1.8/bin/lessc

# Global aliases for often used commands in the command line.
alias -g E='2>&1'
alias -g L='E | less'
alias -g D='E | colordiff L'
alias -g G='| grep'
alias -g S='| sort'
alias -g U='| uniq'
alias -g H='| head'
alias -g T='| tail'

# Make going up directories simple.
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../

# Make sure aliases are expanded when using sudo.
alias sudo='sudo '

# If the window naming feature is used (see above) then use ".zsh" (leading
# dot) as title name after running clear so it's clear to me that the window
# is empty. I open so much windows that I don't know in which I have something
# important. This helps me to remember which windows are empty (I run clear
# after I finished my work in a window).
if [[ -n $window_reset ]]; then
    alias clear='clear; window_reset=yes; window_precmd reset'
fi
