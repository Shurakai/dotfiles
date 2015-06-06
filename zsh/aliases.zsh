alias ls='ls --color=auto -h'
alias l='ls -Al'
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

# apt-get shortcuts;
# only for Ubuntu or Debian
LSB_DISTRIBUTOR=`lsb_release -i -s`

if [[ "$LSB_DISTRIBUTOR" == "Ubuntu" ]] || [[ "$LSB_DISTRIBUTOR" == "Debian" ]]
then
  alias agi='sudo apt-get install'
  alias agu='sudo apt-get update && sudo apt-get upgrade'
  alias acse='apt-cache search'
  alias acsh='apt-cache show'
fi

[[ -x $(which colordiff) ]] && alias diff="colordiff -u" || alias diff="diff -u"

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

# Make sure aliases are expanded when using sudo.
alias sudo='sudo '
