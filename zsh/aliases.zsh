alias ls='ls --color=auto -h'
alias l='ls -AlF'
alias df='df -h'
alias mkdir='mkdir -p -v'
alias rm='rm -i --preserve-root'
alias evince='zathura'

# Add the key to ssh-add if not already added; then just use the plain ssh
# Make sure that $? is escaped as "\$?", because otherwise this will be executed
# when this script is loaded, not when the alias is executed
# $? -gt 0 means: If the grep before returned an error code (because it didn't
# find our username), then add the key.
alias ssh="ssh-add -L G $USERNAME >/dev/null; [[ \$? -gt 0 ]] && ssh-add; ssh"

# Removed this alias, because "type -p" will have the following
# output:
#       % which vim
#       vim is /usr/local/bin/vim
# This output is not suitable for further use.
#alias which='type -p'
alias du='du -h -d 1'
alias dh='dirs -v' # Shows nice directory history. See autopushd!

$(which htop &> /dev/null)
if [[ $? -eq 0 ]]; then
  alias top='htop'
fi

alias grep='/bin/grep -i -n --color=auto'
export EDITOR=$(which vim)

# apt-get shortcuts;
# only for Ubuntu or Debian
# ... lsb_release not installed by default on debian!
#LSB_DISTRIBUTOR=`lsb_release -i -s`
IS_DEBIAN=1 # Note: This is not boolean, but an exit code. 
            # Any value larger 0 disables these features! 
            
if [[ -f /etc/os-release ]]; then
  $(grep -i "ubuntu\|debian" /etc/os-release > /dev/null)
  IS_DEBIAN=$?
fi

if [[ IS_DEBIAN -eq 0 ]]
then
  alias agi='sudo apt-get install'
  alias agu='sudo apt-get update && sudo apt-get upgrade'
  alias agdu='sudo apt-get update && sudo apt-get dist-upgrade'
  alias acse='apt-cache search'
  alias acsh='apt-cache show'
  alias acp='apt-cache policy'
fi

$(which colordiff &> /dev/null)
if [[ $? -eq 0 ]]; then
  alias diff='colordiff -u'
else
  alias diff='diff -u'
fi

# Global aliases for often used commands in the command line.
alias -g E='2>&1'
alias -g L='E | less'
alias -g D='E | diff L'
alias -g G='| grep'
alias -g S='| sort'
alias -g U='| uniq'
alias -g H='| head'
alias -g T='| tail'

# Make sure aliases are expanded when using sudo.
alias sudo='sudo '
