alias ls='ls --color=auto -h'
alias l='ls -AlF'
alias df='df -h'
alias mkdir='mkdir -p -v'
alias rm='rm -i'
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
alias top='htop'
alias grep='/bin/grep -i -n --color=auto'
export EDITOR=`which vim`

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

# TODO: Remove this alias; make it dynamic.
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
