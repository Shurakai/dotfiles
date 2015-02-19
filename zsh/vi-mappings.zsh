#!/usr/bin/env zsh
# -*- mode: zsh; sh-indentation: 2; indent-tabs-mode: nil; sh-basic-offset: 2; -*-
# vim: ft=zsh sw=2 ts=2 et
#
# There are my zsh vi mappings, optimized for the colemak keyboard layout

# Allow command line editing in an external editor.
autoload -Uz edit-command-line

# If I am using vi keys, I want to know what mode I'm currently using.
# From http://zshwiki.org/home/examples/zlewidgets
function display_vi_mode() {
  print -- "${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/-- INSERT --}"
}
foreground-vi() {
  fg %vi
}

zle -N edit-command-line

# Avoid binding ^J, ^M,  ^C, ^?, ^S, ^Q, etc.
bindkey -d # Reset to default.
bindkey -v # Use vi key bindings.
bindkey -M vicmd v edit-command-line # v to edit in an external editor.

zle -N foreground-vi
bindkey '^Z' foreground-vi

# Remove all escapes from vi insert mode. Removes delay when pressing ESC
# to go back to vi cmd mode. (Though you should use 'jj' anyways)
# Source: The manual.
bindkey -rpM viins '\e'

# Vi mappings adapted to colemak layout
bindkey ' ' magic-space
bindkey -M vicmd "gg" beginning-of-history
bindkey -M vicmd "G" end-of-history
#bindkey -M vicmd "N" history-search-backward
#bindkey -M vicmd "n" history-search-forward
bindkey -M vicmd "?" history-incremental-search-backward
bindkey -M vicmd "/" history-incremental-search-forward
bindkey -M viins "^L" clear-screen
bindkey -M viins "^W" backward-kill-word
bindkey -M viins "^A" beginning-of-line
bindkey -M viins "^E" end-of-line
bindkey -M viins "^?" backward-delete-char

# Also use jj to insert vim mode and exit insert mode.
bindkey 'jj' vi-cmd-mode

# Inserts / removes a leading "#".
bindkey -M vicmd ',c ' vi-pound-insert

# Buffer stack bindings.
# See http://zsh.sourceforge.net/Guide/zshguide04.html#l75 section 4.6.3
# and http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Zsh-Line-Editor
# (get-line, push-line, push-line-or-edit)
bindkey -M vicmd ',gl' get-line
bindkey -M vicmd ',pl' push-line-or-edit
bindkey -M vicmd ',psl' push-line

# If you're like me and you sometimes forget to write words or arguments in the correct
# order, this binding is for you - it transposes to words, delimited by blanks (and
# only blanks!)
# If you want to transpose words depending on your current cursor position, check out
# number 4.7.7 http://zsh.sourceforge.net/Guide/zshguide04.html#l75
bindkey -M vicmd ',tw' transpose-words

bindkey -M vicmd ',dw' delete-word

# Vim like completions of previously executed commands (also enter Vi-mode). If
# called at the beginning it just recalls old commands (like cursor up), if
# called after typing something, only lines starting with the typed are
# returned. Very useful to get old commands quickly. Thanks to Mikachu in #zsh
# on Freenode (2010-01-17 12:47) for the information how to a use function
# with bindkey.
zle -N my-vi-history-beginning-search-backward

my-vi-history-beginning-search-backward() {
    local not_at_beginning_of_line
    if [[ $CURSOR -ne 0 ]]; then
        not_at_beginning_of_line=yes
    fi

    zle history-beginning-search-backward

    # Start Vi-mode and stay at the same position (Vi-mode moves one left,
    # this counters it).
    zle vi-cmd-mode
    if [[ -n $not_at_beginning_of_line ]]; then
        zle vi-forward-char
    fi
}
bindkey '^P' my-vi-history-beginning-search-backward
bindkey -a '^P' history-beginning-search-backward # binding for Vi-mode

# Here only Vi-mode is necessary as ^P enters Vi-mode and ^N only makes sense
# after calling ^P.
bindkey -a '^N' history-beginning-search-forward

# Searches should be repeatable, even incremental searches
# This doesn't really work right now, but it might be a good
# starting point.
# See http://www.zsh.org/mla/users/2002/msg00626.html
# for source.
function vi-repeat-incremental-search-backward {
  bindkey -e
  zle -U $'\C-r'
  zle history-incremental-search-backward
  bindkey -v
}
zle -N vi-repeat-incremental-search-backward
bindkey -a N vi-repeat-incremental-search-backward
