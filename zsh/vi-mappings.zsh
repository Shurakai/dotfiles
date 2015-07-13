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
bindkey -M vicmd v edit-command-line # type v to edit in an external editor.

zle -N foreground-vi
bindkey '^Z' foreground-vi

# Remove all escapes from vi insert mode. Removes delay when pressing ESC to go
# back to vi cmd mode. (Though you should use 'jj' anyways) Source: The manual
# and the userguide: http://zsh.sourceforge.net/Guide/zshguide04.html#l75
# Section 4.5.2
#
# If this is used, the arrow keys won't work (most likely) in menu completion,
# so I don't use this any more. You can check if the keys will work or not by
# either trying or pressing ^V and then an arrow key; the sequence will be
# inserted, for me for instance ^[[D for the left-arrow key. Here, ^[ means
# "escape" (so this is the same as \e) and [D is taken to be literal: If you
# type ESCAPE [ D (without spaces) you will do the same as pressing the arrow
# left key.
#
# bindkey -rpM viins '\e'

# Magic-space executes history expansion and inserts a space
# afterwards. For instance,
# ~% !133<space> will replace !133 with command
# no 133 in your history, if <space> is bound to magic-space;
# otherwise, it will just insert a plain space.
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
bindkey 'jk' vi-cmd-mode

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
# order, this binding is for you - it transposes two words, delimited by blanks (and
# only blanks!)
# If you want to transpose words depending on your current cursor position, check out
# number 4.7.7 http://zsh.sourceforge.net/Guide/zshguide04.html#l75
bindkey -M vicmd ',tw' transpose-words

function custom-history-beginning-search-backward {
    zle history-beginning-search-backward

    # This check is necessary, as on my system calling
    # vi-cmd-mode when already in command-mode scrolls
    # up one line in the prompt, that is:
    #     % previous_line_of_code
    #     % <call vi-cmd-mode>
    # results in the removal of the first line.
    if [[ ${KEYMAP} == "viins" ]]; then
      zle vi-cmd-mode
    fi
}
zle -N custom-history-beginning-search-backward
bindkey -v '^P' custom-history-beginning-search-backward    # binding for vi insert mode
bindkey -a '^P' custom-history-beginning-search-backward # binding for vi cmd mode

bindkey '^N' history-beginning-search-forward
bindkey -a '^N' history-beginning-search-forward
bindkey -M 'viins' '^N' history-beginning-search-forward

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

cmdT() {                                                                                                                                                       -- NORMAL -- 16:53:06
   </dev/tty vim -c CommandT
}

zle -N cmdT
bindkey "^T" cmdT

# Make going up directories simple. rationalise-dot() {{{1
# This code comes directly from man 1 zsh-lovers
rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}
# 1}}}
zle -N rationalise-dot
bindkey -M viins '\.' rationalise-dot

bindkey '^F' complete-files
