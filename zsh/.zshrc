# MISCELLANEOUS SETTINGS

# Be paranoid, new files are readable/writable by me only.
# Make sure you know about the chpwd_umask function within this file!
umask 077

# Disable beeps.
setopt NO_BEEP

# Prevent overwriting existing files with '> filename', use '>| filename'
# (or >!) instead.
setopt noclobber

# Entering the name of a directory (if it's not a command) will automatically
# cd to that directory.
setopt AUTOCD

# Options for completion {{{1
# cf. Section 16.2.2 http://zsh.sourceforge.net/Doc/Release/Options.html#Options

# Default by ZSH: AUTO_LIST
# Show menu completion by default if choice is ambiguous
setopt AUTO_LIST

# Default by ZSH: NO_COMPLETE_ALIASES
# If this option is enabled, aliases will not have the same completion as
# the command they are referring to; hence, one can introduce one's own
# completion.
setopt NO_COMPLETE_ALIASES

# Completion can be called in the middle of a word rather than only at the end
setopt COMPLETE_IN_WORD

setopt NO_MENU_COMPLETE

# Always show completion menu if <tab> is hit twice
setopt AUTO_LIST

# Make sure the list of possible completions is displayed after pressing <TAB>
# the first time.
setopt nolistambiguous

# Forces Autocompletion to use columns of different width -> more compact
setopt LIST_PACKED

# }}}

# Options for expansion / globbing {{{1
# cf. Section 16.2.3 http://zsh.sourceforge.net/Doc/Release/Options.html#Options

setopt GLOB_DOTS

# }}}

# When entering a nonexistent command name automatically try to find a similar
# one.
setopt CORRECT
SPROMPT="zsh: correct %R to %r? ([Y]es / [N]o / [A]bort / [E]dit)"

setopt NO_CORRECT_ALL

# Enable zsh's extended glob abilities.
setopt extendedglob

# Enable patterns like {A-D} for "A B C D".
setopt braceccl

# Don't exit if <C-d> is pressed.
setopt ignoreeof

# Make variables, for example home-directory names or named directory (see hash -r)
# directly cd'able. (E.g., if you have set hash -r log=/var/log/, then you normally can cd into log
# via cd ~log. However, with this option set, cd log is okay, too)
setopt cdable_vars

# Run background processes at a lower priority. This is ZSH default, but I still mention it here.
setopt bgnice

# Setup right prompt to display history number because I juse
# bang completion quite frequently.
# Additionally, %* displays the time including seconds. If you don't like
# that, you can replace it with %t (12h format) or %T (24h format), both
# without seconds
# This is deprecated since this prompt will be setup with vi-mode details
# and the current time
# RPS1='%h at %*'

# KEY BINDINGS

# Not all bindings are done here, only those not specific to a given section.
# I don't need the arrow keys, I use ^N and ^P for this (see below).
bindkey -r '^[OA' '^[OB' '^[OC' '^[OD' '^[[A' '^[[B' '^[[C' '^[[D'
# Also not in Vi mode.
bindkey -a -r '^[OA' '^[OB' '^[OC' '^[OD' '^[[A' '^[[B' '^[[C' '^[[D'

# Allow backspacing over characters. ( Not allowed normally due to vi restrictions )
bindkey '^?' backward-delete-char

# FUNCTION SETTINGS

# Make sure every entry in $fpath is unique.
typeset -U fpath
fpath+=(~/.dotfiles/zsh/completions)
# Autoload my functions (except completion functions and hidden files). Thanks
# to caphuso from the Zsh example files for this idea.
if [[ -d ~/.zsh/functions ]]; then
  # Set correct fpath to allow loading my functions (including completion
  # functions).
  fpath=(~/.zsh/functions $fpath)

  # ~/.zsh/functions/completion is a symbolic link to the Completion directory
  # of a Zsh CVS checkout. Use it to get the newest completions if available.
  if [[ -d ~/.zsh/functions/completion ]]; then
      fpath=(~/.zsh/functions/completion/*/*(/) $fpath)
  fi

  autoload ${fpath[1]}/^_*(^/:t)

fi

# Simulate hooks using _functions arrays for Zsh versions older than 4.3.4. At
# the moment only precmd(), preexec() and chpwd() are simulated.
#
# At least 4.3.4 (not sure about later versions) has an error in add-zsh-hook
# so the compatibility version is used there too.
if [[ $ZSH_VERSION != (4.3.<5->|4.<4->*|<5->*) ]]; then
    # Provide add-zsh-hook which was added in 4.3.4.
    fpath=(~/.zsh/functions/compatibility $fpath)

    # Run all functions defined in the ${precmd,preexec,chpwd}_functions
    # arrays.
    function precmd() {
        for function in $precmd_functions; do
            $function $@
        done
    }
    function preexec() {
        for function in $preexec_functions; do
            $function $@
        done
    }
    function chpwd() {
        for function in $chpwd_functions; do
            $function $@
        done
    }
fi

# Autoload add-zsh-hook to add/remove zsh hook functions easily.
autoload -Uz add-zsh-hook

# Load zmv (zsh move) which is powerful to rename files.
autoload zmv


# HISTORY SETTINGS

# Use history and store it in ~/.zsh/history.
# HISTSIZE and SAVEHIST *must* be set for automatic reading
# and writing of history lines to work.
HISTSIZE=50000
SAVEHIST=50000
HISTFILE=~/.zsh/history

# Append to the history file instead of overwriting it and do it immediately
# when a command is executed.
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY

# Shares history between different shells running on the same host.
# This option actually sucks, at least for me. Remove the "NO" if you
# want to use this option.
setopt NO_SHARE_HISTORY

# This option enables logging of (i) execution date and (ii) the time it ran.
setopt EXTENDED_HISTORY

# Do not save commands that begin with a space
setopt HIST_IGNORE_SPACE

# If the same command is run multiple times (without any other command being
# run in the meantime), store it only once in the history.
setopt HIST_IGNORE_DUPS

# Prohibits any command to be listed twice in the whole history file (even
# if it is not the previous command, see HIST_IGNORE_DUPS)
setopt HIST_IGNORE_ALL_DUPS

# When history gets crammed, remove duplicates. Until then, do nothing
setopt HIST_EXPIRE_DUPS_FIRST

# If duplicate commands were saved, searches backward with editor commands won't
# find those commands more than once.
setopt HIST_FIND_NO_DUPS

# Expand substitutions while typing so you can see what you substituted :)
# (You can undo by exiting the insert mode and press "u" for undo! (vi-mode))
# See http://zsh.sourceforge.net/Guide/zshguide02.html#l6 2.5.3, "bang-history"
setopt HIST_VERIFY

# Remove blanks that mean nothing to the shell (ignores quoted blanks)
setopt HIST_REDUCE_BLANKS

# If you tried to redirect output to a file, say "/tmp/test", but that file existed
# you might get an error if you only used "> /tmp/test". With this option set, zsh
# will alter that command to ">| /tmp/test" so you can just go back to the previous
# command and execute it again if you like, without editing!
setopt HIST_ALLOW_CLOBBER

# Do not store "history" or "fc" commands.
setopt HIST_NO_STORE

# Disable mail checks
MAILCHECK=0

# PROMPT SETTINGS

# Use colorized output, necessary for prompts and completions.
autoload -U colors && colors
# Some shortcuts for colors. The %{...%} tells zsh that the data in between
# doesn't need any space, necessary for correct prompt draw.
local red="%{${fg[red]}%}"
local blue="%{${fg[blue]}%}"
local green="%{${fg[green]}%}"
local yellow="%{${fg[yellow]}%}"
local default="%{${fg[default]}%}"

# When screen, xterm or rxvt is used set the name of the window to the
# currently running program.
#
# When a program is started preexec() sets the window's name to it; when it
# stops precmd() resets the window's name to 'zsh'.
#
# It works with screen, xterm and rxvt.
#
# If a command is run with sudo or if the shell is running as root then a ! is
# added at the beginning of the command to make this clear. If a command is
# running on a different computer with ssh a @ is added at the beginning. If
# screen is running on the remote machine instead of @screen @:hostname
# (hostname replaced by the machine's hostname) is displayed. This only works
# if the .zshrc on the server also uses this command.
#
# screen* is necessary as `screen` uses screen.linux for example for a linux
# console.
if [[ $TERM == screen* || $TERM == xterm* || $TERM == rxvt* ]]; then
    # Is set to a non empty value to reset the window name in the next
    # precmd() call.
    window_reset=yes
    # Is set to a non empty value when the shell is running as root.
    if [[ $(id -u) -eq 0 ]]; then
        window_root=yes
    fi

    window_preexec() {
        # Get the program name with its arguments.
        local program_name=$1

        # When sudo is used use real program name instead, but with an
        # exclamation mark at the beginning (handled below).
        local program_sudo=
        if [[ $program_name == sudo* ]]; then
            program_name=${program_name#sudo }
            program_sudo=yes
        fi
        # Remove all arguments from the program name.
        program_name=${program_name%% *}

        # Ignore often used commands which are only running for a very short
        # time. This prevents a "blinking" name when it's changed to "cd" for
        # example and then some milliseconds later back to "zsh".
        [[ $program_name == (cd*|ls|la|ll|clear|c) ]] && return

        # Change my shortcuts so the real name of the program is displayed.
        case $program_name in
            e)
                program_name=elinks
                ;;
            g)
                program_name=git
                ;;
            m)
                program_name=mutt
                ;;
            v)
                program_name=vim
                ;;
        esac

        # Add an exclamation mark at the beginning if running with sudo or if
        # running zsh as root.
        if [[ -n $program_sudo || -n $window_root ]]; then
            program_name=!$program_name
        fi

        # Add an at mark at the beginning if running through ssh on a
        # different computer.
        if [[ -n $SSH_CONNECTION ]]; then
            program_name="@$program_name"

            # If screen is running in SSH then display "@:hostname" as title
            # in the term/outer screen.
            if [[ $program_name == @screen ]]; then
                program_name="@:${$(hostname)//.*/}"
            # Use "@:!hostname" for root screens.
            elif [[ $program_name == @!screen ]]; then
                program_name="@:!${$(hostname)//.*/}"
            fi
        fi

        # Set the window name to the currently running program.
        window_title "$program_name"

        # Tell precmd() to reset the window name when the program stops.
        window_reset=yes
    }

    window_precmd() {
        # Abort if no window name reset is necessary.
        [[ -z $window_reset ]] && return

        # Reset the window name to 'zsh'.
        local name=zsh
        # If the function was called with an argument then reset the window
        # name to '.zsh' (used by clear alias).
        if [[ -n $1 ]]; then
            name=.zsh
        fi

        # Prepend prefixes like in window_preexec().
        if [[ -n $window_root ]]; then
            name="!$name"
        fi
        if [[ -n $SSH_CONNECTION ]]; then
            name="@$name"
        fi
        window_title $name

        # Just reset the name, so no screen reset necessary for the moment.
        window_reset=
    }

    # Sets the window title. Works with screen, xterm and rxvt.
    if [[ $TERM == screen* ]]; then
        window_title() {
            print -n "\ek$1\e\\"
        }
    elif [[ $TERM == xterm* || $TERM == rxvt* ]]; then
        window_title() {
            print -n "\e]2;$1\e\\"
        }
    else
        # Fallback if another TERM is used.
        window_title() { }
    fi

    # Add the preexec() and precmd() hooks.
    add-zsh-hook preexec window_preexec
    add-zsh-hook precmd window_precmd
else
    # Fallback if another TERM is used, necessary to run screen (see below in
    # "RUN COMMANDS").
    window_preexec() { }
fi


# COMPLETION SETTINGS

# Load the complist module which provides additions to completion lists
# (coloring, scrollable).
zmodload zsh/complist
# Use new completion system, store dumpfile in ~/.zsh/cache to prevent
# cluttering of ~/. $fpath must be set before calling this. Thanks to Adlai in
# #zsh on Freenode (2009-08-07 21:05) for reminding me of the $fpath problem.
autoload -U compinit && compinit -d ~/.zsh/cache/zcompdump
# Use cache to speed up completions.
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache


# If you want menu selection (see user guide, 6.2.3) only with
# more than n available completions, change the select keyword
# at the end to select=n (and replace n with your number)
zstyle ':completion:*' menu select

# See the userguide, section 6.5.3, "Job control"
# Allows two background jobs "vim bla" "vim foo" to be completed, as they
# are now identified by %1 and %2 instead of %vim ... . This also offers
# menu selection. 
zstyle ':completion:*:*' numbers 1

# Complete arguments and fix spelling mistakes when possible.
# See also userguide 6.5.1 for details
zstyle ':completion:*' completer _complete _correct _approximate
#zstyle ':completion:*' completer _complete _match _approximate
# insert-unambigous?
zstyle ':completion:*:*:*' original only

# if files "foo" and "foobar" exist, should
# vi foo<tab> immediately accept foo or should
# foobar be offered as well? I think the latter is more suitable
# to my workflow. This is the default value, but I want to set it explicitly.
# Also see the REC_EXACT option
zstyle ':completion:*' accept-exact false

# This separates the folders from the filenames when completing them;
# folders are displayed at the top, files at the bottom
zstyle ':completion:*' list-dirs-first true

# Allow only one error in 3 characters
# This means also that you must have at least 3 characters typed before
# ZSH corrects your first spelling mistake
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'

# cd directory stack menu
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select

# Allow completions in the middle of a text, i.e. "/usr/bin/<TAB>whatever"
# completes like "/usr/bin/<TAB>". Useful when adding new options to commands.
bindkey '^I' expand-or-complete-prefix

# Try uppercase if the currently typed string doesn't match. This allows
# typing in lowercase most of the time and completion fixes the case.
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}'

# Use ls like colors for completions.
#zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors  'reply=( "=(#b)(*$PREFIX)(?)*=00=$color[green]=$color[bg-green]" )'

# Make completion lists scrollable so "do you wish to see all n possibilities"
# is no longer displayed.
zstyle ':completion:*' list-prompt '%p'
# Display group name (like 'external command', 'alias', etc.) when there are
# multiple matches in bold.
zstyle ':completion:*' format '    %B%d%b:'
# Display different types of matches separately; this makes the 'tags' of
# matches also the 'group' they're displayed in, like "builtins", "commands"
# etc. - check zshcompsys(1) (keyword: "group-name")
zstyle ':completion:*' group-name ''

# Ignore completion functions.
zstyle ':completion:*:functions' ignored-patterns '_*'

# Ignore current directory when completing from parent directory
# i.e., cd ../<tab> should not offer the current directory as an
# option.
zstyle ':completion:*:(cd|mv|cp|ln):*' ignore-parents parent pwd

# Always complete one value (file name) only once in the current line. This
# makes it easy to complete multiple values because I can just press tab to
# get all possible values. Otherwise I would have to skip the first value
# again and again.
#
# The value "other" here is important. It means that the word the cursor
# is currently on does not count as "already present on current line". This
# is necessary, as otherwise exact matches could not be completed!!
#
# FIXME Quote from the manual (search for "ignore-line"): 
#   Note that you almost certainly don't want to set this to `true' or `other' 
#   for a general context such as `:completion:*'. This is because it would 
#   disallow completion of, for example, options multiple times even if the 
#   command in question accepts the option more than once.
zstyle ':completion:*:(*files|*directories)' ignore-line other

# FIXME: I'm not sure if I still need this?
zstyle ':completion:*:processes' command ps --forest -A -o pid,cmd

# {{{2 kill | killall etc. 
zstyle ':completion::*:(kill|wait):*:processes' format "%B PID | CPU | CMD %b"
# -U $USER makes sure only processes by this user are displayed
zstyle ':completion::*:(kill|wait):*:processes' command 'ps xf -U $USER -o pid,%cpu,cmd'
# To change the color to a grey tone, use '=(#b)( #[0-9]#)[^[/0-9a-zA-Z]#(*)=0=01;32=30;1'
zstyle ':completion::*:(kill|wait):*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'
#Explicitly use the "verbose" style: This makes sure that not only PIDs are
#displayed, but also commands incl. arguments.
zstyle ':completion:*:(killall|pkill|kill|wait):*' verbose yes 
zstyle ':completion:*:(killall|pkill|kill|wait):*' menu yes select
zstyle ':completion:*:(killall|pkill|kill|wait):*' force-list always
# }}}

# Complete the hosts and - last but not least - the remote directories. Try it:
# $ scp file username@<TAB><TAB>:/<TAB>
#zstyle ':completion:*:(ssh|scp|ftp):*' hosts $hosts
#zstyle ':completion:*:(ssh|scp|ftp):*' users $users

#$ man write<TAB>
# manual page, section 1:
# write
# manual page, section 1p:
# write
# ...
# Its evil. Isn't it?!
# From http://www.strcat.de/dotfiles/dot.zshstyle
#
# If you are not familiar with the different manpage sections
# (and therefore, the reason for using this directive), read
# http://en.wikipedia.org/wiki/Man_page (Section: Usage)
zstyle ':completion:*:manuals' separate-sections true

# Provide a fallback completer which always completes files. Useful when Zsh's
# completion is too "smart". Thanks to Frank Terbeck <ft@bewatermyfriend.org>
# (http://www.zsh.org/mla/users/2009/msg01038.html).
zle -C complete-files complete-word _generic
zstyle ':completion:complete-files:*' completer _files


# Ignore filetypes when completing using the vi or vim commands.
# (Very helpful when you want to open a file XYZ.tex and all the other
# files were already automatically generated, like XYZ.aux etc. -- these
# files will not be offered by completion, just XYZ.tex!)
zstyle ':completion:*:*:vi:*:*files' ignored-patterns '*?.aux' '*?.bbl' '*?.blg' '*?.dvi' '*?.out' '*?.idx' '*?.idl' '*?.toc' '*?.snm' '*?.nav' '*?.pdf' '*?.bak' '*\~'

zstyle ':completion:*:*:vi:*:*files' _files -g "*.(#i)(pdf|ps|eps|djvu)(-.)"

# CUSTOM ALIASES AND FUNCTIONS

# If ^C is pressed while typing a command, add it to the history so it can be
# easily retrieved later and then abort like ^C normally does. This is useful
# when I want to abort an command to do something in between and then finish
# typing the command.
#
# Thanks to Vadim Zeitlin <vz-zsh@zeitlins.org> for a fix (--) so lines
# starting with - don't cause errors; and to Nadav Har'El
# <nyh@math.technion.ac.il> for a fix (-r) to handle whitespace/quotes
# correctly, both on the Zsh mailing list.
TRAPINT() {
    # Store the current buffer in the history.
    zle && print -s -r -- $BUFFER

    # Return the default exit code so Zsh aborts the current command.
    return $1
}

# Colorize stderr in red. Very useful when looking for errors. Thanks to
# http://gentoo-wiki.com/wiki/Zsh for the basic script and Mikachu in #zsh on
# Freenode (2010-03-07 04:03) for some improvements (-r, printf). It's not yet
# perfect and doesn't work with su and git for example, but it can handle most
# interactive output quite well (even with no trailing new line) and in cases
# it doesn't work, the E alias can be used as workaround.
#exec 2>>(while read -r -k -u 0 line; do
#    printf '\e[91m%s\e[0m' "$line";
#    print -n $'\0';
#done &)

# Allows us to use the directory stack ( google for it ) as a directory history
# We bind dh to dirs -v so we can list all the directories on the stack.
# Changing to a directory with number n can be achieved via cd -n
# Additionally we set the number of saved directories to 10
DIRSTACKSIZE=10
setopt autopushd pushdminus pushdsilent pushdtohome
setopt pushd_ignore_dups


# RUN COMMANDS

# If not already in screen reattach to a running session or create a new one.
# This also starts screen one a remote server when connecting through ssh.
if [[ $TERM != dumb && -z $STY ]]; then
    # Get running detached sessions.
    session=$(screen -list | grep 'Detached' | awk '{ print $1; exit }')

    # As we exec later we have to set the title here.
    window_preexec "screen"

    # Create a new session if none is running.
    if [[ -z $session ]]; then
        exec screen
    # Reattach to a running session.
    else
        exec screen -r $session
    fi
fi

config_dir=`dirname $(readlink -f $HOME/.zshrc)`

# Load configuration files for zsh
for zsh_file ($config_dir/*.zsh) source $zsh_file

# The following libraries are from oh-my-zsh
source $config_dir/lib/git.zsh

# Load theme
DEFAULT_USER="heinrich"
source $config_dir/themes/agnoster-fcamblor.zsh

# Prompt gets updated before every command to make sure
# we have an up-to-date prompt
prompt_precmd() {
PROMPT="%{%f%b%k%}$(build_prompt)
%h %% "
}
add-zsh-hook precmd prompt_precmd

setup_right_prompt() {
    # The following two lines make sure that deleting a text object
    # does not move the prompt up a line. see
    # https://github.com/hchbaw/opp.zsh/issues/1
    setopt localoptions no_ksharrays
    [[ "${@[2]-}" == opp ]] && return

    # Define right prompt.
    # Personally, I like to see the VI mode I'm in explicitly, but it's also
    # possible to change the cursor color. See for a working example here (but
    # make sure that you use the last one - the first one will fail with GNU screen!)
    # http://andreasbwagner.tumblr.com/post/804629866/zsh-cursor-color-vi-mode
    RPS1=$(display_vi_mode)
    RPS1+=" "%*
    zle reset-prompt
}
zle -N zle-keymap-select setup_right_prompt
zle -N zle-line-init     setup_right_prompt



# This was installed by the package command-not-found on Debian / Ubuntu
# (installed by default). If a command was not found, it proposes the package
# names that this command is contained in.
if [[ -r /etc/zsh_command_not_found ]]; then
source /etc/zsh_command_not_found
fi

if [[ -r .zshrc.local ]]; then
source .zshrc.local
fi

# vim: ft=zsh
