# MISCELLANEOUS SETTINGS

# Be paranoid, new files are readable/writable by me only.
# Make sure you know about the chpwd_umask function within this file!
umask 077

# Disable beeps.
setopt nobeep

# Prevent overwriting existing files with '> filename', use '>| filename'
# (or >!) instead.
setopt noclobber

# Entering the name of a directory (if it's not a command) will automatically
# cd to that directory.
setopt autocd

# When entering a nonexistent command name automatically try to find a similar
# one.
setopt correct
SPROMPT="zsh: correct %R to %r? ([Y]es / [N]o / [A]bort / [E]dit)"

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
# ~/.zsh/functions/completion is a symbolic link to the Completion directory
# of a Zsh CVS checkout. Use it to get the newest completions if available.
if [[ -d ~/.zsh/functions/completion ]]; then
    fpath=(~/.zsh/functions/completion/*/*(/) $fpath)
fi
# Set correct fpath to allow loading my functions (including completion
# functions).
fpath=(~/.zsh/functions $fpath)
# Autoload my functions (except completion functions and hidden files). Thanks
# to caphuso from the Zsh example files for this idea.
if [[ -d ~/.zsh/functions ]]; then
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

HISTSIZE=50000
SAVEHIST=50000
HISTFILE=~/.zsh/history

# Append to the history file instead of overwriting it and do it immediately
# when a command is executed.
setopt appendhistory
setopt incappendhistory

# If the same command is run multiple times store it only once in the history.
setopt histignoredups


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

zstyle ':completion:*' menu select

# Use cache to speed up completions.
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# Complete arguments and fix spelling mistakes when possible.
zstyle ':completion:*' completer _complete _match _correct _approximate
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'

# cd directory stack menu
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select

# Make sure the list of possible completions is displayed after pressing <TAB>
# the first time.
setopt nolistambiguous
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
# Display different types of matches separately.
zstyle ':completion:*' group-name ''

# Ignore completion functions.
zstyle ':completion:*:functions' ignored-patterns '_*'
# Ignore parent directory.
zstyle ':completion:*:(cd|mv|cp):*' ignore-parents parent pwd
# Always complete one value (file name) only once in the current line. This
# makes it easy to complete multiple values because I can just press tab to
# get all possible values. Otherwise I would have to skip the first value
# again and again.
zstyle ':completion:*' ignore-line yes
# Except for mv and cp, because I often want to use to similar names, so I
# complete to the same and change it.
zstyle ':completion:*:(mv|cp):*' ignore-line no

zstyle ':completion:*:processes' command ps --forest -A -o pid,cmd
zstyle ':completion:*:processes' list-colors '=(#b)( #[0-9]#)[^[/0-9a-zA-Z]#(*)=34=37;1=30;1'
#zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:(killall|pkill|kill):*' menu yes select
zstyle ':completion:*:(killall|pkill|kill):*' force-list always

# Complete the hosts and - last but not least - the remote directories. Try it:
# $ scp file username@<TAB><TAB>:/<TAB>
zstyle ':completion:*:(ssh|scp|ftp):*' hosts $hosts
zstyle ':completion:*:(ssh|scp|ftp):*' users $users

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
bindkey '^F' complete-files


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
exec 2>>(while read -r -k -u 0 line; do
    printf '\e[91m%s\e[0m' "$line";
    print -n $'\0';
done &)

# Allows us to use the directory stack ( google for it ) as a directory history
# We bind dh to dirs -v so we can list all the directories on the stack.
# Changing to a directory with number n can be achieved via cd -n
# Additionally we set the number of saved directories to 10
DIRSTACKSIZE=10
setopt autopushd pushdminus pushdsilent pushdtohome


# Display all branches (except stash) in gitk but only 200 commits as this is
# much faster. Also put in the background and disown. Thanks to sitaram in
# #git on Freenode (2009-04-20 15:51).
gitk() {
    command gitk \
        --max-count=200 \
        $(git rev-parse --symbolic-full-name --remotes --branches) \
        $@ &
    disown %command
}
# Same for tig (except the disown part as it's no GUI program).
tig() {
    command tig \
        --max-count=200 \
        $(git rev-parse --symbolic-full-name --remotes --branches) \
        $@
}

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

# Load configuration files for zsh
dotfiles=$HOME/.dotfiles
for zsh_file ($dotfiles/zsh/*.zsh) source $zsh_file

# vim: ft=zsh
