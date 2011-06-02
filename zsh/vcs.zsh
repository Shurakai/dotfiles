# vcs_info was added in 4.3.9 but it works in earlier versions too. So load it
# if the necessary files are available in ~/.zsh/functions/vcs_info (often a
# symbolic link to current checkout of Zsh's sources).
if [[ $ZSH_VERSION == (4.3.<9->|4.<4->*|<5->*) ||
      -d ~/.zsh/functions/vcs_info ]]; then
    # Update fpath to allow loading the vcs_info functions.
    if [[ -d ~/.zsh/functions/vcs_info ]]; then
       fpath=(~/.zsh/functions/vcs_info/
              ~/.zsh/functions/vcs_info/Backends
              $fpath)
    fi

    # Load vcs_info to display information about version control repositories.
    autoload -Uz vcs_info
    # Only look for git and mercurial repositories; the only I use.
    zstyle ':vcs_info:*' enable git hg
    # Check the repository for changes so they can be used in %u/%c (see
    # below). This comes with a speed penalty for bigger repositories.
    zstyle ':vcs_info:*' check-for-changes true

    # Set style of vcs_info display. The current branch (green) and VCS (blue)
    # is displayed. If there is an special action going on (merge, rebase)
    # it's also displayed (red). Also display if there are unstaged or staged
    # (%u/%c) changes.
    if [[ $ZSH_VERSION == (4.3.<11->|4.<4->*|<5->*) ||
          -d ~/.zsh/functions/vcs_info ]]; then
        zstyle ':vcs_info:*' formats \
            "($green%b%u%c$default:$blue%s$default)"
        zstyle ':vcs_info:*' actionformats \
            "($green%b%u%c$default/$red%a$default:$blue%s$default)"
    else
        # In older versions %u and %c are not defined yet and are not
        # correctly expanded.
        zstyle ':vcs_info:*' formats \
            "($green%b$default:$blue%s$default)"
        zstyle ':vcs_info:*' actionformats \
            "($green%b$default/$red%a$default:$blue%s$default)"
    fi
    # Set style for formats/actionformats when unstaged (%u) and staged (%c)
    # changes are detected in the repository; check-for-changes must be set to
    # true for this to work. Thanks to Bart Trojanowski
    # (http://jukie.net/~bart/blog/pimping-out-zsh-prompt) for the idea
    # (2010-03-11 00:20).
    zstyle ':vcs_info:*' unstagedstr '¹'
    zstyle ':vcs_info:*' stagedstr   '²'

    # Default to running vcs_info. If possible we prevent running it later for
    # speed reasons. If set to a non empty value vcs_info is run.
    FORCE_RUN_VCS_INFO=1

    # Cache system inspired by Bart Trojanowski
    # (http://jukie.net/~bart/blog/pimping-out-zsh-prompt).
    #zstyle ':vcs_info:*+pre-get-data:*' hooks pre-get-data
    +vi-pre-get-data() {
        # Only Git and Mercurial support and need caching. Abort if any other
        # VCS is used.
        [[ "$vcs" != git && "$vcs" != hg ]] && return

        # If the shell just started up or we changed directories (or for other
        # custom reasons) we must run vcs_info.
        if [[ -n $FORCE_RUN_VCS_INFO ]]; then
            FORCE_RUN_VCS_INFO=
            return
        fi

        # Don't run vcs_info by default to speed up the shell.
        ret=1
        # If a git/hg command was run then run vcs_info as the status might
        # need to be updated.
        case "$(fc -ln $(($HISTCMD-1)))" in
            git* | g\ *)
                ret=0
                ;;
            hg*)
                ret=0
                ;;
        esac
    }

    # Must run vcs_info when changing directories.
    prompt_chpwd() {
        FORCE_RUN_VCS_INFO=1
    }
    add-zsh-hook chpwd prompt_chpwd

    # Used by prompt code below to determine if vcs_info should be run.
    RUN_VCS_INFO=1
else
    RUN_VCS_INFO=
fi
