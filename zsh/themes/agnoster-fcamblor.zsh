#
# agnoster's Theme - https://gist.github.com/3712874
# A Powerline-inspired theme for ZSH
#
# # README
#
# In order for this theme to render correctly, you will need a
# [Powerline-patched font](https://gist.github.com/1595572).
#
# In addition, I recommend the
# [Solarized theme](https://github.com/altercation/solarized/) and, if you're
# using it on Mac OS X, [iTerm 2](http://www.iterm2.com/) over Terminal.app -
# it has significantly better color fidelity.
#
# # Goals
#
# The aim of this theme is to only show you *relevant* information. Like most
# prompts, it will only show git information when in a git working directory.
# However, it goes a step further: everything from the current user and
# hostname to whether the last call exited with an error to whether background
# jobs are running in this shell will all be displayed automatically when
# appropriate.

### Segment drawing
# A few utility functions to make it easy and re-usable to draw segmented prompts

CURRENT_BG='NONE'
SEGMENT_SEPARATOR=''
SHOW_STASH_SEGMENT=1

# Begin a segment
# Takes two arguments, background and foreground. Both can be omitted,
# rendering default background/foreground.
prompt_segment() {
  local bg fg
  [[ -n $1 ]] && bg="%K{$1}" || bg="%k"
  [[ -n $2 ]] && fg="%F{$2}" || fg="%f"
  if [[ $CURRENT_BG != 'NONE' && $1 != $CURRENT_BG ]]; then
    echo -n " %{$bg%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR%{$fg%} "
  else
    echo -n "%{$bg%}%{$fg%} "
  fi
  CURRENT_BG=$1
  [[ -n $3 ]] && echo -n $3
}

# End the prompt, closing any open segments
prompt_end() {
  if [[ -n $CURRENT_BG ]]; then
    echo -n " %{%k%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR"
  else
    echo -n "%{%k%}"
  fi
  echo -n "%{%f%}"
  CURRENT_BG=''
}

### Prompt components
# Each component will draw itself, and hide itself if no information needs to be shown

# Context: user@hostname (who am I and where am I)
prompt_context() {
  local user=`whoami`

  if [[ "$user" != "$DEFAULT_USER" || -n "$SSH_CLIENT" ]]; then
    prompt_segment black white "%(!.%{%F{yellow}%}.)$user |%*| $(prompt_ram)G"
  fi
}

# Git: branch/detached head, dirty status
prompt_git() {
  local ref dirty
  if $(git rev-parse --is-inside-work-tree >/dev/null 2>&1); then
    dirty=$(parse_git_dirty)

    if [[ $SHOW_STASH_SEGMENT -eq 1 ]]; then
        stash_size=$(git stash list | wc -l | tr -d ' ')
        if [[ stash_size -ne 0 ]]; then
            prompt_segment yellow black
            echo -n "+${stash_size}"
        fi
    fi

    ref=$(git symbolic-ref HEAD 2> /dev/null) || ""
    if [[ -z $ref ]]; then
      detached_head=true;
      ref="$(git show-ref --head -s --abbrev |head -n1 2> /dev/null)";
      ref_symbol="➦"
    else
      detached_head=false;
      ref=${ref/refs\/heads\//}
      ref_symbol=""
    fi

    new_from_upstream=0;
    upstream="";
    # If in detached head, there should never be any upstream
    if [ "$detached_head" = true ]; then
      upstream="";
      new_from_local="";
    else
      # On a branch ... looking for upstream branch and, if available, calculating
      # commits deltas

      # git branch -vv potential outputs :
      # `* master e4b49d0 ...` => Just after create and checkout new local branch. Should be considered untracked
      # `* master e4b49d0 [origin/master: gone] ...` => Untracked branch. Should be considered untracked
      # `* master e4b49d0 [origin/master] ...` => Tracked branch without diff
      # `* master e4b49d0 [origin/master: ahead 1, behind 1] ...` => Tracked branch with diffs
      if [[ $(git branch -vv | grep "* $ref .* \\[" | wc -l) -eq 0 ]]; then
          # First case or case where branch doesn't exist
          # We should consider this as an untracked branch
          upstream_status="gone"
      else
          upstream_infos=$(git branch -vv | grep "* $ref" | sed 's@.*\[\(.*\)\].*@\1@g')
          upstream=$(echo $upstream_infos | cut -d':' -f1)
          if [[ $(echo $upstream_infos | grep ':' | wc -l) -eq 0 ]]; then
              upstream_status="synced"
          else
              upstream_status=$(echo $upstream_infos | cut -d':' -f2 | tr -d ' ')
          fi
      fi

      if [ "$upstream_status" = "gone" ]; then
        # Case for untracked branch
        new_from_local="";
        upstream=""
      else
        # Tracked branch : calculating commits deltas
        new_from_local=" (+$(git cherry $upstream $ref | wc -l | tr -d ' '))";
        new_from_upstream=$(git cherry $ref $upstream | wc -l | tr -d ' ')
      fi
    fi

    if [[ -n $dirty ]]; then
      prompt_segment yellow black
    else
      prompt_segment green black
    fi

    echo -n "${ref_symbol} ${ref}${new_from_local}"

    setopt promptsubst
    autoload -Uz vcs_info

    zstyle ':vcs_info:*' enable git
    zstyle ':vcs_info:*' get-revision true
    zstyle ':vcs_info:*' check-for-changes true
    zstyle ':vcs_info:*' stagedstr '✚'
    zstyle ':vcs_info:git:*' unstagedstr '●'
    zstyle ':vcs_info:*' formats ' %u%c'
    zstyle ':vcs_info:*' actionformats '%u%c'
    vcs_info
    echo -n "${vcs_info_msg_0_}"

    # Displaying upstream dedicated segment
    if [[ -n $upstream ]]; then
      if [ $new_from_upstream -ne 0 ]; then
        prompt_segment magenta white
      else
        prompt_segment cyan black
      fi
      echo -n " $upstream (-$new_from_upstream)"
    fi
  fi
}


# Dir: current working directory
prompt_dir() {
  prompt_segment blue white '%~'
}

# Virtualenv: current working virtualenv
prompt_virtualenv() {
  local virtualenv_path="$VIRTUAL_ENV"
  if [[ -n $virtualenv_path ]]; then
    prompt_segment blue black "(`basename $virtualenv_path`)"
  fi
}

# Status:
# - was there an error
# - am I root
# - are there background jobs?
prompt_status() {
  local symbols
  symbols=()
  [[ $RETVAL -ne 0 ]] && symbols+="%{%F{red}%}✘ ($RETVAL)"
  [[ $UID -eq 0 ]] && symbols+="%{%F{yellow}%}⚡"
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}⚙"

  [[ -n "$symbols" ]] && prompt_segment black default "$symbols"
}

function prompt_ram {
  free -m | awk '{if (NR==2) print $4}' | xargs -i echo 'scale=4;{}/1000' | bc | xargs -i printf "%.2f" {}
}

prompt_next_line() {
  prompt_segment default yellow "%c>"
  echo -n "%{%f%}"
}


## Main prompt
build_prompt() {
  RETVAL=$?
  prompt_status
  prompt_virtualenv
  prompt_context
  prompt_dir
  prompt_git
  prompt_end
}
