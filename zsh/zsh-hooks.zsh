# Set the prompt. A two line prompt is used. On the top left the current
# working directory is displayed, on the right vcs_info (if available). On the
# bottom left current username and host is shown, the exit code of the last
# command if it wasn't 0, the number of running jobs if not 0.
#
# The prompt is in green and blue to make easily detectable, the error exit
# code in red and bold and the job count in yellow.
#
# Thanks to Adam's prompt for the basic idea of this prompt.
prompt_precmd() {
    # Regex to remove elements which take no space. Used to calculate the
    # width of the top prompt. Thanks to Bart's and Adam's prompt code in
    # Functions/Prompts/prompt_*_setup.
    local zero='%([BSUbfksu]|([FB]|){*})'

    # Call vcs_info before every prompt.
    if [[ -n $RUN_VCS_INFO ]]; then
        vcs_info
    else
        vcs_info_msg_0_=
    fi

    local width width_left width_right
    local top_left top_right

    # Display vcs_info (if used) on the right in the top prompt.
    top_right="${vcs_info_msg_0_}"
    width_right=${#${(S%%)top_right//$~zero/}}
    # Remove vcs_info if it would get too long.
    if [[ $(( COLUMNS - 4 - 1 - width_right )) -lt 0 ]]; then
        top_right=
        width_right=0
    fi

    # Display current directory on the left in the top prompt. Truncate the
    # directory if necessary.
    width=$(( COLUMNS - 4 - 1 - width_right ))
    top_left=".-$default%b($yellow%$width<..<%~%<<$default)%B$blue"

    # Calculate the width of the top prompt to fill the middle with "-".
    width_left=${#${(S%%)top_left//$~zero/}}
    width_right=${#${(S%%)top_right//$~zero/}}
    width=$(( COLUMNS - width_left - width_right ))

    PROMPT="$blue%B$top_left${(l:$width::-:)}%b$default$top_right
$blue%B'%b$default\
$green%B%n%b$default@$green%B%m%b$default %(1j.$yellow%j$default.)%# \
%(?..($red%B%?%b$default%) )"

}
add-zsh-hook precmd prompt_precmd

# Being paranoid and setting umask 077 is especially a good thing within the home directory,
# so nobody can read my files. On the other hand, there might
# be some places where we don't want umask 077 - we use the chpwd( ) function for this.
chpwd_umask() { # Taken from http://matt.blissett.me.uk/linux/zsh/zshrc
    case $PWD in
        ($HOME|$HOME/*)) # Everything in ~ should be private!
            if [[ $(umask) -ne 077 ]]; then
                umask 0077
                echo -e "\033[01;32mumask: private \033[m"
            fi;;
        *)
            if [[ $(umask) -ne 022 ]]; then
                umask 0022
                echo -e "\033[01;31mumask: world readable \033[m"
            fi;;
    esac
}
add-zsh-hook chpwd chpwd_umask
