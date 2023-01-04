# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/seanld/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git z bgnotify)


bgnotify_threshold=10  # Set long-running-command-finished notification threshold (seconds).

function notify_formatted {
    # $1=exit_status, $2=command, $3=elapsed_time
    [ $1 -eq 0 ] && title="Completed successfully" || title="Failed"
    bgnotify "$title, after $3 s" "$2";
}


source $ZSH/oh-my-zsh.sh

export PATH=$PATH:/home/seanld/repos/lua-language-server/bin/Linux
export PATH=$PATH:/home/seanld/.emacs.d/bin # Doom Emacs command

# You may need to manually set your language environment
export LANG=en_US.UTF-8
export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"

# Shorten the Arch package manager commands, as I type them a lot, and
# it's not as ergonomic to type fully as `sudo apt install ...` was.
alias pi="doas pacman -S"
alias pir="doas pacman -R"
alias update="doas pacman -Syu"
alias au="sudo aura"

# Shortcut to hexedit (with option for colored bytes).
alias hx="hexedit"
alias hxc="hexedit --color"

# Shortcut to terminal text editor.
alias m="micro"

# Quicker attaching in Tmux (may be a better way -- still a noob).
alias ta="tmux attach-session -t"

# Make copying easier.
alias c="xclip -selection clipboard"

# Quickly go back to the previous CWD.
alias back="cd - > /dev/null"

# Bandaid-fix launcher for DBeaver because it's weird.
alias dbeaver="/usr/share/dbeaver/dbeaver"

# Launches Emacs Calc-mode in a full separate window, or in the terminal.
alias calc-mode="emacs -f full-calc"
alias calc-term="emacs -nw -f full-calc"

# Shortcuts for launching a client of a running Emacs daemon in a new
# frame , but using Spacemacs naming, since that's the flavor I use.
alias ec="emacsclient"

# Shortcut for arduino-cli.
alias ard="arduino-cli"

# Ensure that every time I use the `screen` command, it starts the
# 256 color mode, because muh color.
alias screen="screen -T screen-256color"

# Shorten the Git clone command. "qc" is for "quick-clone", because
# "gc" is already a commonly-installed command name.
alias qc="git clone ";

# Add Go stuff to make it function properly.
export GOPATH="$(go env GOPATH)"
export PATH="${PATH}:${GOPATH}/bin"

alias luamake=/home/seanld/repos/lua-language-server/3rd/luamake/luamake

# bun completions
[ -s "/home/seanld/.bun/_bun" ] && source "/home/seanld/.bun/_bun"

# Bun
export BUN_INSTALL="/home/seanld/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# Nimble executable path
export PATH="/home/seanld/.nimble/bin:$PATH"

# Rust executable path
export PATH="/home/seanld/.cargo/bin:$PATH"

# My systemd user-level services are saying "failed to connect to bus"
# and this fixes it. No clue why yet.
export XDG_RUNTIME_DIR=/run/user/$(id -u)
