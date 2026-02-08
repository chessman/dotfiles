set fish_greeting
set -gx PATH ~/bin $PATH
set -gx PATH ~/node_modules/.bin $PATH
set -gx PATH ~/.local/share/coursier/bin $PATH
set -gx PATH ~/.gem/ruby/2.7.0/bin $PATH
set -gx PATH ~/.local/bin $PATH
set -gx PATH ~/go/bin $PATH
set -gx PATH ~/.krew/bin $PATH
set -gx PATH ~/.cargo/bin $PATH
set -gx NODE_PATH ~/node/lib/node_modules $NODE_PATH

set -gx PAGER /usr/bin/ov

set -gx RIPGREP_CONFIG_PATH ~/.dotfiles/ripgreprc

starship init fish | source
zoxide init fish | source

alias ls exa
alias fd fdfind

eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
