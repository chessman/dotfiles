set fish_greeting
set -gx PATH ~/bin $PATH
set -gx PATH ~/node/bin $PATH
set -gx PATH ~/.local/share/coursier/bin $PATH
set -gx PATH ~/.gem/ruby/2.7.0/bin $PATH
set -gx PATH ~/.local/bin $PATH
set -gx NODE_PATH ~/node/lib/node_modules $NODE_PATH

set -gx PAGER /home/ea/bin/slit

set -gx RIPGREP_CONFIG_PATH /home/ea/.dotfiles/ripgreprc

starship init fish | source

alias ls exa
