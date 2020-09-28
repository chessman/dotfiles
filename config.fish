set fish_greeting
set -gx PATH ~/bin $PATH
set -gx PATH ~/node/bin $PATH
set -gx PATH ~/.local/share/coursier/bin $PATH
set -gx PATH ~/.gem/ruby/2.7.0/bin $PATH
set -gx PATH ~/.local/bin $PATH
set -gx NODE_PATH ~/node/lib/node_modules $NODE_PATH

starship init fish | source
