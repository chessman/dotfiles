set fish_greeting
set -gx PATH ~/bin $PATH
set -gx PATH ~/node/bin $PATH
set -gx PATH ~/.local/share/coursier/bin $PATH
set -gx NODE_PATH ~/node/lib/node_modules $NODE_PATH

starship init fish | source
