set fish_greeting
set -gx PATH ~/bin $PATH
set -gx PATH ~/gocode/bin $PATH
set -gx PATH ~/node/bin $PATH
set -gx NODE_PATH ~/node/lib/node_modules $NODE_PATH
set -gx GOPATH ~/gocode


starship init fish | source
