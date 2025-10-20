#!/usr/bin/env bash

# Name:		    yazi-picker.sh
# Author:		  Ian Kollipara <ian.kollipara@gmail.com>
# Created:		2025-10-17
# Updated:		2025-10-17
# Description: 
# 	Integration with Tmux, Yazi, and Helix


paths=$(yazi --chooser-file=/dev/stdout)

if [[ -n "$paths" ]]; then
  tmux last-window
  tmux send-keys Escape
  if [[ "$1" = "hx" ]]; then
    tmux send-keys "hx $paths"
  else
    tmux send-keys ":$1 $paths"
    tmux send-keys Enter
  fi
else
  tmux kill-window -t fx
fi
