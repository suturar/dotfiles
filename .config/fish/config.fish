setxkbmap -option ctrl:nocaps
setxkbmap -layout us -variant altgr-intl
# Set natural scrolling for touchpad (numbers may change)
xinput set-prop 19 350 1

alias hx=helix
alias lla="ls -hla"
set -x EDITOR helix

if status is-interactive
    # Commands to run in interactive sessions can go here
end
