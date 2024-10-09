if begin status --is-login; and test $(tty) = /dev/tty1; end
    exec startx
end


