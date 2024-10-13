if begin status --is-login; and test $(tty) = /dev/tty1; end
    if test (hostname) = "nuez"
        exec dbus-run-session -- sway
    else
	exec startx
    end
end


