#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

#if [ -d /etc/X11/xinit/xinitrc.d ]; then
#  for f in /etc/X11/xinit/xinitrc.d/*; do
#    [ -x "$f" ] && . "$f"
#  done
#  unset f
#fi

/usr/bin/start-pulseaudio-x11

# stop screen tearing
compton &

# notifications
dunst &

xset b off

sudo bash -c 'echo 0 >/sys/block/sda/queue/iosched/slice_idle'
sudo bash -c 'echo 0 >/sys/block/sda/queue/iosched/group_idle'

xsetroot -cursor_name Neutral

xrdb -merge .Xresources

feh --bg-scale ~/wallpaper.jpg

envfile="$HOME/.gnupg/gpg-agent.env"
if [[ -e "$envfile" ]] && kill -0 $(grep GPG_AGENT_INFO "$envfile" | cut -d: -f 2) 2>/dev/null; then
    eval "$(cat "$envfile")"
else
    eval "$(gpg-agent --daemon --enable-ssh-support --write-env-file "$envfile")"
fi
export GPG_AGENT_INFO  # the env file does not contain the export statement
wexport SSH_AUTH_SOCK   # enable gpg-agent for ssh


# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
exec xmonad
