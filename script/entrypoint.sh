#!/bin/bash
#
# Entrypoint for the qualx image: adapts the "qualx" user to the
# host UID/GID (PUID/PGID variables), so files created in /work
# belong to the host user instead of root, then runs the requested
# command with non-root privileges.
#
# Defaults to 1000:1000 if PUID/PGID are not set.
#
set -e

PUID="${PUID:-1000}"
PGID="${PGID:-1000}"

if [ "$(id -g qualx)" != "$PGID" ]; then
    groupmod -o -g "$PGID" qualx
fi

if [ "$(id -u qualx)" != "$PUID" ]; then
    usermod -o -u "$PUID" qualx
fi

# Skip .Xauthority: it is normally bind-mounted read-only from the
# host, so chown would fail on it.
find /home/qualx \( -path /home/qualx/.Xauthority -prune \) -o -exec chown "$PUID:$PGID" {} +

export HOME=/home/qualx

exec setpriv --reuid="$PUID" --regid="$PGID" --clear-groups "$@"
