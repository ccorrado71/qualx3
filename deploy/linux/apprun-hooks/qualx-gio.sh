# Sourced by the linuxdeploy-generated AppRun before qualx starts.
#
# The AppImage bundles glib/gio from its (old) build base. Without this,
# that glib scans the host's gio modules directory and tries to load
# modules built against a newer glib (libdconfsettings.so, libgvfsdbus.so),
# failing with "undefined symbol: g_..." errors. Point it at a directory
# inside the AppImage instead so no host gio module is ever loaded.
export GIO_MODULE_DIR="$APPDIR/usr/lib/gio/modules"
