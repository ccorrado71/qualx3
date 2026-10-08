# Sourced by the linuxdeploy-generated AppRun before qualx starts.

# LD_LIBRARY_PATH takes precedence over the binaries' $ORIGIN/../lib
# RUNPATH, so a host LD_LIBRARY_PATH pointing at another Qt (e.g. a
# ~/Qt/6.x/gcc_64/lib install) or at Intel oneAPI libraries would make
# qualx load those instead of the bundled ones, failing with errors like
# "undefined symbol: qt_resourceFeatureZstd". Search the bundled
# libraries first.
export LD_LIBRARY_PATH="$APPDIR/usr/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# The AppImage bundles glib/gio from its (old) build base. Without this,
# that glib scans the host's gio modules directory and tries to load
# modules built against a newer glib (libdconfsettings.so, libgvfsdbus.so),
# failing with "undefined symbol: g_..." errors. Point it at a directory
# inside the AppImage instead so no host gio module is ever loaded.
export GIO_MODULE_DIR="$APPDIR/usr/lib/gio/modules"
