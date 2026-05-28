#ifndef PROGKEYSETTINGS_H
#define PROGKEYSETTINGS_H

#define DEFAULT_DIR_KEY "default_dir"
#define QUALX_GEOMETRY_KEY "expogeometry"
#define QUALX_STATE_KEY "expostate"
#define QUALX_USER_WAVE "expouserwave"
#define QUALX_WAVE_INDEX "expowaveindex"
#define QUALX_RECENT_FILES_KEY "qualxrecentfiles"

// Database registry keys.
// DB_INUSE_KEY / DB_NAME_KEY / DB_PATH_KEY expect a single %1 = zero-based index.
#define DB_COUNT_KEY "databases/count"
#define DB_INUSE_KEY "databases/%1/inUse"
#define DB_NAME_KEY  "databases/%1/name"
#define DB_PATH_KEY  "databases/%1/path"

#endif // PROGKEYSETTINGS_H
