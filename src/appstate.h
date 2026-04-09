#pragma once

#include <QList>
#include "managedatabasesdialog.h"  // DatabaseEntry
#include "qualxdbmanager.h"

// -----------------------------------------------------------------------
// AppState
//
// Application-level state loaded at startup and accessible from anywhere,
// including no-GUI (command-line) mode.
//
// Usage:
//   // In main.cpp, after QApplication metadata is set:
//   AppState::load();
//
//   // Anywhere else:
//   const QList<DatabaseEntry> &dbs = AppState::databases();
//   const DatabaseEntry *active     = AppState::activeDatabase();
//   QualxDbManager &db              = AppState::db();
//
//   // After the user modifies the list (e.g. in ManageDatabasesDialog):
//   AppState::setDatabases(newList); // persists to QSettings + reopens DB if active changed
// -----------------------------------------------------------------------
class AppState
{
public:
    // Loads the database list from QSettings and opens the active database.
    // Call once at startup, after QApplication metadata is set.
    static void load();

    // Replaces the in-memory list, persists it to QSettings,
    // and reopens the database if the active entry has changed.
    static void setDatabases(const QList<DatabaseEntry> &databases);

    // Returns the full list of known databases.
    static const QList<DatabaseEntry> &databases();

    // Returns a pointer to the currently active (inUse) database entry,
    // or nullptr if none is marked active.
    static const DatabaseEntry *activeDatabase();

    // Returns the database manager for the currently active database.
    // The manager is opened automatically when the active database changes.
    static QualxDbManager &db();

private:
    // Opens the active database (closes any previously open one first).
    static void openActiveDatabase();

    static QList<DatabaseEntry> s_databases;
    static QualxDbManager       s_db;
    static QString              s_openPath;   // path of the currently open database
};
