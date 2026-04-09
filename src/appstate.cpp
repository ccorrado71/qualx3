#include "appstate.h"
#include "databasebuilder.h"

#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>

QList<DatabaseEntry> AppState::s_databases;
QualxDbManager       AppState::s_db;
QString              AppState::s_openPath;

void AppState::load()
{
    s_databases = ManageDatabasesDialog::loadSettings();

    // First launch (no databases configured): register the bundled default DB
    // if it exists at <installdir>/DB/cod/cod_inorg
    if (s_databases.isEmpty()) {
        const QString appDir = QCoreApplication::applicationDirPath();
        const QString dbPath = QDir::cleanPath(appDir + "/../DB/cod/cod_inorg");

        if (QFileInfo::exists(dbPath + ".sq")) {
            DatabaseEntry e;
            e.inUse   = true;
            e.name    = QStringLiteral("cod_inorg");
            e.path    = dbPath;
            e.entries = DatabaseBuilder::queryEntries(dbPath);
            s_databases.append(e);
            ManageDatabasesDialog::saveSettings(s_databases);
        }
    }

    openActiveDatabase();
}

void AppState::setDatabases(const QList<DatabaseEntry> &databases)
{
    s_databases = databases;
    ManageDatabasesDialog::saveSettings(databases);
    openActiveDatabase();
}

const QList<DatabaseEntry> &AppState::databases()
{
    return s_databases;
}

const DatabaseEntry *AppState::activeDatabase()
{
    for (const DatabaseEntry &db : s_databases) {
        if (db.inUse)
            return &db;
    }
    return nullptr;
}

QualxDbManager &AppState::db()
{
    return s_db;
}

void AppState::openActiveDatabase()
{
    const DatabaseEntry *active = activeDatabase();
    const QString newPath = active ? active->path : QString();

    if (newPath == s_openPath)
        return;

    s_db.closeDatabeses();
    s_openPath.clear();

    if (!newPath.isEmpty()) {
        if (s_db.openDatabases(newPath))
            s_openPath = newPath;
        else
            qWarning("AppState: failed to open database: %s", qPrintable(newPath));
    }
}
