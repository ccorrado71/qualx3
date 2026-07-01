#include "appstate.h"
#include "databasebuilder.h"
#include "progkeysettings.h"

#include <QDir>
#include <QDirIterator>
#include <QFileDialog>
#include <QFileInfo>
#include <QMessageBox>
#include <QPushButton>
#include <QSet>
#include <QSettings>

QList<DatabaseEntry> AppState::s_databases;
QualxDbManager       AppState::s_db;
QString              AppState::s_openPath;
ExperimentalPeaks    AppState::s_peaks;

void AppState::load()
{
    s_databases = ManageDatabasesDialog::loadSettings();

    // Scan the default QualXDB folder every launch so that databases added
    // manually to the folder are discovered without user intervention.
    const QString defDir = defaultDbDir();
    if (QDir(defDir).exists()) {
        scanAndRegisterDatabases(defDir);
    } else if (s_databases.isEmpty()) {
        promptForDefaultDir();
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

ExperimentalPeaks &AppState::peaks()
{
    return s_peaks;
}

QString AppState::defaultDbDir()
{
    QSettings s;
    return s.value(DB_DEFAULT_DIR_KEY,
                   QDir::homePath() + "/QualXDB").toString();
}

void AppState::setDefaultDbDir(const QString &dir)
{
    QSettings s;
    s.setValue(DB_DEFAULT_DIR_KEY, dir);
}

void AppState::scanAndRegisterDatabases(const QString &dir)
{
    QSet<QString> known;
    for (const DatabaseEntry &e : s_databases)
        known.insert(e.path);

    const int initialSize = s_databases.size();

    QDirIterator it(dir, QStringList{QStringLiteral("*.sq")},
                    QDir::Files, QDirIterator::Subdirectories);
    while (it.hasNext()) {
        const QString sqFile = it.next();
        const QString base   = sqFile.chopped(3); // strip ".sq"
        if (known.contains(base))
            continue;

        DatabaseEntry e;
        // The first newly discovered database becomes active only if the list
        // was empty before this scan (no databases were configured at all).
        e.inUse       = (initialSize == 0) && (s_databases.size() == initialSize);
        e.name        = QFileInfo(sqFile).baseName();
        e.path        = base;
        e.entries     = DatabaseBuilder::queryEntries(base);
        e.contentType = DatabaseBuilder::queryContentType(base);
        s_databases.append(e);
    }

    if (s_databases.size() > initialSize)
        ManageDatabasesDialog::saveSettings(s_databases);
}

void AppState::promptForDefaultDir()
{
    QMessageBox msgBox;
    msgBox.setWindowTitle(QStringLiteral("No database found"));
    msgBox.setIcon(QMessageBox::Warning);
    msgBox.setText(
        QString("No crystallographic databases were found.\n\n"
                "The default folder does not exist:\n  %1").arg(defaultDbDir()));
    msgBox.setInformativeText(
        "You can choose a different default folder, or manage databases "
        "later via Search > Manage Databases.");

    QPushButton *chooseBtn =
        msgBox.addButton("Choose folder...", QMessageBox::ActionRole);
    msgBox.addButton("Manage Databases later", QMessageBox::RejectRole);

    msgBox.exec();

    if (msgBox.clickedButton() == chooseBtn) {
        const QString dir = QFileDialog::getExistingDirectory(
            nullptr,
            QStringLiteral("Select QualXDB folder"),
            QDir::homePath());
        if (!dir.isEmpty()) {
            setDefaultDbDir(dir);
            scanAndRegisterDatabases(dir);
        }
    }
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
