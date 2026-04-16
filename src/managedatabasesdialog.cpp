#include "managedatabasesdialog.h"
#include "ui_managedatabasesdialog.h"
#include "createdatabasedialog.h"
#include "databasebuilder.h"
#include "mainwindow.h"
#include "libcomune.h"
#include "progkeysettings.h"

#include <QCheckBox>
#include <QDir>
#include <QFile>
#include <QFileDialog>
#include <QHeaderView>
#include <QMessageBox>
#include <QSettings>
#include <QTableWidgetItem>

ManageDatabasesDialog::ManageDatabasesDialog(QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::ManageDatabasesDialog)
    , mActiveIndex(-1)
{
    ui->setupUi(this);

    ui->tableWidget->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->tableWidget->horizontalHeader()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    ui->tableWidget->horizontalHeader()->setSectionResizeMode(2, QHeaderView::ResizeToContents);
    ui->tableWidget->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Stretch);
    ui->tableWidget->verticalHeader()->setVisible(false);

    connect(ui->quitButton,   &QPushButton::clicked, this, &QDialog::accept);
    connect(ui->renameButton, &QPushButton::clicked, this, &ManageDatabasesDialog::onRenameClicked);
    connect(ui->addButton,    &QPushButton::clicked, this, &ManageDatabasesDialog::onAddClicked);
    connect(ui->createButton, &QPushButton::clicked, this, &ManageDatabasesDialog::onCreateClicked);
    connect(ui->deleteButton, &QPushButton::clicked, this, &ManageDatabasesDialog::onDeleteClicked);
}

ManageDatabasesDialog::~ManageDatabasesDialog()
{
    delete ui;
}

void ManageDatabasesDialog::setDatabases(const QList<DatabaseEntry> &databases)
{
    mDatabases = databases;
    mActiveIndex = -1;
    for (int i = 0; i < mDatabases.size(); ++i) {
        if (mDatabases[i].inUse) {
            mActiveIndex = i;
            break;
        }
    }
    rebuildTable();
}

QList<DatabaseEntry> ManageDatabasesDialog::databases() const
{
    return mDatabases;
}

int ManageDatabasesDialog::activeDatabase() const
{
    return mActiveIndex;
}

void ManageDatabasesDialog::rebuildTable()
{
    ui->tableWidget->blockSignals(true);
    ui->tableWidget->setRowCount(mDatabases.size());

    for (int row = 0; row < mDatabases.size(); ++row) {
        const DatabaseEntry &db = mDatabases[row];

        // Column 0: Use (checkbox, centered)
        QWidget *checkWidget = new QWidget();
        QCheckBox *checkBox = new QCheckBox();
        checkBox->setChecked(row == mActiveIndex);
        QHBoxLayout *layout = new QHBoxLayout(checkWidget);
        layout->addWidget(checkBox);
        layout->setAlignment(Qt::AlignCenter);
        layout->setContentsMargins(0, 0, 0, 0);
        checkWidget->setLayout(layout);
        ui->tableWidget->setCellWidget(row, 0, checkWidget);

        connect(checkBox, &QCheckBox::toggled, this, [this, row](bool checked) {
            onCheckboxChanged(row, checked);
        });

        // Column 1: Database name
        QTableWidgetItem *nameItem = new QTableWidgetItem(db.name);
        nameItem->setFlags(nameItem->flags() & ~Qt::ItemIsEditable);
        ui->tableWidget->setItem(row, 1, nameItem);

        // Column 2: Entries
        QTableWidgetItem *entriesItem = new QTableWidgetItem(QString::number(db.entries));
        entriesItem->setFlags(entriesItem->flags() & ~Qt::ItemIsEditable);
        entriesItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        ui->tableWidget->setItem(row, 2, entriesItem);

        // Column 3: Path — show the containing directory, not the full base path
        QTableWidgetItem *pathItem = new QTableWidgetItem(QFileInfo(db.path).path());
        pathItem->setFlags(pathItem->flags() & ~Qt::ItemIsEditable);
        ui->tableWidget->setItem(row, 3, pathItem);
    }

    ui->tableWidget->blockSignals(false);
}

void ManageDatabasesDialog::onCheckboxChanged(int row, bool checked)
{
    if (!checked) {
        // Prevent unchecking without selecting another
        if (mActiveIndex == row) {
            // Re-check it silently
            QWidget *w = ui->tableWidget->cellWidget(row, 0);
            if (w) {
                QCheckBox *cb = w->findChild<QCheckBox *>();
                if (cb) {
                    cb->blockSignals(true);
                    cb->setChecked(true);
                    cb->blockSignals(false);
                }
            }
        }
        return;
    }

    // Uncheck the previously active row
    if (mActiveIndex >= 0 && mActiveIndex != row) {
        QWidget *w = ui->tableWidget->cellWidget(mActiveIndex, 0);
        if (w) {
            QCheckBox *cb = w->findChild<QCheckBox *>();
            if (cb) {
                cb->blockSignals(true);
                cb->setChecked(false);
                cb->blockSignals(false);
            }
        }
        mDatabases[mActiveIndex].inUse = false;
    }

    mActiveIndex = row;
    mDatabases[row].inUse = true;
}

int ManageDatabasesDialog::currentSelectedRow() const
{
    return ui->tableWidget->currentRow();
}

void ManageDatabasesDialog::onRenameClicked()
{
    emit renameRequested(currentSelectedRow());
}

void ManageDatabasesDialog::onAddClicked()
{
    emit addRequested();
}

// Helper: append an entry for an existing .sq database and persist settings.
// sqFile is the full path to the .sq file; name is the display name.
void ManageDatabasesDialog::registerExistingSqDatabase(const QString &sqFile,
                                                        const QString &name)
{
    const QString base = sqFile.chopped(3);   // strip trailing ".sq"
    DatabaseEntry entry;
    entry.inUse   = mDatabases.isEmpty();
    entry.name    = name;
    entry.entries = DatabaseBuilder::queryEntries(base);
    entry.path    = base;
    mDatabases.append(entry);
    if (entry.inUse)
        mActiveIndex = mDatabases.size() - 1;
    saveSettings(mDatabases);
    rebuildTable();
}

void ManageDatabasesDialog::onCreateClicked()
{
    CreateDatabaseDialog dlg(this);
    if (dlg.exec() != QDialog::Accepted)
        return;

    const QString name = dlg.databaseName();

    // COD: register an existing QualX .sq database (no build step)
    if (dlg.isCodSelected()) {
        registerExistingSqDatabase(dlg.codSqFile(), name);
        return;
    }

    // User source — .sq file: register an existing QualX database (no build step)
    if (dlg.isUserSelected() &&
            dlg.userSource() == CreateDatabaseDialog::UserSource::SqDatabase) {
        registerExistingSqDatabase(dlg.userSqFile(), name);
        return;
    }

    if (!dlg.isPdfSelected() && !dlg.isUserSelected()) {
        QMessageBox::information(this, tr("Not Implemented"),
            tr("Only the ICDD PDF and user CIF sources are currently supported."));
        return;
    }

    const QString dir      = dlg.databaseDirectory();
    const QString basePath = dir + QDir::separator() + name;

    if (!QDir().mkpath(dir)) {
        QMessageBox::critical(this, tr("Error"),
            tr("Could not create directory:\n%1").arg(dir));
        return;
    }

    bool cancelled = false;

    if (dlg.isPdfSelected()) {
        const QString pdf2File = dlg.pdfFile();
        if (pdf2File.isEmpty()) {
            QMessageBox::warning(this, tr("Missing File"),
                tr("Please select a PDF-2 data file (.dat)."));
            return;
        }
        if (!DatabaseBuilder::buildPdfDatabase(basePath, pdf2File, this, &cancelled)) {
            if (!cancelled)
                QMessageBox::critical(this, tr("Error"),
                    tr("Failed to build database at:\n%1").arg(basePath));
            return;
        }
    } else {
        // User CIF source
        const QString cifDir = dlg.userSourceFolder();
        if (cifDir.isEmpty()) {
            QMessageBox::warning(this, tr("Missing Folder"),
                tr("Please select the folder containing the CIF files."));
            return;
        }
        if (!initQualxTables(MainWindow::getPathDataFiles())) {
            QMessageBox::critical(this, tr("Error"),
                tr("Could not initialise chemical tables."));
            return;
        }
        if (!DatabaseBuilder::buildCifDatabase(basePath, cifDir,
                dlg.isRecursive(), this, &cancelled)) {
            if (!cancelled)
                QMessageBox::critical(this, tr("Error"),
                    tr("Failed to build CIF database at:\n%1").arg(basePath));
            return;
        }
    }

    DatabaseEntry entry;
    entry.inUse   = mDatabases.isEmpty();
    entry.name    = name;
    entry.entries = DatabaseBuilder::queryEntries(basePath);
    entry.path    = basePath;

    mDatabases.append(entry);
    if (entry.inUse)
        mActiveIndex = mDatabases.size() - 1;

    saveSettings(mDatabases);
    rebuildTable();
}

// static
void ManageDatabasesDialog::saveSettings(const QList<DatabaseEntry> &databases)
{
    QSettings s;
    s.setValue(DB_COUNT_KEY, databases.size());
    for (int i = 0; i < databases.size(); ++i) {
        s.setValue(QString(DB_INUSE_KEY).arg(i), databases[i].inUse);
        s.setValue(QString(DB_NAME_KEY).arg(i),  databases[i].name);
        s.setValue(QString(DB_PATH_KEY).arg(i),  databases[i].path);
    }
}

// static
QList<DatabaseEntry> ManageDatabasesDialog::loadSettings()
{
    QSettings s;
    const int count = s.value(DB_COUNT_KEY, 0).toInt();
    QList<DatabaseEntry> list;
    list.reserve(count);
    for (int i = 0; i < count; ++i) {
        DatabaseEntry e;
        e.inUse   = s.value(QString(DB_INUSE_KEY).arg(i), false).toBool();
        e.name    = s.value(QString(DB_NAME_KEY).arg(i)).toString();
        e.path    = s.value(QString(DB_PATH_KEY).arg(i)).toString();
        e.entries = DatabaseBuilder::queryEntries(e.path);
        list.append(e);
    }
    return list;
}

void ManageDatabasesDialog::onDeleteClicked()
{
    const int row = currentSelectedRow();
    if (row < 0 || row >= mDatabases.size()) {
        QMessageBox::information(this, tr("No Selection"),
            tr("Please select a database to remove."));
        return;
    }

    const DatabaseEntry &entry = mDatabases[row];

    // Single confirmation dialog with embedded "delete files" checkbox
    QMessageBox msgBox(this);
    msgBox.setWindowTitle(tr("Remove Database"));
    msgBox.setText(tr("Remove <b>%1</b> from the database list?").arg(entry.name));
    msgBox.setIcon(QMessageBox::Warning);
    msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::Cancel);
    msgBox.setDefaultButton(QMessageBox::Cancel);

    QCheckBox *deleteFilesCheck = new QCheckBox(
        tr("Also delete database files from disk (.sq, .sq.info, .sq.infostat, .sq.search)"));
    deleteFilesCheck->setChecked(true);
    msgBox.setCheckBox(deleteFilesCheck);   // msgBox takes ownership

    if (msgBox.exec() != QMessageBox::Yes)
        return;

    const bool deleteFiles = deleteFilesCheck->isChecked();
    const QString basePath = entry.path;

    // --- Remove from list ---
    mDatabases.removeAt(row);

    // Update active index
    if (mActiveIndex == row) {
        // Deleted the active database: activate the first remaining, if any
        mActiveIndex = mDatabases.isEmpty() ? -1 : 0;
        if (mActiveIndex == 0)
            mDatabases[0].inUse = true;
    } else if (mActiveIndex > row) {
        --mActiveIndex;
    }

    // --- Delete files from disk (if requested) ---
    if (deleteFiles) {
        const QStringList suffixes = { ".sq", ".sq.info", ".sq.infostat", ".sq.search" };
        QStringList failed;
        for (const QString &suffix : suffixes) {
            const QString filePath = basePath + suffix;
            QFile f(filePath);
            if (f.exists() && !f.remove())
                failed << filePath;
        }
        if (!failed.isEmpty()) {
            QMessageBox::warning(this, tr("Could Not Delete Files"),
                tr("The following files could not be deleted:\n%1")
                    .arg(failed.join('\n')));
        }
    }

    // --- Persist and refresh ---
    saveSettings(mDatabases);
    rebuildTable();
}
