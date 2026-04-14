#ifndef MANAGEDATABASESDIALOG_H
#define MANAGEDATABASESDIALOG_H

#include <QDialog>
#include <QList>

namespace Ui {
class ManageDatabasesDialog;
}

struct DatabaseEntry {
    bool    inUse   = false;
    QString name;
    int     entries = 0;
    QString path;
};

class ManageDatabasesDialog : public QDialog
{
    Q_OBJECT

public:
    explicit ManageDatabasesDialog(QWidget *parent = nullptr);
    ~ManageDatabasesDialog();

    void setDatabases(const QList<DatabaseEntry> &databases);
    QList<DatabaseEntry> databases() const;
    int activeDatabase() const;

    // Persist / restore the database list from QSettings.
    // loadSettings queries each database for its entry count.
    static void saveSettings(const QList<DatabaseEntry> &databases);
    static QList<DatabaseEntry> loadSettings();

signals:
    void renameRequested(int row);
    void addRequested();
    void deleteRequested(int row);

private slots:
    void onCheckboxChanged(int row, bool checked);
    void onRenameClicked();
    void onAddClicked();
    void onCreateClicked();
    void onDeleteClicked();

private:
    Ui::ManageDatabasesDialog *ui;

    void rebuildTable();
    void registerExistingSqDatabase(const QString &sqFile, const QString &name);
    int currentSelectedRow() const;

    QList<DatabaseEntry> mDatabases;
    int mActiveIndex;
};

#endif // MANAGEDATABASESDIALOG_H
