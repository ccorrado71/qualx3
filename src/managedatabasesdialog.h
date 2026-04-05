#ifndef MANAGEDATABASESDIALOG_H
#define MANAGEDATABASESDIALOG_H

#include <QDialog>

namespace Ui {
class ManageDatabasesDialog;
}

struct DatabaseEntry {
    bool inUse;
    QString name;
    int entries;
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

signals:
    void renameRequested(int row);
    void addRequested();
    void createRequested();
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
    int currentSelectedRow() const;

    QList<DatabaseEntry> mDatabases;
    int mActiveIndex;
};

#endif // MANAGEDATABASESDIALOG_H
