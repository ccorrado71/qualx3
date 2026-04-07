#ifndef CREATEDATABASEDIALOG_H
#define CREATEDATABASEDIALOG_H

#include <QDialog>

namespace Ui {
class CreateDatabaseDialog;
}

class CreateDatabaseDialog : public QDialog
{
    Q_OBJECT

public:
    explicit CreateDatabaseDialog(QWidget *parent = nullptr);
    ~CreateDatabaseDialog();

    bool isCodSelected()  const;
    bool isPdfSelected()  const;
    bool isUserSelected() const;

    QString databaseName()      const;
    QString databaseDirectory() const;

private slots:
    void onHelpRequested();
    void onAutoNameToggled(bool checked);
    void onAutoDirToggled(bool checked);
    void onSourceChanged();

private:
    Ui::CreateDatabaseDialog *ui;

    QString generateAutoName()      const;
    QString generateAutoDirectory() const;
};

#endif // CREATEDATABASEDIALOG_H
