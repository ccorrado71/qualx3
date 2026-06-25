#ifndef CREATEDATABASEDIALOG_H
#define CREATEDATABASEDIALOG_H

#include <QDialog>
#include <QStringList>

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

    QString pdfFile()    const;
    QString codSqFile()  const;
    QString userSqFile() const;

    // User source options (valid only when isUserSelected() == true)
    enum class UserSource { CifFiles, SqDatabase };
    UserSource userSource()       const;
    bool       isRecursive()      const;
    QString    userSourceFolder() const;

    QString databaseName()      const;
    QString databaseDirectory() const;

    // Returns the paths of missing companion files for a given .sq file.
    // Empty list means all four files are present.
    static QStringList missingSqFiles(const QString &sqFile);

    void accept() override;

private slots:
    void onHelpRequested();
    void onAutoNameToggled(bool checked);
    void onAutoDirToggled(bool checked);
    void onSourceChanged();
    void onUserSourceTypeChanged();
    void onSourceExclusiveToggled(bool checked);

private:
    Ui::CreateDatabaseDialog *ui;

    QString generateAutoName()      const;
    QString generateAutoDirectory() const;
};

#endif // CREATEDATABASEDIALOG_H
