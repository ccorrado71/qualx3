#ifndef SAVEDIALOG_H
#define SAVEDIALOG_H

#include <QFileDialog>
#include <QCheckBox>

class SaveDialog : public QFileDialog
{
    Q_OBJECT
public:
    SaveDialog(QWidget *parent,
               const QString& windowTitle,
               const QString& defaultDirectory,
               const QString& defaultFileName,
               const QString& filters,
               const QString& defaultSuffix,
               int   check_type);
    virtual ~SaveDialog() {}

    static const QString run(QWidget *parent,
                             const QString& windowTitle,
                             const QString& defaultDirectory,
                             const QString& defaultFileName,
                             const QString& filters,
                             const QString &defaultSuffix,
                             QString &selectedFilter,
                             int *check_type=nullptr);


signals:

public slots:
    void updateDefaultSuffix();

    private slots:
    void checkChanged(int stato);

private:
    const QString m_defaultSuffix;
    QCheckBox *checkButton;
    static int mChecked;
};

#endif // SAVEDIALOG_H
