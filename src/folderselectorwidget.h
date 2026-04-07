#ifndef FOLDERSELECTORWIDGET_H
#define FOLDERSELECTORWIDGET_H

#include <QWidget>

QT_BEGIN_NAMESPACE
namespace Ui { class FolderSelectorWidget; }
QT_END_NAMESPACE

class FolderSelectorWidget : public QWidget
{
    Q_OBJECT

public:
    explicit FolderSelectorWidget(QWidget *parent = nullptr);
    ~FolderSelectorWidget();

    QString folderPath() const;
    void setFolderPath(const QString &path);

signals:
    void folderChanged(const QString &path);

private slots:
    void onBrowseClicked();
    void onPathEdited(const QString &text);

private:
    Ui::FolderSelectorWidget *ui;
};

#endif // FOLDERSELECTORWIDGET_H
