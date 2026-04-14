#ifndef FILESELECTORWIDGET_H
#define FILESELECTORWIDGET_H

#include <QWidget>

namespace Ui {
class FileSelectorWidget;
}

class FileSelectorWidget : public QWidget
{
    Q_OBJECT

public:
    explicit FileSelectorWidget(QWidget *parent = nullptr);
    ~FileSelectorWidget();
    
    QString filePath() const;
    void setFilePath(const QString &path);
    void setFilter(const QString &filter);
    void setPlaceholder(const QString &placeholder);
    void setInitialDirectory(const QString &directory);

signals:
    void fileSelected(const QString &filePath);

private slots:
    void onBrowseClicked();

private:
    Ui::FileSelectorWidget *ui;
    QString fileFilter;
    QString initialDirectory;
};

#endif // FILESELECTORWIDGET_H
