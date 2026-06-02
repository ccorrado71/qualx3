#ifndef ABOUTDIALOG_H
#define ABOUTDIALOG_H

#include <QDialog>

namespace Ui {
class AboutDialog;
}

class AboutDialog : public QDialog
{
    Q_OBJECT

public:
    explicit AboutDialog(QWidget *parent = nullptr);
    ~AboutDialog();

    void setWebsiteUrl(const QString &url, const QString &text = QString());
    void setContactsUrl(const QString &url, const QString &text = QString());
    void setCitationUrl(const QString &url, const QString &text = QString());

private:
    Ui::AboutDialog *ui;

    static QString linkHtml(const QString &url, const QString &text = QString());
};

#endif // ABOUTDIALOG_H
