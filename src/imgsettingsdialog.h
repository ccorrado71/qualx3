#ifndef IMGSETTINGSDIALOG_H
#define IMGSETTINGSDIALOG_H

#include <QDialog>

namespace Ui {
class ImgSettingsDialog;
}

class ImgSettingsDialog : public QDialog
{
    Q_OBJECT

public:
    explicit ImgSettingsDialog(QWidget *parent = nullptr, const QString &ext = " ");
    bool isTransparent();
    double scale();
    ~ImgSettingsDialog();

private:
    Ui::ImgSettingsDialog *ui;
};

#endif // IMGSETTINGSDIALOG_H
