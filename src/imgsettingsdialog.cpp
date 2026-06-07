#include "imgsettingsdialog.h"
#include "ui_imgsettingsdialog.h"

ImgSettingsDialog::ImgSettingsDialog(QWidget *parent, const QString &ext) :
    QDialog(parent),
    ui(new Ui::ImgSettingsDialog)
{
    ui->setupUi(this);

    if (ext != "png") ui->transparentCheckBox->setEnabled(false);
}

bool ImgSettingsDialog::isTransparent()
{
    return ui->transparentCheckBox->isChecked();
}

double ImgSettingsDialog::scale()
{
    return ui->scaleSpinBox->value();
}

ImgSettingsDialog::~ImgSettingsDialog()
{
    delete ui;
}
