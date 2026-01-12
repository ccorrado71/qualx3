#include "wavedialog.h"
#include "ui_wavedialog.h"
#include "progkeysettings.h"

#include <QMessageBox>
#include <QSettings>

WaveDialog::WaveDialog(QWidget *parent, int startIndex) :
    QDialog(parent),
    ui(new Ui::WaveDialog)
{
    ui->setupUi(this);

//    //Align radiationLabel to waveLWidget
//    ui->radiationLabel->setMinimumSize(ui->waveLWidget->columnSize());

    QSettings settings;
    if (settings.contains(QUALX_USER_WAVE)) {
        double userWave = settings.value(QUALX_USER_WAVE,-1.0).toDouble();
        if (userWave > 0) ui->waveLWidget->setUserWave(userWave);
    }
    if (settings.contains(QUALX_WAVE_INDEX)) {
        startIndex = settings.value(QUALX_WAVE_INDEX,startIndex).toInt();
    }
    ui->waveLWidget->setComboIndex(startIndex);

}

int WaveDialog::nWaves() const
{
    return ui->waveLWidget->nWaves();
}

double WaveDialog::wave1() const
{
    return ui->waveLWidget->wave1().toDouble();
}

double WaveDialog::wave2() const
{
    return ui->waveLWidget->wave2().toDouble();
}

double WaveDialog::ratio() const
{
    return ui->waveLWidget->ratio().toDouble();
}

bool WaveDialog::isValidInput(QString &errMessage)
{
    return ui->waveLWidget->isValidInput(errMessage);
}

void WaveDialog::done(int result)
{
    if (result == QDialog::Accepted) {
        QString errMessage;
        if (!isValidInput(errMessage)) {
            QMessageBox::warning(this, tr("Error"),errMessage,QMessageBox::Cancel);
            return;
        }
        QSettings settings;
        if (ui->waveLWidget->isUserDefined()) {
            settings.setValue(QUALX_USER_WAVE, wave1());
        }
        settings.setValue(QUALX_WAVE_INDEX,ui->waveLWidget->currentIndex());
    }
    QDialog::done(result);
}

int WaveDialog::radiationType() const
{
    //return ui->radiationComboBox->currentIndex();
    return ui->waveLWidget->radiationIndex();
}

WaveDialog::~WaveDialog()
{
    delete ui;
}
