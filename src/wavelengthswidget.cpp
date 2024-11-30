#include "wavelengthswidget.h"
#include "wavelengthscombobox.h"
#include "ui_wavelengthswidget.h"

WavelengthsWidget::WavelengthsWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::WavelengthsWidget)
{
    ui->setupUi(this);

//#ifdef Q_OS_MACOS
//    ui->formLayout->setFormAlignment(Qt::AlignLeft | Qt::AlignTop);
//    ui->formLayout->setLabelAlignment(Qt::AlignLeft | Qt::AlignVCenter);
//#endif

    QDoubleValidator *waveVal = new QDoubleValidator(0.0, 5.0, 8, ui->wave1LineEdit);
    ui->wave1LineEdit->setValidator(waveVal);
    ui->wave2LineEdit->setValidator(waveVal);
    ui->ratioLineEdit->setValidator(new QDoubleValidator(0.0, 1.0, 8, ui->ratioLineEdit));

    ui->wave2CheckBox->setChecked(false);

    connect(ui->waveComboBox, &WavelengthsComboBox::wavelComboBoxActivated, this, [=] (int index) {
        setWave1LineEditVisible(index);
        if (ui->wave2CheckBox->isChecked()) setWave2LineEdit();
    });
}

int WavelengthsWidget::nWaves() const
{
    return ui->wave2CheckBox->isChecked() ? 2 : 1;
}

QString WavelengthsWidget::wave1() const
{
    QString wave;
    if (ui->waveComboBox->isUserDefined(ui->waveComboBox->currentIndex())) {
        return wave = ui->wave1LineEdit->text();
    } else {
        return wave = ui->waveComboBox->wave1();
    }
}

QString WavelengthsWidget::wave2() const
{
    if (hasWave2()) return ui->wave2LineEdit->text();
    return QString();
}

QString WavelengthsWidget::ratio() const
{
    if (hasWave2()) return ui->ratioLineEdit->text();
    return QString();
}

bool WavelengthsWidget::hasWave2() const
{
    return ui->wave2CheckBox->isChecked();
}

QSize WavelengthsWidget::columnSize() const
{
    //compute size of first column of form layout from the largest label
    return ui->wave2checkLabel->sizeHint();
}

bool WavelengthsWidget::isValidInput(QString &errMessage) const
{
    bool isValid = true;
    if (ui->waveComboBox->isUserDefined(ui->waveComboBox->currentIndex())) {
        if (!ui->wave1LineEdit->hasAcceptableInput()) {
            errMessage = "Wavelength is not a valid value.";
            isValid = false;
        }
    }
    if (hasWave2()) {
        if (!ui->wave2LineEdit->hasAcceptableInput()) {
            errMessage += "Wavelength 2 is not a valid value.";
            isValid = false;
        }
        if (!ui->ratioLineEdit->hasAcceptableInput()) {
            errMessage += "Ratio is not a valid value.";
            isValid = false;
        }
    }
    return isValid;
}

bool WavelengthsWidget::isUserDefined() const
{
    return ui->waveComboBox->isUserDefined(ui->waveComboBox->currentIndex());
}

int WavelengthsWidget::currentIndex()
{
    return ui->waveComboBox->currentIndex();
}

int WavelengthsWidget::radiationIndex()
{
    return ui->radiationComboBox->currentIndex();
}

void WavelengthsWidget::setComboIndex(int index)
{
    ui->waveComboBox->setCurrentIndex(index);
    setWave1LineEditVisible(index);
}

void WavelengthsWidget::setComboAsUserDefined(double wave)
{
    setComboIndex(ui->waveComboBox->getUserDefinedIndex());
    setUserWave(wave);
}

void WavelengthsWidget::setUserWave(double wave)
{
    if (wave > 0.0) ui->wave1LineEdit->setText(QString::number(wave));
}

void WavelengthsWidget::setSeconWave(Qt::CheckState checkState, double wave, double ratio)
{
    ui->wave2CheckBox->setCheckState(checkState);
    ui->wave2LineEdit->setText(QString::number(wave));
    ui->ratioLineEdit->setText(QString::number(ratio));
}

WavelengthsWidget::~WavelengthsWidget()
{
    delete ui;
}

void WavelengthsWidget::setWave2LineEdit()
{
    if (!ui->waveComboBox->isUserDefined(ui->waveComboBox->currentIndex())) {
        QString anode = ui->waveComboBox->anode();
        double wave2 = -1;
        for (int i = 0; i < waveType2.count(); i++) {
            if (waveType2.at(i).contains(anode)) wave2 = waveValue2.at(i);
        }
        if (wave2 > 0.0) ui->wave2LineEdit->setText(QString::number(wave2));
    }
}

void WavelengthsWidget::on_wave2CheckBox_stateChanged(int arg1)
{
    ui->wave2LineEdit->setEnabled(arg1 == Qt::Checked);
    ui->ratioLineEdit->setEnabled(arg1 == Qt::Checked);

    //Now try to find a valid Ka2 wavelength in mapWaves2
    if (arg1 == Qt::Checked) {
        setWave2LineEdit();
        if (ui->ratioLineEdit->text().isEmpty()) ui->ratioLineEdit->setText(QString::number(0.5));
    }
}

void WavelengthsWidget::setWave1LineEditVisible(int index)
{
    //Set visibility of wave1LineEdit in case of "User Defined"
    bool vis = ui->waveComboBox->isUserDefined(index);
    ui->wave1LineEdit->setVisible(vis);
    if (vis) ui->wave1LineEdit->setFocus();
}
