#include "backgroundcoefwidget.h"
#include "ui_backgroundcoefwidget.h"

BackgroundCoefWidget::BackgroundCoefWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::BackgroundCoefWidget)
{
    ui->setupUi(this);

    ui->coefSpinBox->setKeyboardTracking(false);

    connect(ui->coefSlider, &QSlider::valueChanged, ui->coefSpinBox, &QSpinBox::setValue);
    connect(ui->coefSpinBox, QOverload<int>::of(&QSpinBox::valueChanged), ui->coefSlider, &QSlider::setValue);
    connect(ui->checkBox, &QCheckBox::clicked, this, &BackgroundCoefWidget::onCheckBoxClicked);
    connect(ui->coefSpinBox, QOverload<int>::of(&QSpinBox::valueChanged), this, &BackgroundCoefWidget::onCoefSpinBoxValueChanged);
}

void BackgroundCoefWidget::setMaximumCoef(int value)
{
    ui->coefSlider->setMaximum(value);
    ui->coefSpinBox->setMaximum(value);
}

BackgroundCoefWidget::~BackgroundCoefWidget()
{
    delete ui;
}

void BackgroundCoefWidget::onCheckBoxClicked(bool checked)
{
    ui->coefSlider->setEnabled(!checked);
    ui->coefSpinBox->setEnabled(!checked);
    autob = checked;
    emit autoCoefChanged();
}

bool BackgroundCoefWidget::getAutob() const
{
    return autob;
}

void BackgroundCoefWidget::setNcoef(int value)
{
    ncoef = value;
    ui->coefSpinBox->setValue(value);
}

void BackgroundCoefWidget::setAutob(bool value)
{
    autob = value;
    ui->checkBox->setChecked(value);
    ui->coefSlider->setEnabled(!value);
    ui->coefSpinBox->setEnabled(!value);
}

void BackgroundCoefWidget::onCoefSpinBoxValueChanged(int arg1)
{
    ncoef = arg1;
    emit ncoefChanged();
}

int BackgroundCoefWidget::getNcoef() const
{
    return ncoef;
}
