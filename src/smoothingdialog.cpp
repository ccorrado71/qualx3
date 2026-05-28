#include "smoothingdialog.h"
#include "ui_smoothingdialog.h"
#include "mainwindow.h"

extern "C" void set_smooth(int kaction, smoothOptions *soptions);

SmoothingDialog::SmoothingDialog(MainWindow *mw) :
    QDialog(mw),
    ui(new Ui::SmoothingDialog),
    mWindow(mw)
{
    ui->setupUi(this);

    ui->pointsSpinBox->setKeyboardTracking(false);
    ui->polynomialSpinBox->setKeyboardTracking(false);
    connect(ui->pointsSlider,&QSlider::valueChanged,ui->pointsSpinBox,&QSpinBox::setValue);
    connect(ui->pointsSpinBox,QOverload<int>::of(&QSpinBox::valueChanged),ui->pointsSlider,&QSlider::setValue);
    connect(ui->polynomialSlider,&QSlider::valueChanged,ui->polynomialSpinBox,&QSpinBox::setValue);
    connect(ui->polynomialSpinBox,QOverload<int>::of(&QSpinBox::valueChanged),ui->polynomialSlider,&QSlider::setValue);
}

SmoothingDialog::~SmoothingDialog()
{
    delete ui;
}

void SmoothingDialog::setSmoothing()
{
    set_smooth(0, &sOptions);
    sOptionsSaved = sOptions;
    ui->methodComboBox->setCurrentIndex(sOptions.method);
    ui->pointsSpinBox->setValue(getPoints());
    ui->polynomialSpinBox->setValue(sOptions.pol_order);

    mWindow->saveEnabledActions();
    mWindow->enableActions(MainWindow::DialogOpenAction);
}

void SmoothingDialog::on_methodComboBox_currentIndexChanged(int index)
{    
    ui->polynomialLabel->setEnabled(index == SavGol);
    ui->polynomialSlider->setEnabled(index == SavGol);
    ui->polynomialSpinBox->setEnabled(index == SavGol);
    switch (index) {
    case SavGol:
        ui->pointsSlider->setMinimum(2);
        ui->pointsSpinBox->setMinimum(2);
        break;
    case Averaging:
        ui->pointsSlider->setMinimum(1);
        ui->pointsSpinBox->setMinimum(1);
        break;
    }
    if (sOptions.method != index) {
        sOptions.method = index;
        ui->pointsSpinBox->setValue(getPoints());
        set_smooth(1,&sOptions);
    }
}

int SmoothingDialog::getPoints() const
{
    switch (sOptions.method) {
    case SavGol: return sOptions.npoints_sg;
    case Averaging: return sOptions.npoints_ave;
    default: return 10;
    }
}

void SmoothingDialog::setPoints(int npoints)
{
    switch (sOptions.method) {
    case SavGol:
        sOptions.npoints_sg = npoints;
        break;
    case Averaging:
        sOptions.npoints_ave = npoints;
        break;
    }
}

void SmoothingDialog::on_pointsSpinBox_valueChanged(int arg1)
{
    if (getPoints() != arg1) {
        setPoints(arg1);
        if (2*arg1 >= sOptions.pol_order) set_smooth(1,&sOptions);
    }
}

void SmoothingDialog::on_polynomialSpinBox_valueChanged(int arg1)
{
    if (sOptions.pol_order != arg1) {
        sOptions.pol_order = arg1;
        if (2*arg1 >= sOptions.pol_order) set_smooth(2,&sOptions);
    }
}

void SmoothingDialog::accept()
{
    //'ok' get focus if you hide the dialog. This avoid that enter key close the dialog
    if (!ui->buttonBox->button(QDialogButtonBox::Ok)->hasFocus()) return;

    QDialog::accept();
}

void SmoothingDialog::restoreOptions()
{
    sOptions = sOptionsSaved;
    set_smooth(3,&sOptions);
}

void SmoothingDialog::reject()
{
    restoreOptions();
    mWindow->restoreEnabledActions();
    QDialog::reject();
}

void SmoothingDialog::on_buttonBox_clicked(QAbstractButton *button)
{
    QDialogButtonBox::StandardButton stdButton = ui->buttonBox->standardButton(button);

    if(stdButton == QDialogButtonBox::Ok) {
        set_smooth(4,&sOptions);
        mWindow->restoreEnabledActions();
    } else if (stdButton == QDialogButtonBox::Cancel) {
        restoreOptions();
    }
}
