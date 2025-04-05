#include "peaksearchdialog.h"
#include "ui_peaksearchdialog.h"
#include "mainwindow.h"

#include <cmath>
#include <QPushButton>

extern "C" void peak_search_action(int iAction, peakSearchSettings *pSettings);

PeakSearchDialog::PeakSearchDialog(MainWindow *mw) :
    QDialog(mw),
    ui(new Ui::PeakSearchDialog),
    mWindow(mw)
{
    ui->setupUi(this);

    //Disable emission of valueChanged signal when spin is edited
    ui->intensitySpinBox->setKeyboardTracking(false);
    ui->minSpinBox->setKeyboardTracking(false);
    ui->maxSpinBox->setKeyboardTracking(false);
    ui->sensitivitySpinBox->setKeyboardTracking(false);
    ui->numPeaksSpinBox->setKeyboardTracking(false);

    connect(ui->intensitySlider,&QSlider::valueChanged,ui->intensitySpinBox,[=](double value){
        ui->intensitySpinBox->setValue(value*0.01);        
    });
    connect(ui->intensitySpinBox,QOverload<double>::of(&QDoubleSpinBox::valueChanged),ui->intensitySlider,[=](double value){
        ui->intensitySlider->setValue(value*100);
    });
    connect(ui->intensitySpinBox, QOverload<double>::of(&QDoubleSpinBox::valueChanged), this, &PeakSearchDialog::onIntensitySpinBoxValueChanged);
    connect(ui->sensitivitySlider, &QSlider::valueChanged,ui->sensitivitySpinBox,&QSpinBox::setValue);
    connect(ui->sensitivitySpinBox, QOverload<int>::of(&QSpinBox::valueChanged),ui->sensitivitySlider,&QSlider::setValue);
    connect(ui->sensitivitySpinBox, QOverload<int>::of(&QSpinBox::valueChanged), this, &PeakSearchDialog::onSensitivitySpinBoxValueChanged);
    connect(ui->appendCheckBox, &QCheckBox::clicked,this,[=](bool state){ pkSettings.append = state; });
    connect(ui->numPeaksSpinBox, &QSpinBox::valueChanged, this, &PeakSearchDialog::onNumPeaksSpinBoxValueChanged);
    connect(ui->minSpinBox, QOverload<double>::of(&QDoubleSpinBox::valueChanged), this, &PeakSearchDialog::onMinSpinBoxValueChanged);
    connect(ui->maxSpinBox, QOverload<double>::of(&QDoubleSpinBox::valueChanged), this, &PeakSearchDialog::onMaxSpinBoxValueChanged);
    connect(ui->buttonBox, &QDialogButtonBox::clicked, this, &PeakSearchDialog::onButtonBoxClicked);
}

void PeakSearchDialog::setOptions()
{
    peak_search_action(1, &pkSettings);

    setWidgets(pkSettings);

    pkSettingsSaved = pkSettings;

    //To prevent that enter key close the dialog. Call this after the show. See also accept()
    ui->buttonBox->button(QDialogButtonBox::Ok)->setDefault(false);
    ui->buttonBox->button(QDialogButtonBox::Ok)->setAutoDefault(false);

    mWindow->setZoomAction(); //Force zoom action and disable del/add peak action
    mWindow->saveEnabledActions();
    mWindow->enableActions(MainWindow::DialogOpenAction);
}

PeakSearchDialog::~PeakSearchDialog()
{
    delete ui;
}

void PeakSearchDialog::onNumPeaksSpinBoxValueChanged()
{
    if (ui->numPeaksSpinBox->value() != pkSettings.numPeaks) {
        pkSettings.numPeaks = ui->numPeaksSpinBox->value();        
        peak_search_action(5, &pkSettings);
        ui->intensitySpinBox->setValue(pkSettings.threshold);
        updatePeakListTable();
    }
}

void PeakSearchDialog::setWidgets(const peakSearchSettings &psettings)
{
    ui->minSpinBox->setValue(psettings.minRange);
    ui->minSpinBox->setRange(psettings.minRange,psettings.maxRange);

    ui->maxSpinBox->setValue(psettings.maxRange);
    ui->maxSpinBox->setRange(psettings.minRange,psettings.maxRange);

    ui->numPeaksSpinBox->setValue(psettings.numPeaks);
    ui->numPeaksSpinBox->setMaximum(psettings.numPeaksTot);

    ui->intensitySpinBox->setValue(psettings.threshold);

    ui->sensitivitySpinBox->setValue(psettings.sensitivity);

    ui->appendCheckBox->setChecked(psettings.append);
}

void PeakSearchDialog::accept()
{
#ifndef Q_OS_MACOS
    //'ok' get focus if you hide the dialog. This avoid that enter key close the dialog
    if (!ui->buttonBox->button(QDialogButtonBox::Ok)->hasFocus()) return;
#endif

    QDialog::accept();
}

void PeakSearchDialog::reject()
{
    restoreSavedSettings();
    mWindow->restoreEnabledActions();
    QDialog::reject();
}

void PeakSearchDialog::restoreSavedSettings()
{
    pkSettings = pkSettingsSaved;
    setWidgets(pkSettings);    
    peak_search_action(0, &pkSettings);
    updatePeakListTable();    
}

void PeakSearchDialog::onButtonBoxClicked(QAbstractButton *button)
{
    QDialogButtonBox::StandardButton stdButton = ui->buttonBox->standardButton(button);

    if(stdButton == QDialogButtonBox::Ok) {
        mWindow->restoreEnabledActions();

    } else if (stdButton == QDialogButtonBox::Cancel) {
        //restoreSavedSettings();

    } else if (stdButton == QDialogButtonBox::RestoreDefaults) {        
        peak_search_action(6, &pkSettings);
        setWidgets(pkSettings);
        updatePeakListTable();
    }
}

void PeakSearchDialog::updatePeakListTable()
{
    MainWindow *mw = qobject_cast<MainWindow *>(this->parent());
    //FIX THIS LATER
    //mw->updatePeakListTable();
}

void PeakSearchDialog::onIntensitySpinBoxValueChanged(double arg1)
{
    if (std::abs(arg1 - pkSettings.threshold) >= 0.01) {
        pkSettings.threshold = arg1;        
        peak_search_action(4, &pkSettings);
        ui->numPeaksSpinBox->setValue(pkSettings.numPeaks);
        updatePeakListTable();
    }
}

void PeakSearchDialog::onMinSpinBoxValueChanged(double arg1)
{
    if (std::abs(arg1 - pkSettings.minSearch) > 0.01) {
        if (arg1 > ui->maxSpinBox->value()) {
            ui->minSpinBox->setValue(pkSettings.minSearch);
            return;
        }
        pkSettings.minSearch = arg1;        
        peak_search_action(3, &pkSettings);
        ui->numPeaksSpinBox->setValue(pkSettings.numPeaks);
        updatePeakListTable();
    }
}

void PeakSearchDialog::onMaxSpinBoxValueChanged(double arg1)
{
    if (std::abs(arg1 - pkSettings.maxSearch) > 0.01) {
        if (ui->maxSpinBox->value() < ui->minSpinBox->value()) {
            ui->maxSpinBox->setValue(pkSettings.maxSearch);
            return;
        }
        pkSettings.maxSearch = arg1;        
        peak_search_action(3, &pkSettings);
        ui->numPeaksSpinBox->setValue(pkSettings.numPeaks);
        updatePeakListTable();
    }
}

void PeakSearchDialog::onSensitivitySpinBoxValueChanged(int arg1)
{
    if (arg1 != pkSettings.sensitivity) {
        pkSettings.sensitivity = arg1;        
        peak_search_action(7, &pkSettings);
        ui->numPeaksSpinBox->setValue(pkSettings.numPeaks);
        updatePeakListTable();
    }
}
