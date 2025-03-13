#include "backgrounddialog.h"
#include "ui_backgrounddialog.h"
#include "mainwindow.h"

#include <QPushButton>

extern "C" void modify_background(BackgroundSettings *backSettings, int kaction);

BackgroundDialog::BackgroundDialog(QWidget *parent) :
    QDialog(parent),    
    ui(new Ui::BackgroundDialog)
{
    ui->setupUi(this);

    ui->chebyshevWidget->setMaximumCoef(36);
    ui->polynomialWidget->setMaximumCoef(12);
    ui->fourierWidget->setMaximumCoef(12);

    bkSettings[Chebyshev] = {Chebyshev,true,0,0,0,0,0};
    bkSettings[Polynomial] = {Polynomial,true,0,0,0,0,0};
    bkSettings[Cosine_Fourier] = {Cosine_Fourier,true,0,0,0,0,0};
    bkSettings[Cubic_Spline] = {Cubic_Spline,false,0,0,0,0,0};
    bkSettings[Bezier_Spline] = {Bezier_Spline,false,0,0,0,0,0};
    bkSettings[Filter] = {Filter,false,6,5,50,0,0};
    bkSettings[None] = {None,false,0,0,0,0,0};

    connect(ui->chebyshevWidget, &BackgroundCoefWidget::autoCoefChanged, this, &BackgroundDialog::applyAutoChanged);
    connect(ui->chebyshevWidget, &BackgroundCoefWidget::ncoefChanged, this, &BackgroundDialog::applyNcoefChanged);
    connect(ui->polynomialWidget, &BackgroundCoefWidget::autoCoefChanged, this, &BackgroundDialog::applyAutoChanged);
    connect(ui->polynomialWidget, &BackgroundCoefWidget::ncoefChanged, this, &BackgroundDialog::applyNcoefChanged);
    connect(ui->fourierWidget, &BackgroundCoefWidget::autoCoefChanged, this, &BackgroundDialog::applyAutoChanged);
    connect(ui->fourierWidget, &BackgroundCoefWidget::ncoefChanged, this, &BackgroundDialog::applyNcoefChanged);
    connect(ui->resetFilterButton, &QPushButton::clicked, this, &BackgroundDialog::onResetFilterButtonClicked);
    connect(ui->applyFilterButton, &QPushButton::clicked, this, &BackgroundDialog::onApplyFilterButtonClicked);
    connect(ui->typeComboBox, &QComboBox::currentIndexChanged, this, &BackgroundDialog::onTypeComboBoxCurrentIndexChanged);
    connect(ui->buttonBox, &QDialogButtonBox::clicked, this, &BackgroundDialog::onButtonBoxClicked);
}

BackgroundDialog::~BackgroundDialog()
{
    delete ui;
}

void BackgroundDialog::setBackground()
{
    modify_background(&bkCurrentSettings,0);
    setWidgets(bkCurrentSettings);
    ui->minSpinBox->setRange(bkCurrentSettings.minf,bkCurrentSettings.maxf);
    ui->maxSpinBox->setRange(bkCurrentSettings.minf,bkCurrentSettings.maxf);
    bkSavedSettings = bkCurrentSettings;
    bkSettings[Filter].minf = bkCurrentSettings.minf;
    bkSettings[Filter].maxf = bkCurrentSettings.maxf;
    oldIndex = bkCurrentSettings.btype;

    //Set menu only if parent is a MainWindow
    MainWindow *mw = qobject_cast<MainWindow *>(this->parent());
    if (mw) {
        mw->saveEnabledActions();
        mw->enableActions(MainWindow::DialogOpenAction);
    }
}

void BackgroundDialog::onTypeComboBoxCurrentIndexChanged(int index)
{
    if (index == Cubic_Spline || index == Bezier_Spline || index == None) {
        enableBkWidget(false, oldIndex);
    } else {
        enableBkWidget(true, index);
        ui->stackedWidget->setCurrentIndex(index);
        oldIndex = index;
    }
    bkSettings[bkCurrentSettings.btype] = bkCurrentSettings;
    bkCurrentSettings = bkSettings[index];
    modify_background(&bkCurrentSettings,1);
    setPage(bkCurrentSettings);    
}

void BackgroundDialog::setPage(const BackgroundSettings &bSettings)
{    
    if (bSettings.btype == Chebyshev) {        
        if (bSettings.autob)
            ui->chebyshevWidget->setAutob(true);
        else
            ui->chebyshevWidget->setAutob(false);
        ui->chebyshevWidget->setNcoef(bSettings.ncoef);
    }

    if (bSettings.btype == Polynomial) {
        if (bSettings.autob)
            ui->polynomialWidget->setAutob(true);
        else
            ui->polynomialWidget->setAutob(false);
        ui->polynomialWidget->setNcoef(bSettings.ncoef);
    }

    if (bSettings.btype == Cosine_Fourier) {
        if (bSettings.autob)
            ui->fourierWidget->setAutob(true);
        else
            ui->fourierWidget->setAutob(false);
        ui->fourierWidget->setNcoef(bSettings.ncoef);
    }

    if (bSettings.btype == Filter) {
        ui->iterationsSpinBox->setValue(bSettings.niterf);
        ui->windowSizeSpinBox->setValue(bSettings.nwinf);
        ui->minSpinBox->setValue(bSettings.minf);
        ui->maxSpinBox->setValue(bSettings.maxf);
    }
}

void BackgroundDialog::enableBkWidget(bool enabled, int index)
{
    if (index == Chebyshev) ui->chebyshevWidget->setEnabled(enabled);
    if (index == Polynomial) ui->polynomialWidget->setEnabled(enabled);
    if (index == Cosine_Fourier) ui->fourierWidget->setEnabled(enabled);
}

void BackgroundDialog::setWidgets(const BackgroundSettings &bSettings)
{    
    int index = ui->typeComboBox->currentIndex();
    ui->typeComboBox->setCurrentIndex(bSettings.btype);
    if (index == bkCurrentSettings.btype) setPage(bkCurrentSettings); // in other case setPage is called by setCurrentIndex
}

void BackgroundDialog::applyAutoChanged()
{
    BackgroundCoefWidget *widget = qobject_cast<BackgroundCoefWidget*>(sender());
    bkCurrentSettings.autob = widget->getAutob();
    if (bkCurrentSettings.autob) {
        modify_background(&bkCurrentSettings,1);
        widget->setNcoef(bkCurrentSettings.ncoef);
    }
}

void BackgroundDialog::applyNcoefChanged()
{
    if (bkCurrentSettings.autob) return;

    BackgroundCoefWidget *widget = qobject_cast<BackgroundCoefWidget*>(sender());
    qInfo() << "Changed Applied, Ncoef: " << widget->getNcoef();
    bkCurrentSettings.ncoef = widget->getNcoef();
    modify_background(&bkCurrentSettings,1);
}

void BackgroundDialog::accept()
{
#ifndef Q_OS_MACOS
    //'ok' get focus if you hide the dialog. This avoid that enter key close the dialog
    if (!ui->buttonBox->button(QDialogButtonBox::Ok)->hasFocus()) return;
#endif

    QDialog::accept();
}

void BackgroundDialog::reject()
{
    restoreSavedSettings();    
    MainWindow *mw = qobject_cast<MainWindow *>(this->parent());
    if (mw)
        mw->restoreEnabledActions();

    QDialog::reject();
}

void BackgroundDialog::restoreSavedSettings()
{
    bkCurrentSettings = bkSavedSettings;
    setWidgets(bkCurrentSettings);
    modify_background(&bkCurrentSettings,1);
}

void BackgroundDialog::onButtonBoxClicked(QAbstractButton *button)
{
    QDialogButtonBox::StandardButton stdButton = ui->buttonBox->standardButton(button);

    if(stdButton == QDialogButtonBox::Ok) {
        MainWindow *mw = qobject_cast<MainWindow *>(this->parent());
        if (mw)
            mw->restoreEnabledActions();

    } else if (stdButton == QDialogButtonBox::Cancel) {
        //restoreSavedSettings();

    } else if (stdButton == QDialogButtonBox::RestoreDefaults) {
        bkCurrentSettings = {Chebyshev, true, 0, 0 ,0, 0, 0};
        modify_background(&bkCurrentSettings,1);
        setWidgets(bkCurrentSettings);
    }
}

void BackgroundDialog::onApplyFilterButtonClicked()
{
    bkCurrentSettings.niterf = ui->iterationsSpinBox->value();
    bkCurrentSettings.nwinf = ui->iterationsSpinBox->value();
    if (ui->minSpinBox->value() < ui->maxSpinBox->value()) {
        bkCurrentSettings.minf = ui->minSpinBox->value();
        bkCurrentSettings.maxf = ui->maxSpinBox->value();
    } else {
        ui->minSpinBox->setValue(bkCurrentSettings.minf);
        ui->maxSpinBox->setValue(bkCurrentSettings.maxf);
    }
    QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
    modify_background(&bkCurrentSettings,3);
    QApplication::restoreOverrideCursor();
}

void BackgroundDialog::onResetFilterButtonClicked()
{
    QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
    modify_background(&bkCurrentSettings,1);
    QApplication::restoreOverrideCursor();
}
