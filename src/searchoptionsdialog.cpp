#include "searchoptionsdialog.h"
#include "ui_searchoptionsdialog.h"

#include <QAbstractButton>
#include <QDialogButtonBox>
#include <QDoubleSpinBox>
#include <QMessageBox>
#include <QSettings>
#include <QSignalBlocker>
#include <QSlider>

static constexpr char SK_FOM[]       = "SearchOptions/minFom";
static constexpr char SK_2THETA[]    = "SearchOptions/weight2thetaD";
static constexpr char SK_INT[]       = "SearchOptions/weightIntensity";
static constexpr char SK_PHASES[]    = "SearchOptions/weightPhases";
static constexpr char SK_DELTA[]     = "SearchOptions/delta2theta";
static constexpr char SK_AUTO[]      = "SearchOptions/delta2thetaAuto";
static constexpr char SK_RESIDUAL[]  = "SearchOptions/residualSearching";
static constexpr char SK_STRONGEST[] = "SearchOptions/checkStrongest";
static constexpr char SK_DELETED[]   = "SearchOptions/checkDeleted";
static constexpr char SK_MAXENT[]    = "SearchOptions/maxEntries";

static void wireSliderSpin(QSlider *sl, QDoubleSpinBox *sp)
{
    QObject::connect(sl, &QSlider::valueChanged, sp, [sp](int v) {
        QSignalBlocker b(sp);
        sp->setValue(v / 100.0);
    });
    QObject::connect(sp, &QDoubleSpinBox::valueChanged, sl, [sl](double v) {
        QSignalBlocker b(sl);
        sl->setValue(qRound(v * 100.0));
    });
}

SearchOptionsDialog::SearchOptionsDialog(QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::SearchOptionsDialog)
{
    ui->setupUi(this);

    wireSliderSpin(ui->sliderFom,       ui->spinFom);
    wireSliderSpin(ui->sliderTwothetaD, ui->spinTwothetaD);
    wireSliderSpin(ui->sliderIntensity, ui->spinIntensity);
    wireSliderSpin(ui->sliderPhases,    ui->spinPhases);
    wireSliderSpin(ui->sliderDelta,     ui->spinDelta);

    connect(ui->checkAuto, &QCheckBox::toggled,
            this, &SearchOptionsDialog::onAutoToggled);

    connect(ui->buttonBox, &QDialogButtonBox::accepted, this, [this]() {
        saveSettings();
        accept();
    });
    connect(ui->buttonBox, &QDialogButtonBox::rejected,
            this, &QDialog::reject);
    connect(ui->buttonBox, &QDialogButtonBox::helpRequested, this, []() {
        QMessageBox::information(nullptr, QObject::tr("Help"),
            QObject::tr("Adjust the weights and options used during database search.\n\n"
                        "Min. FOM: minimum figure of merit threshold.\n"
                        "2θ/d: relative weight of 2theta/d-spacing matching.\n"
                        "Intensity: relative weight of intensity matching.\n"
                        "Phases: bias towards single- or multi-phase solutions.\n"
                        "Δ2θ: 2theta tolerance (use Auto for automatic selection)."));
    });
    connect(ui->buttonBox, &QDialogButtonBox::clicked,
            this, &SearchOptionsDialog::onButtonClicked);

    loadSettings();
}

SearchOptionsDialog::~SearchOptionsDialog()
{
    delete ui;
}

// ---------------------------------------------------------------------------
// Slots
// ---------------------------------------------------------------------------

void SearchOptionsDialog::onAutoToggled(bool checked)
{
    ui->sliderDelta->setDisabled(checked);
    ui->spinDelta->setDisabled(checked);
}

void SearchOptionsDialog::onButtonClicked(QAbstractButton *button)
{
    if (ui->buttonBox->buttonRole(button) == QDialogButtonBox::ResetRole)
        resetToDefaults();
}

// ---------------------------------------------------------------------------
// Settings
// ---------------------------------------------------------------------------

void SearchOptionsDialog::resetToDefaults()
{
    ui->spinFom->setValue(0.35);
    ui->spinTwothetaD->setValue(0.50);
    ui->spinIntensity->setValue(0.50);
    ui->spinPhases->setValue(0.50);
    ui->spinDelta->setValue(0.08);
    ui->checkAuto->setChecked(true);
    ui->checkResidual->setChecked(true);
    ui->checkStrongest->setChecked(true);
    ui->checkDeleted->setChecked(true);
    ui->lineEditMaxEntries->setText(QStringLiteral("3000"));
}

void SearchOptionsDialog::loadSettings()
{
    QSettings s;
    ui->spinFom->setValue(      s.value(SK_FOM,       0.35).toDouble());
    ui->spinTwothetaD->setValue(s.value(SK_2THETA,    0.50).toDouble());
    ui->spinIntensity->setValue(s.value(SK_INT,       0.50).toDouble());
    ui->spinPhases->setValue(   s.value(SK_PHASES,    0.50).toDouble());
    ui->spinDelta->setValue(    s.value(SK_DELTA,     0.08).toDouble());
    ui->checkAuto->setChecked(  s.value(SK_AUTO,      true).toBool());
    ui->checkResidual->setChecked( s.value(SK_RESIDUAL,  true).toBool());
    ui->checkStrongest->setChecked(s.value(SK_STRONGEST, true).toBool());
    ui->checkDeleted->setChecked(  s.value(SK_DELETED,   true).toBool());
    ui->lineEditMaxEntries->setText(s.value(SK_MAXENT, 3000).toString());
}

void SearchOptionsDialog::saveSettings()
{
    QSettings s;
    s.setValue(SK_FOM,       ui->spinFom->value());
    s.setValue(SK_2THETA,    ui->spinTwothetaD->value());
    s.setValue(SK_INT,       ui->spinIntensity->value());
    s.setValue(SK_PHASES,    ui->spinPhases->value());
    s.setValue(SK_DELTA,     ui->spinDelta->value());
    s.setValue(SK_AUTO,      ui->checkAuto->isChecked());
    s.setValue(SK_RESIDUAL,  ui->checkResidual->isChecked());
    s.setValue(SK_STRONGEST, ui->checkStrongest->isChecked());
    s.setValue(SK_DELETED,   ui->checkDeleted->isChecked());
    s.setValue(SK_MAXENT,    ui->lineEditMaxEntries->text());
}

// ---------------------------------------------------------------------------
// Accessors
// ---------------------------------------------------------------------------

double SearchOptionsDialog::savedMinFom()    { return QSettings().value(SK_FOM,    0.35).toDouble(); }
int    SearchOptionsDialog::savedMaxEntries() { return QSettings().value(SK_MAXENT, 3000).toInt(); }

double SearchOptionsDialog::minFom()              const { return ui->spinFom->value(); }
double SearchOptionsDialog::weight2thetaD()       const { return ui->spinTwothetaD->value(); }
double SearchOptionsDialog::weightIntensity()     const { return ui->spinIntensity->value(); }
double SearchOptionsDialog::weightPhases()        const { return ui->spinPhases->value(); }
double SearchOptionsDialog::delta2theta()         const { return ui->spinDelta->value(); }
bool   SearchOptionsDialog::delta2thetaAuto()     const { return ui->checkAuto->isChecked(); }
bool   SearchOptionsDialog::residualSearching()   const { return ui->checkResidual->isChecked(); }
bool   SearchOptionsDialog::checkStrongestPeaks() const { return ui->checkStrongest->isChecked(); }
bool   SearchOptionsDialog::checkDeletedCards()   const { return ui->checkDeleted->isChecked(); }
int    SearchOptionsDialog::maxEntries()          const { return ui->lineEditMaxEntries->text().toInt(); }
