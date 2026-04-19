#include "restraintsdialog.h"
#include "ui_restraintsdialog.h"
#include "periodictablewidget.h"

#include <QGroupBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>
#include <QRadioButton>
#include <QSpinBox>
#include <QVBoxLayout>

RestraintsDialog::RestraintsDialog(QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::RestraintsDialog)
{
    ui->setupUi(this);

    connect(ui->helpButton,             &QPushButton::clicked,
            this, &RestraintsDialog::onHelpClicked);
    connect(ui->loadCardsButton,        &QPushButton::clicked,
            this, [this]() { emit loadCardsRequested(); accept(); });
    connect(ui->loadMergeButton,        &QPushButton::clicked,
            this, &RestraintsDialog::loadAndMergeCardsRequested);
    connect(ui->searchButton,           &QPushButton::clicked,
            this, &RestraintsDialog::searchWithRestraintsRequested);
    connect(ui->cancelRestraintsButton, &QPushButton::clicked,
            this, &RestraintsDialog::onCancelAllRestraintsClicked);
    connect(ui->closeButton,            &QPushButton::clicked,
            this, &QDialog::reject);

    setupCompositionTab();
}

RestraintsDialog::~RestraintsDialog()
{
    delete ui;
}

// ---------------------------------------------------------------------------
// Composition tab — built entirely in C++
// ---------------------------------------------------------------------------

void RestraintsDialog::setupCompositionTab()
{
    QWidget     *tab    = ui->tabComposition;
    QVBoxLayout *layout = new QVBoxLayout(tab);
    layout->setContentsMargins(6, 6, 6, 6);
    layout->setSpacing(6);

    // 1. Periodic table (multi-selection, helpers visible)
    m_periodicTable = new PeriodicTableWidget(
        PeriodicTableWidget::SelectionMode::Multi, tab);
    m_periodicTable->setSelectionHelpersVisible(true);
    layout->addWidget(m_periodicTable);

    // 2. Formula line edit
    m_formulaEdit = new QLineEdit(tab);
    m_formulaEdit->setPlaceholderText(tr("e.g.  Sc AND Ti OR Pm OR Sm"));
    layout->addWidget(m_formulaEdit);

    // 3. Operator group box
    QGroupBox   *opBox    = new QGroupBox(tr("Operator"), tab);
    QHBoxLayout *opLayout = new QHBoxLayout(opBox);

    m_radioAnd         = new QRadioButton(tr("And"),           opBox);
    m_radioOr          = new QRadioButton(tr("Or"),            opBox);
    m_radioNot         = new QRadioButton(tr("Not"),           opBox);
    m_radioExact       = new QRadioButton(tr("Exact elements"),opBox);
    m_radioContainsAny = new QRadioButton(tr("Contains any"),  opBox);

    m_radioAnd->setChecked(true);

    opLayout->addWidget(m_radioAnd);
    opLayout->addWidget(m_radioOr);
    opLayout->addWidget(m_radioNot);
    opLayout->addWidget(m_radioExact);
    opLayout->addWidget(m_radioContainsAny);
    opLayout->addStretch();

    layout->addWidget(opBox);

    // 4. Bottom row: Clear + min/max species
    QHBoxLayout *bottomLayout = new QHBoxLayout();

    m_clearButton = new QPushButton(tr("Clear"), tab);
    m_clearButton->setFixedWidth(80);
    bottomLayout->addWidget(m_clearButton);
    bottomLayout->addStretch();

    bottomLayout->addWidget(new QLabel(tr("Min species:"), tab));
    m_minSpecies = new QSpinBox(tab);
    m_minSpecies->setRange(0, 99);
    m_minSpecies->setValue(0);
    m_minSpecies->setSpecialValueText(tr("any"));
    m_minSpecies->setFixedWidth(60);
    bottomLayout->addWidget(m_minSpecies);

    bottomLayout->addSpacing(12);

    bottomLayout->addWidget(new QLabel(tr("Max species:"), tab));
    m_maxSpecies = new QSpinBox(tab);
    m_maxSpecies->setRange(0, 99);
    m_maxSpecies->setValue(0);
    m_maxSpecies->setSpecialValueText(tr("any"));
    m_maxSpecies->setFixedWidth(60);
    bottomLayout->addWidget(m_maxSpecies);

    layout->addLayout(bottomLayout);

    // --- Connections ---

    connect(m_periodicTable, &PeriodicTableWidget::selectionChanged,
            this, &RestraintsDialog::onCompositionSelectionChanged);

    // Exact elements / Contains any → disable And/Or/Not and rebuild formula with AND
    connect(m_radioExact,       &QRadioButton::toggled,
            this, &RestraintsDialog::onSpecialModeToggled);
    connect(m_radioContainsAny, &QRadioButton::toggled,
            this, &RestraintsDialog::onSpecialModeToggled);

    connect(m_clearButton, &QPushButton::clicked,
            this, &RestraintsDialog::onCompositionClearClicked);
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

QString RestraintsDialog::currentOperator() const
{
    if (m_radioOr->isChecked())  return QStringLiteral("OR");
    if (m_radioNot->isChecked()) return QStringLiteral("NOT");
    return QStringLiteral("AND");   // And / Exact elements / Contains any
}

// ---------------------------------------------------------------------------
// Slots — Composition tab
// ---------------------------------------------------------------------------

void RestraintsDialog::onCompositionSelectionChanged(const QStringList &symbols)
{
    // Detect what changed vs previous state
    QStringList added, removed;
    for (const QString &s : symbols)
        if (!m_lastSelectedSymbols.contains(s))
            added << s;
    for (const QString &s : m_lastSelectedSymbols)
        if (!symbols.contains(s))
            removed << s;

    if (!removed.isEmpty()) {
        // Rebuild formula from the new (smaller) selection
        const QString op = currentOperator();
        QString formula;
        for (const QString &s : symbols) {
            if (!formula.isEmpty())
                formula += QLatin1Char(' ') + op + QLatin1Char(' ');
            formula += s;
        }
        m_formulaEdit->setText(formula);
    } else {
        // Append newly added elements
        const QString op = currentOperator();
        QString formula  = m_formulaEdit->text().trimmed();
        for (const QString &s : added) {
            if (!formula.isEmpty())
                formula += QLatin1Char(' ') + op + QLatin1Char(' ');
            formula += s;
        }
        m_formulaEdit->setText(formula);
    }

    m_lastSelectedSymbols = symbols;
}

void RestraintsDialog::onSpecialModeToggled(bool checked)
{
    // Disable/enable And-Or-Not radios when Exact or ContainsAny is active
    const bool specialActive = (m_radioExact->isChecked() ||
                                m_radioContainsAny->isChecked());
    m_radioAnd->setEnabled(!specialActive);
    m_radioOr->setEnabled(!specialActive);
    m_radioNot->setEnabled(!specialActive);

    // When a special mode becomes active, rebuild the formula using AND
    if (checked && !m_lastSelectedSymbols.isEmpty()) {
        QString formula;
        for (const QString &s : m_lastSelectedSymbols) {
            if (!formula.isEmpty())
                formula += QStringLiteral(" AND ");
            formula += s;
        }
        m_formulaEdit->setText(formula);
    }
}

void RestraintsDialog::onCompositionClearClicked()
{
    m_periodicTable->clearSelection();   // emits selectionChanged({})
    m_formulaEdit->clear();
    m_lastSelectedSymbols.clear();
    m_radioAnd->setChecked(true);
    m_radioAnd->setEnabled(true);
    m_radioOr->setEnabled(true);
    m_radioNot->setEnabled(true);
    m_minSpecies->setValue(0);
    m_maxSpecies->setValue(0);
}

// ---------------------------------------------------------------------------
// Public accessors
// ---------------------------------------------------------------------------

QStringList RestraintsDialog::compositionSymbols() const
{
    return m_periodicTable ? m_periodicTable->selectedSymbols() : QStringList{};
}

QString RestraintsDialog::compositionFormula() const
{
    return m_formulaEdit ? m_formulaEdit->text() : QString{};
}

bool RestraintsDialog::isExactComposition() const
{
    return m_radioExact && m_radioExact->isChecked();
}

bool RestraintsDialog::isContainsAny() const
{
    return m_radioContainsAny && m_radioContainsAny->isChecked();
}

int RestraintsDialog::compositionMinSpecies() const
{
    return m_minSpecies ? m_minSpecies->value() : 0;
}

int RestraintsDialog::compositionMaxSpecies() const
{
    return m_maxSpecies ? m_maxSpecies->value() : 0;
}

QString RestraintsDialog::chemicalName() const
{
    return ui->chemicalNameEdit->text().trimmed();
}

// ---------------------------------------------------------------------------
// Other slots
// ---------------------------------------------------------------------------

void RestraintsDialog::onCancelAllRestraintsClicked()
{
    onCompositionClearClicked();
    emit cancelAllRestraintsRequested();
}

void RestraintsDialog::onHelpClicked()
{
    QMessageBox::information(this, tr("Help"),
        tr("<b>Composition</b>: Filter by chemical elements present in the structure.<br><br>"
           "<b>Subfiles</b>: Filter by subfile category (inorganic, organic, …).<br><br>"
           "<b>Chemical Name</b>: Filter by chemical or mineral name.<br><br>"
           "<b>Entries</b>: Filter by entry ID range.<br><br>"
           "<b>Symmetries</b>: Filter by space group or crystal system.<br><br>"
           "<b>Cell and Properties</b>: Filter by unit cell parameters, density, volume, …"));
}
