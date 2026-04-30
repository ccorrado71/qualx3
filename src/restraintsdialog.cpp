#include "restraintsdialog.h"
#include "ui_restraintsdialog.h"
#include "appstate.h"
#include "periodictablewidget.h"

#include <QCheckBox>
#include <QDialog>
#include <QDialogButtonBox>
#include <QGridLayout>
#include <QHeaderView>
#include <QTableWidget>
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

    connect(ui->symbolListButton,        &QPushButton::clicked,
            this, &RestraintsDialog::onSymbolListClicked);
    connect(ui->availableColorsButton,   &QPushButton::clicked,
            this, &RestraintsDialog::onAvailableColorsClicked);
    connect(ui->helpButton,             &QPushButton::clicked,
            this, &RestraintsDialog::onHelpClicked);
    connect(ui->loadCardsButton,        &QPushButton::clicked,
            this, [this]() { accept(); emit loadCardsRequested(); });
    connect(ui->loadMergeButton,        &QPushButton::clicked,
            this, &RestraintsDialog::loadAndMergeCardsRequested);
    connect(ui->searchButton,           &QPushButton::clicked,
            this, &RestraintsDialog::searchWithRestraintsRequested);
    connect(ui->clearEntryIdsButton,     &QPushButton::clicked,
            this, [this]() { ui->entryIdsEdit->clear(); });
    connect(ui->cancelRestraintsButton, &QPushButton::clicked,
            this, &RestraintsDialog::onCancelAllRestraintsClicked);
    connect(ui->closeButton,            &QPushButton::clicked,
            this, &QDialog::reject);

    setupCompositionTab();
    setupSubfilesTab();

    // Symmetry tab — Clear All / Select All
    const QList<QCheckBox*> csysChecks = {
        ui->checkCubic, ui->checkHexagonalTrigonal, ui->checkMonoclinic,
        ui->checkOrthorhombic, ui->checkTetragonal, ui->checkTriclinic,
        ui->checkRhombohedral
    };
    connect(ui->clearCrystalSystemButton, &QPushButton::clicked, this,
            [csysChecks]() { for (QCheckBox *cb : csysChecks) cb->setChecked(false); });
    connect(ui->selectAllCrystalSystemButton, &QPushButton::clicked, this,
            [csysChecks]() { for (QCheckBox *cb : csysChecks) cb->setChecked(true); });
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

QStringList RestraintsDialog::crystalSystemStrings() const
{
    QStringList result;
    if (ui->checkCubic->isChecked())            result << QStringLiteral("Cubic");
    if (ui->checkHexagonalTrigonal->isChecked()) {
        result << QStringLiteral("Hexagonal");
        result << QStringLiteral("Trigonal (hexagonal axes)");
    }
    if (ui->checkMonoclinic->isChecked())       result << QStringLiteral("Monoclinic");
    if (ui->checkOrthorhombic->isChecked())     result << QStringLiteral("Orthorhombic");
    if (ui->checkTetragonal->isChecked())       result << QStringLiteral("Tetragonal");
    if (ui->checkTriclinic->isChecked())        result << QStringLiteral("Triclinic");
    if (ui->checkRhombohedral->isChecked())     result << QStringLiteral("Trigonal (rhombohedral axes)");
    return result;
}

RestraintsDialog::CellQuery RestraintsDialog::cellQuery() const
{
    CellQuery q;
    const QLineEdit *edits[6] = {
        ui->cellSearch->lineEditA(),  ui->cellSearch->lineEditB(),
        ui->cellSearch->lineEditC(),  ui->cellSearch->lineEditAl(),
        ui->cellSearch->lineEditBe(), ui->cellSearch->lineEditGa()
    };
    bool ok;
    for (int i = 0; i < 6; ++i) {
        const double v = edits[i]->text().trimmed().toDouble(&ok);
        if (ok && v > 0.0)
            q.values[i] = v;
    }
    q.lenTol = ui->cellSearch->doubleSpinLenTol()->value();
    q.angTol = ui->cellSearch->doubleSpinAngTol()->value();
    return q;
}

QStringList RestraintsDialog::spaceGroupStrings() const
{
    QStringList result;
    const QString text = ui->spaceGroupEdit->text().trimmed();
    if (text.isEmpty())
        return result;
    for (const QString &tok : text.split(QLatin1Char(';'), Qt::SkipEmptyParts)) {
        const QString s = tok.trimmed();
        if (!s.isEmpty())
            result << s;
    }
    return result;
}

QStringList RestraintsDialog::entryIds() const
{
    QStringList result;
    const QString text = ui->entryIdsEdit->toPlainText().trimmed();
    if (text.isEmpty())
        return result;
    for (const QString &tok : text.split(QLatin1Char(' '), Qt::SkipEmptyParts))
        result << tok.trimmed();
    return result;
}

QString RestraintsDialog::chemicalName() const
{
    return ui->chemicalNameEdit->text().trimmed();
}

QString RestraintsDialog::colorString() const
{
    return ui->colorEdit->text().trimmed();
}

QStringList RestraintsDialog::colorStrings() const
{
    QStringList result;
    const QString text = ui->colorEdit->text().trimmed();
    if (text.isEmpty())
        return result;
    for (const QString &tok : text.split(QLatin1Char(';'), Qt::SkipEmptyParts)) {
        const QString s = tok.trimmed();
        if (!s.isEmpty())
            result << s;
    }
    return result;
}

double RestraintsDialog::densityCalc() const
{
    bool ok;
    double v = ui->densityCalculatedEdit->text().trimmed().toDouble(&ok);
    return ok ? v : -1.0;
}

double RestraintsDialog::densityMeas() const
{
    bool ok;
    double v = ui->densityMeasuredEdit->text().trimmed().toDouble(&ok);
    return ok ? v : -1.0;
}

double RestraintsDialog::densityTolerance() const
{
    bool ok;
    double v = ui->densityToleranceEdit->text().trimmed().toDouble(&ok);
    return (ok && v >= 0.0) ? v : 0.1;
}

// ---------------------------------------------------------------------------
// Subfiles tab — built in C++
// ---------------------------------------------------------------------------

void RestraintsDialog::setupSubfilesTab()
{
    QWidget     *tab    = ui->tabSubfiles;
    // Remove placeholder spacer inserted by Qt Designer
    qDeleteAll(tab->children());
    QVBoxLayout *layout = new QVBoxLayout(tab);
    layout->setContentsMargins(6, 6, 6, 6);
    layout->setSpacing(8);

    // Helper lambda to register a checkbox
    auto addCheck = [&](QGroupBox *box, QLayout *boxLayout,
                        const QString &label, const QString &code) {
        QCheckBox *cb = new QCheckBox(label, box);
        boxLayout->addWidget(cb);
        m_subfileChecks.append(cb);
        m_subfileCodes.append(code);
    };

    // --- Main Subfiles ---
    QGroupBox   *mainBox    = new QGroupBox(tr("Main Subfiles"), tab);
    QGridLayout *mainLayout = new QGridLayout(mainBox);
    mainLayout->setHorizontalSpacing(20);

    auto addCheckGrid = [&](QGroupBox *box, QGridLayout *grid,
                            const QString &label, const QString &code, int col) {
        QCheckBox *cb = new QCheckBox(label, box);
        grid->addWidget(cb, 0, col);
        m_subfileChecks.append(cb);
        m_subfileCodes.append(code);
    };

    addCheckGrid(mainBox, mainLayout, tr("Inorganic"), QStringLiteral("I"), 0);
    addCheckGrid(mainBox, mainLayout, tr("Organic"),   QStringLiteral("O"), 1);
    addCheckGrid(mainBox, mainLayout, tr("Mineral"),   QStringLiteral("M"), 2);
    for (int col = 0; col < 3; ++col)
        mainLayout->setColumnStretch(col, 1);

    layout->addWidget(mainBox);

    // --- Additional Subfiles (grid, 3 columns) ---
    const QList<QPair<QString,QString>> additional = {
        { tr("Alloy"),               QStringLiteral("A")   },
        { tr("Battery material"),    QStringLiteral("BAT") },
        { tr("Cement"),              QStringLiteral("CEM") },
        { tr("Ceramic"),             QStringLiteral("CER") },
        { tr("Corrosion"),           QStringLiteral("COR") },
        { tr("Coordination Polymer"),QStringLiteral("CP")  },
        { tr("Detected"),            QStringLiteral("DET") },
        { tr("Educational"),         QStringLiteral("EDU") },
        { tr("Experimental"),        QStringLiteral("EXP") },
        { tr("Forensic"),            QStringLiteral("FOR") },
        { tr("Ionic Conductor"),     QStringLiteral("ION") },
        { tr("NBS"),                 QStringLiteral("NBS") },
        { tr("Pharmaceutical"),      QStringLiteral("PHR") },
        { tr("Pigment"),             QStringLiteral("PIG") },
        { tr("Polymer"),             QStringLiteral("POL") },
        { tr("Superconductor"),      QStringLiteral("SCM") },
        { tr("Zeolite"),             QStringLiteral("ZEO") },
    };

    QGroupBox   *addBox    = new QGroupBox(tr("Additional Subfiles"), tab);
    QGridLayout *addLayout = new QGridLayout(addBox);
    addLayout->setHorizontalSpacing(20);
    for (int col = 0; col < 3; ++col)
        addLayout->setColumnStretch(col, 1);

    const int cols = 3;
    for (int i = 0; i < additional.size(); ++i) {
        QCheckBox *cb = new QCheckBox(additional[i].first, addBox);
        addLayout->addWidget(cb, i / cols, i % cols);
        m_subfileChecks.append(cb);
        m_subfileCodes.append(additional[i].second);
    }

    layout->addWidget(addBox);

    // --- Clear All / Select All ---
    QHBoxLayout *btnLayout = new QHBoxLayout();
    QPushButton *clearAll  = new QPushButton(tr("Clear All"),  tab);
    QPushButton *selectAll = new QPushButton(tr("Select All"), tab);
    clearAll->setFixedWidth(100);
    selectAll->setFixedWidth(100);
    btnLayout->addWidget(clearAll);
    btnLayout->addWidget(selectAll);
    btnLayout->addStretch();
    layout->addLayout(btnLayout);

    layout->addStretch();

    connect(clearAll,  &QPushButton::clicked, this, &RestraintsDialog::onSubfilesClearAll);
    connect(selectAll, &QPushButton::clicked, this, &RestraintsDialog::onSubfilesSelectAll);
}

// ---------------------------------------------------------------------------
// Public accessors — Subfiles tab
// ---------------------------------------------------------------------------

bool RestraintsDialog::hasRestraints() const
{
    if (!compositionFormula().isEmpty()) return true;
    if (!chemicalName().isEmpty())       return true;
    if (!subfilesCodes().isEmpty())      return true;
    if (!crystalSystemStrings().isEmpty()) return true;
    if (!spaceGroupStrings().isEmpty())  return true;

    const CellQuery cell = cellQuery();
    for (int i = 0; i < 6; ++i)
        if (cell.values[i] > 0.0) return true;

    if (!ui->entryIdsEdit->toPlainText().trimmed().isEmpty()) return true;

    return false;
}

QStringList RestraintsDialog::subfilesCodes() const
{
    QStringList codes;
    for (int i = 0; i < m_subfileChecks.size(); ++i)
        if (m_subfileChecks[i]->isChecked())
            codes << m_subfileCodes[i];
    return codes;
}

// ---------------------------------------------------------------------------
// Slots — Subfiles tab
// ---------------------------------------------------------------------------

void RestraintsDialog::onSubfilesClearAll()
{
    for (QCheckBox *cb : m_subfileChecks)
        cb->setChecked(false);
}

void RestraintsDialog::onSubfilesSelectAll()
{
    for (QCheckBox *cb : m_subfileChecks)
        cb->setChecked(true);
}

// ---------------------------------------------------------------------------
// Other slots
// ---------------------------------------------------------------------------

void RestraintsDialog::showValuePickerDialog(const QString &title,
                                             const QString &colHeader,
                                             const QList<QPair<QString,int>> &rows,
                                             QLineEdit *targetEdit)
{
    QDialog dlg(this);
    dlg.setWindowTitle(title);
    dlg.resize(380, 500);

    auto *infoLabel = new QLabel(
        tr("Select one or more values to use as filter:"), &dlg);
    infoLabel->setWordWrap(true);

    QTableWidget *table = new QTableWidget(rows.size(), 2, &dlg);
    table->setHorizontalHeaderLabels({ colHeader, tr("Count") });
    table->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
    table->horizontalHeader()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    table->verticalHeader()->setVisible(false);
    table->verticalHeader()->setDefaultSectionSize(22);
    table->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table->setSelectionBehavior(QAbstractItemView::SelectRows);
    table->setSelectionMode(QAbstractItemView::MultiSelection);

    for (int i = 0; i < rows.size(); ++i) {
        auto *valItem = new QTableWidgetItem(rows[i].first);
        auto *cntItem = new QTableWidgetItem(QString::number(rows[i].second));
        cntItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        table->setItem(i, 0, valItem);
        table->setItem(i, 1, cntItem);
    }

    auto *buttons = new QDialogButtonBox(
        QDialogButtonBox::Ok | QDialogButtonBox::Cancel, &dlg);
    connect(buttons, &QDialogButtonBox::accepted, &dlg, &QDialog::accept);
    connect(buttons, &QDialogButtonBox::rejected, &dlg, &QDialog::reject);

    auto *layout = new QVBoxLayout(&dlg);
    layout->addWidget(infoLabel);
    layout->addWidget(table);
    layout->addWidget(buttons);

    if (dlg.exec() != QDialog::Accepted)
        return;

    QStringList selected;
    for (auto *item : table->selectedItems()) {
        if (item->column() == 0)
            selected << item->text();
    }
    if (!selected.isEmpty()) {
        const QString existing = targetEdit->text().trimmed();
        const QString appended = selected.join(QStringLiteral(" ; "));
        targetEdit->setText(
            existing.isEmpty() ? appended
                               : existing + QStringLiteral(" ; ") + appended);
    }
}

void RestraintsDialog::onSymbolListClicked()
{
    showValuePickerDialog(tr("Space Group Symbols"), tr("Space Group"),
                          AppState::db().querySpaceGroups(),
                          ui->spaceGroupEdit);
}

void RestraintsDialog::onAvailableColorsClicked()
{
    showValuePickerDialog(tr("Available Colors"), tr("Color"),
                          AppState::db().queryColors(),
                          ui->colorEdit);
}

void RestraintsDialog::onCancelAllRestraintsClicked()
{
    onCompositionClearClicked();
    onSubfilesClearAll();
    ui->entryIdsEdit->clear();
    ui->chemicalNameEdit->clear();
    ui->colorEdit->clear();
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
