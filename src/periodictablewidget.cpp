#include "periodictablewidget.h"
#include "elementbutton.h"
#include <QGridLayout>
#include <QLabel>
#include <QPushButton>
#include <algorithm>

// ---------------------------------------------------------------------------
// Dati degli elementi
// ---------------------------------------------------------------------------
namespace {

struct ElementData {
    int         atomicNumber;
    const char* symbol;
    const char* name;
    int         row; // riga base (0-based): 0-6 = periodi 1-7, 8 = lantanidi, 9 = attinidi
    int         col; // colonna base (0-based, 0-17)
    const char* category;
};

// In modalità Multi il layout usa un offset (+1 riga, +1 colonna):
//   Riga 0         → tasti gruppo G1-G18
//   Col  0         → tasti periodo P1-P7
//   Righe 1-7      → Periodi 1-7
//   Riga  8        → separatore
//   Riga  9, col 2 → tasto serie lantanidi; col 3-17 → La-Lu
//   Riga 10, col 2 → tasto serie attinidi;  col 3-17 → Ac-Lr

static const ElementData kElements[] = {
    // Periodo 1
    {1,   "H",  "Hydrogen",        0,  0,  "nonmetal"},
    {2,   "He", "Helium",          0,  17, "noble_gas"},
    // Periodo 2
    {3,   "Li", "Lithium",         1,  0,  "alkali_metal"},
    {4,   "Be", "Beryllium",       1,  1,  "alkaline_earth"},
    {5,   "B",  "Boron",           1,  12, "metalloid"},
    {6,   "C",  "Carbon",          1,  13, "nonmetal"},
    {7,   "N",  "Nitrogen",        1,  14, "nonmetal"},
    {8,   "O",  "Oxygen",          1,  15, "nonmetal"},
    {9,   "F",  "Fluorine",        1,  16, "halogen"},
    {10,  "Ne", "Neon",            1,  17, "noble_gas"},
    // Periodo 3
    {11,  "Na", "Sodium",          2,  0,  "alkali_metal"},
    {12,  "Mg", "Magnesium",       2,  1,  "alkaline_earth"},
    {13,  "Al", "Aluminium",       2,  12, "post_transition"},
    {14,  "Si", "Silicon",         2,  13, "metalloid"},
    {15,  "P",  "Phosphorus",      2,  14, "nonmetal"},
    {16,  "S",  "Sulfur",          2,  15, "nonmetal"},
    {17,  "Cl", "Chlorine",        2,  16, "halogen"},
    {18,  "Ar", "Argon",           2,  17, "noble_gas"},
    // Periodo 4
    {19,  "K",  "Potassium",       3,  0,  "alkali_metal"},
    {20,  "Ca", "Calcium",         3,  1,  "alkaline_earth"},
    {21,  "Sc", "Scandium",        3,  2,  "transition_metal"},
    {22,  "Ti", "Titanium",        3,  3,  "transition_metal"},
    {23,  "V",  "Vanadium",        3,  4,  "transition_metal"},
    {24,  "Cr", "Chromium",        3,  5,  "transition_metal"},
    {25,  "Mn", "Manganese",       3,  6,  "transition_metal"},
    {26,  "Fe", "Iron",            3,  7,  "transition_metal"},
    {27,  "Co", "Cobalt",          3,  8,  "transition_metal"},
    {28,  "Ni", "Nickel",          3,  9,  "transition_metal"},
    {29,  "Cu", "Copper",          3,  10, "transition_metal"},
    {30,  "Zn", "Zinc",            3,  11, "transition_metal"},
    {31,  "Ga", "Gallium",         3,  12, "post_transition"},
    {32,  "Ge", "Germanium",       3,  13, "metalloid"},
    {33,  "As", "Arsenic",         3,  14, "metalloid"},
    {34,  "Se", "Selenium",        3,  15, "nonmetal"},
    {35,  "Br", "Bromine",         3,  16, "halogen"},
    {36,  "Kr", "Krypton",         3,  17, "noble_gas"},
    // Periodo 5
    {37,  "Rb", "Rubidium",        4,  0,  "alkali_metal"},
    {38,  "Sr", "Strontium",       4,  1,  "alkaline_earth"},
    {39,  "Y",  "Yttrium",         4,  2,  "transition_metal"},
    {40,  "Zr", "Zirconium",       4,  3,  "transition_metal"},
    {41,  "Nb", "Niobium",         4,  4,  "transition_metal"},
    {42,  "Mo", "Molybdenum",      4,  5,  "transition_metal"},
    {43,  "Tc", "Technetium",      4,  6,  "transition_metal"},
    {44,  "Ru", "Ruthenium",       4,  7,  "transition_metal"},
    {45,  "Rh", "Rhodium",         4,  8,  "transition_metal"},
    {46,  "Pd", "Palladium",       4,  9,  "transition_metal"},
    {47,  "Ag", "Silver",          4,  10, "transition_metal"},
    {48,  "Cd", "Cadmium",         4,  11, "transition_metal"},
    {49,  "In", "Indium",          4,  12, "post_transition"},
    {50,  "Sn", "Tin",             4,  13, "post_transition"},
    {51,  "Sb", "Antimony",        4,  14, "metalloid"},
    {52,  "Te", "Tellurium",       4,  15, "metalloid"},
    {53,  "I",  "Iodine",          4,  16, "halogen"},
    {54,  "Xe", "Xenon",           4,  17, "noble_gas"},
    // Periodo 6  (La-Lu → riga 8)
    {55,  "Cs", "Caesium",         5,  0,  "alkali_metal"},
    {56,  "Ba", "Barium",          5,  1,  "alkaline_earth"},
    {72,  "Hf", "Hafnium",         5,  3,  "transition_metal"},
    {73,  "Ta", "Tantalum",        5,  4,  "transition_metal"},
    {74,  "W",  "Tungsten",        5,  5,  "transition_metal"},
    {75,  "Re", "Rhenium",         5,  6,  "transition_metal"},
    {76,  "Os", "Osmium",          5,  7,  "transition_metal"},
    {77,  "Ir", "Iridium",         5,  8,  "transition_metal"},
    {78,  "Pt", "Platinum",        5,  9,  "transition_metal"},
    {79,  "Au", "Gold",            5,  10, "transition_metal"},
    {80,  "Hg", "Mercury",         5,  11, "transition_metal"},
    {81,  "Tl", "Thallium",        5,  12, "post_transition"},
    {82,  "Pb", "Lead",            5,  13, "post_transition"},
    {83,  "Bi", "Bismuth",         5,  14, "post_transition"},
    {84,  "Po", "Polonium",        5,  15, "metalloid"},
    {85,  "At", "Astatine",        5,  16, "halogen"},
    {86,  "Rn", "Radon",           5,  17, "noble_gas"},
    // Periodo 7  (Ac-Lr → riga 9)
    {87,  "Fr", "Francium",        6,  0,  "alkali_metal"},
    {88,  "Ra", "Radium",          6,  1,  "alkaline_earth"},
    {104, "Rf", "Rutherfordium",   6,  3,  "transition_metal"},
    {105, "Db", "Dubnium",         6,  4,  "transition_metal"},
    {106, "Sg", "Seaborgium",      6,  5,  "transition_metal"},
    {107, "Bh", "Bohrium",         6,  6,  "transition_metal"},
    {108, "Hs", "Hassium",         6,  7,  "transition_metal"},
    {109, "Mt", "Meitnerium",      6,  8,  "unknown"},
    {110, "Ds", "Darmstadtium",    6,  9,  "unknown"},
    {111, "Rg", "Roentgenium",     6,  10, "unknown"},
    {112, "Cn", "Copernicium",     6,  11, "transition_metal"},
    {113, "Nh", "Nihonium",        6,  12, "unknown"},
    {114, "Fl", "Flerovium",       6,  13, "unknown"},
    {115, "Mc", "Moscovium",       6,  14, "unknown"},
    {116, "Lv", "Livermorium",     6,  15, "unknown"},
    {117, "Ts", "Tennessine",      6,  16, "unknown"},
    {118, "Og", "Oganesson",       6,  17, "unknown"},
    // Lantanidi (riga 8, col 2-16)
    {57,  "La", "Lanthanum",       8,  2,  "lanthanide"},
    {58,  "Ce", "Cerium",          8,  3,  "lanthanide"},
    {59,  "Pr", "Praseodymium",    8,  4,  "lanthanide"},
    {60,  "Nd", "Neodymium",       8,  5,  "lanthanide"},
    {61,  "Pm", "Promethium",      8,  6,  "lanthanide"},
    {62,  "Sm", "Samarium",        8,  7,  "lanthanide"},
    {63,  "Eu", "Europium",        8,  8,  "lanthanide"},
    {64,  "Gd", "Gadolinium",      8,  9,  "lanthanide"},
    {65,  "Tb", "Terbium",         8,  10, "lanthanide"},
    {66,  "Dy", "Dysprosium",      8,  11, "lanthanide"},
    {67,  "Ho", "Holmium",         8,  12, "lanthanide"},
    {68,  "Er", "Erbium",          8,  13, "lanthanide"},
    {69,  "Tm", "Thulium",         8,  14, "lanthanide"},
    {70,  "Yb", "Ytterbium",       8,  15, "lanthanide"},
    {71,  "Lu", "Lutetium",        8,  16, "lanthanide"},
    // Attinidi (riga 9, col 2-16)
    {89,  "Ac", "Actinium",        9,  2,  "actinide"},
    {90,  "Th", "Thorium",         9,  3,  "actinide"},
    {91,  "Pa", "Protactinium",    9,  4,  "actinide"},
    {92,  "U",  "Uranium",         9,  5,  "actinide"},
    {93,  "Np", "Neptunium",       9,  6,  "actinide"},
    {94,  "Pu", "Plutonium",       9,  7,  "actinide"},
    {95,  "Am", "Americium",       9,  8,  "actinide"},
    {96,  "Cm", "Curium",          9,  9,  "actinide"},
    {97,  "Bk", "Berkelium",       9,  10, "actinide"},
    {98,  "Cf", "Californium",     9,  11, "actinide"},
    {99,  "Es", "Einsteinium",     9,  12, "actinide"},
    {100, "Fm", "Fermium",         9,  13, "actinide"},
    {101, "Md", "Mendelevium",     9,  14, "actinide"},
    {102, "No", "Nobelium",        9,  15, "actinide"},
    {103, "Lr", "Lawrencium",      9,  16, "actinide"},
};

QColor colorForCategory(const char* category)
{
    const QLatin1String cat(category);
    if (cat == QLatin1String("alkali_metal"))     return QColor(0xFF, 0x66, 0x66);
    if (cat == QLatin1String("alkaline_earth"))   return QColor(0xFF, 0xDE, 0xAD);
    if (cat == QLatin1String("transition_metal")) return QColor(0xFF, 0xC0, 0xA0);
    if (cat == QLatin1String("post_transition"))  return QColor(0xC0, 0xC0, 0xC0);
    if (cat == QLatin1String("metalloid"))        return QColor(0xCC, 0xCC, 0x66);
    if (cat == QLatin1String("nonmetal"))         return QColor(0x80, 0xFF, 0x80);
    if (cat == QLatin1String("halogen"))          return QColor(0x80, 0xFF, 0xFF);
    if (cat == QLatin1String("noble_gas"))        return QColor(0xC0, 0xBF, 0xFF);
    if (cat == QLatin1String("lanthanide"))       return QColor(0xFF, 0xBF, 0xFF);
    if (cat == QLatin1String("actinide"))         return QColor(0xFF, 0x99, 0xAB);
    return QColor(0xE0, 0xE0, 0xE0);
}

} // namespace

// ---------------------------------------------------------------------------
// PeriodicTableWidget
// ---------------------------------------------------------------------------

PeriodicTableWidget::PeriodicTableWidget(SelectionMode mode, QWidget* parent)
    : QWidget(parent)
    , m_selectionMode(mode)
{
    setupLayout();
}

void PeriodicTableWidget::setupLayout()
{
    // In modalità Multi si lascia spazio per i tasti helper (offset +1,+1).
    // In modalità Single nessun offset: layout identico alla versione originale.
    const int rOff = (m_selectionMode == SelectionMode::Multi) ? 1 : 0;
    const int cOff = (m_selectionMode == SelectionMode::Multi) ? 1 : 0;

    auto* grid = new QGridLayout(this);
    grid->setSpacing(2);
    grid->setContentsMargins(4, 4, 4, 4);

    // Pulsanti elementi
    for (const auto& e : kElements) {
        const QString sym = QLatin1String(e.symbol);
        auto* btn = new ElementButton(e.atomicNumber, sym, e.name,
                                      colorForCategory(e.category), this);
        connect(btn, &ElementButton::elementClicked,
                this, &PeriodicTableWidget::onElementClicked);
        m_buttons[sym] = btn;
        grid->addWidget(btn, e.row + rOff, e.col + cOff);

        // Mappe periodo/gruppo per i tasti di selezione rapida
        if (m_selectionMode == SelectionMode::Multi) {
            int period;
            if (e.row <= 6)       period = e.row + 1;
            else if (e.row == 8)  period = 6;  // lantanidi → periodo 6
            else                  period = 7;  // attinidi  → periodo 7
            m_periodElements[period] << sym;

            if (e.row <= 6)  // solo tavola principale (non f-block)
                m_groupElements[e.col + 1] << sym;

            const QLatin1String cat(e.category);
            if (cat == QLatin1String("lanthanide"))
                m_lanthanideSymbols << sym;
            else if (cat == QLatin1String("actinide"))
                m_actinideSymbols << sym;
        }
    }

    // Placeholder per serie lantanidi/attinidi nelle celle del periodo 6-7 col 3
    auto makePlaceholder = [](const QString& text, const QColor& color) -> QLabel* {
        auto* lbl = new QLabel(text);
        lbl->setFixedSize(40, 40);
        lbl->setAlignment(Qt::AlignCenter);
        lbl->setStyleSheet(
            QString("background-color: %1; border: 1px solid #808080; font-size: 7px;")
            .arg(color.name()));
        return lbl;
    };
    grid->addWidget(makePlaceholder("57-71\n*",   colorForCategory("lanthanide")),
                    5 + rOff, 2 + cOff);
    grid->addWidget(makePlaceholder("89-103\n**", colorForCategory("actinide")),
                    6 + rOff, 2 + cOff);

    // Separatore tra tavola principale e serie f
    auto* sep = new QWidget(this);
    sep->setFixedHeight(8);
    const int totalCols = 18 + cOff;
    grid->addWidget(sep, 7 + rOff, 0, 1, totalCols);

    // Tasti helper (solo Multi, inizialmente nascosti)
    if (m_selectionMode == SelectionMode::Multi)
        setupHelpers(grid);
}

// ---------------------------------------------------------------------------
// Helper buttons
// ---------------------------------------------------------------------------

QPushButton* PeriodicTableWidget::createSelectorButton(const QString& label, const QColor& color)
{
    auto* btn = new QPushButton(label, this);
    btn->setFixedSize(40, 40);
    btn->setCursor(Qt::PointingHandCursor);
    btn->setStyleSheet(
        QString("QPushButton { background-color: %1; border: 1px solid #505050;"
                " font-size: 8px; font-weight: bold; }"
                "QPushButton:hover { background-color: %2; }"
                "QPushButton:pressed { background-color: %3; }")
        .arg(color.name())
        .arg(color.darker(115).name())
        .arg(color.darker(130).name()));
    return btn;
}

void PeriodicTableWidget::setupHelpers(QGridLayout* grid)
{
    const QColor periodColor(0xA0, 0xC8, 0xE8);  // blu chiaro
    const QColor groupColor (0xA0, 0xC8, 0xE8);
    const QColor laColor    = colorForCategory("lanthanide").darker(110);
    const QColor acColor    = colorForCategory("actinide").darker(110);

    // --- Tasti periodo P1-P7  (col 0, righe 1-7) ---
    for (int p = 1; p <= 7; ++p) {
        auto* btn = createSelectorButton(QString("P%1").arg(p), periodColor);
        btn->setToolTip(QString("Seleziona tutti gli elementi del periodo %1").arg(p));
        connect(btn, &QPushButton::clicked, this, [this, p]() {
            toggleSelection(m_periodElements.value(p));
        });
        grid->addWidget(btn, p, 0);
        m_helperWidgets << btn;
    }

    // --- Tasti gruppo G1-G18  (riga 0, col 1-18) ---
    for (int g = 1; g <= 18; ++g) {
        auto* btn = createSelectorButton(QString::number(g), groupColor);
        btn->setToolTip(QString("Seleziona tutti gli elementi del gruppo %1").arg(g));
        connect(btn, &QPushButton::clicked, this, [this, g]() {
            toggleSelection(m_groupElements.value(g));
        });
        grid->addWidget(btn, 0, g);
        m_helperWidgets << btn;
    }

    // --- Tasto serie lantanidi  (riga 9, col 2) ---
    auto* laBtn = createSelectorButton("La\n*", laColor);
    laBtn->setToolTip("Seleziona tutti i lantanidi (57-71)");
    connect(laBtn, &QPushButton::clicked, this, [this]() {
        toggleSelection(m_lanthanideSymbols);
    });
    grid->addWidget(laBtn, 9, 2);   // riga 8+1, col 2 (prima di La a col 3)
    m_helperWidgets << laBtn;

    // --- Tasto serie attinidi  (riga 10, col 2) ---
    auto* acBtn = createSelectorButton("Ac\n**", acColor);
    acBtn->setToolTip("Seleziona tutti gli attinidi (89-103)");
    connect(acBtn, &QPushButton::clicked, this, [this]() {
        toggleSelection(m_actinideSymbols);
    });
    grid->addWidget(acBtn, 10, 2);  // riga 9+1, col 2
    m_helperWidgets << acBtn;

    // Nascosti di default
    for (auto* w : m_helperWidgets)
        w->setVisible(false);
}

void PeriodicTableWidget::setSelectionHelpersVisible(bool visible)
{
    if (m_selectionMode != SelectionMode::Multi || m_helpersVisible == visible)
        return;
    m_helpersVisible = visible;
    for (auto* w : m_helperWidgets)
        w->setVisible(visible);
}

// ---------------------------------------------------------------------------
// Logica di selezione
// ---------------------------------------------------------------------------

void PeriodicTableWidget::toggleSelection(const QStringList& symbols)
{
    bool allSelected = std::all_of(symbols.cbegin(), symbols.cend(),
        [this](const QString& s) {
            auto it = m_buttons.constFind(s);
            return it != m_buttons.cend() && it.value()->isElementSelected();
        });

    for (const QString& s : symbols) {
        if (m_buttons.contains(s))
            m_buttons[s]->setElementSelected(!allSelected);
    }
    emit selectionChanged(selectedSymbols());
}

QStringList PeriodicTableWidget::selectedSymbols() const
{
    QStringList result;
    for (auto it = m_buttons.cbegin(); it != m_buttons.cend(); ++it) {
        if (it.value()->isElementSelected())
            result << it.key();
    }
    result.sort();
    return result;
}

void PeriodicTableWidget::clearSelection()
{
    for (auto* btn : m_buttons)
        btn->setElementSelected(false);
    m_lastSelected.clear();
    emit selectionChanged({});
}

void PeriodicTableWidget::onElementClicked(const QString& symbol, int atomicNumber)
{
    if (m_selectionMode == SelectionMode::Single) {
        if (m_lastSelected == symbol) {
            m_buttons[symbol]->setElementSelected(false);
            m_lastSelected.clear();
            emit selectionChanged({});
        } else {
            if (!m_lastSelected.isEmpty() && m_buttons.contains(m_lastSelected))
                m_buttons[m_lastSelected]->setElementSelected(false);
            m_buttons[symbol]->setElementSelected(true);
            m_lastSelected = symbol;
            emit elementSelected(symbol, atomicNumber);
            emit selectionChanged({symbol});
        }
    } else { // Multi
        bool nowSelected = !m_buttons[symbol]->isElementSelected();
        m_buttons[symbol]->setElementSelected(nowSelected);
        emit selectionChanged(selectedSymbols());
    }
}
