#include "dbresultswidget.h"
#include "paginationmodel.h"
#include "ui_dbresultswidget.h"
#include "textfilterproxymodel.h"
#include "scopedtimer.h"

#include "floatdelegate.h"

#include <QColor>
#include <QStandardItemModel>
#include <QHeaderView>
#include <QSignalBlocker>
#include <QItemSelectionModel>
#include <QPushButton>

#include <cmath>

// Maps a card ID string to a visually distinct, stable HSV colour.
// Uses the golden-ratio method so sequential IDs spread evenly around
// the hue wheel.
static QColor cardColor(const QString &id)
{
    bool ok;
    quint64 n = id.toULongLong(&ok);
    if (!ok)
        n = static_cast<quint64>(qHash(id));
    const double hue = std::fmod(n * 0.618033988749895, 1.0);
    return QColor::fromHsvF(hue, 0.65, 0.88);
}

DbResultsWidget::DbResultsWidget(QWidget* parent)
    : QWidget(parent), ui(new Ui::DbResultsWidget)
{
    ui->setupUi(this);

    sourceModel = new QStandardItemModel(0, 10, this);
    sourceModel->setHeaderData(0, Qt::Horizontal, "");
    sourceModel->setHeaderData(1, Qt::Horizontal, "QM");
    sourceModel->setHeaderData(2, Qt::Horizontal, "ID");
    sourceModel->setHeaderData(3, Qt::Horizontal, "Chemical Name");
    sourceModel->setHeaderData(4, Qt::Horizontal, "Chemical Formula");
    sourceModel->setHeaderData(5, Qt::Horizontal, "Peakpos.");
    sourceModel->setHeaderData(6, Qt::Horizontal, "Intensity");
    sourceModel->setHeaderData(7, Qt::Horizontal, "Scale");
    sourceModel->setHeaderData(8, Qt::Horizontal, "FOM");
    sourceModel->setHeaderData(9, Qt::Horizontal, "S-Quant.");

    ui->table->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);
    ui->table->horizontalHeader()->show();
    ui->table->verticalHeader()->setDefaultSectionSize(22);
    ui->table->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui->table->setSelectionMode(QAbstractItemView::ExtendedSelection);

    filterModel = new TextFilterProxyModel(this);
    filterModel->setSourceModel(sourceModel);
    filterModel->setFilterCaseSensitivity(Qt::CaseInsensitive);

    pageModel = new PaginationModel(500, this);
    pageModel->setSourceModel(filterModel);

    ui->table->setModel(pageModel);
    // NIENTE sortingEnabled qui!

    // FloatDelegate: right-align numerics, show '-' when value is missing
    for (int col : {5, 6, 7, 8})
        ui->table->setItemDelegateForColumn(col, new FloatDelegate(this, 5));
    ui->table->setItemDelegateForColumn(9, new FloatDelegate(this, 3));

    ui->ascButton->setEnabled(false);
    ui->desButton->setEnabled(false);

    ui->maxRowSpin->setMinimum(1);
    ui->maxRowSpin->setMaximum(10000);
    ui->maxRowSpin->setSingleStep(5);
    ui->maxRowSpin->setValue(pageModel->maxRows());

    ui->pageSpin->setMinimum(1);
    ui->pageSpin->setMaximum(pageModel->pageCount());
    ui->pageSpin->setValue(1);
    ui->pageSpin->setSuffix(QString("/%1").arg(pageModel->pageCount()));

    connect(ui->textFilterEdit, &QLineEdit::textEdited, filterModel, &TextFilterProxyModel::setFilterFixedString);
    connect(ui->maxRowSpin, QOverload<int>::of(&QSpinBox::valueChanged), pageModel, &PaginationModel::setMaxRows);

    connect(ui->firstBtn, &QToolButton::clicked, pageModel, &PaginationModel::firstPage);
    connect(ui->prevBtn, &QToolButton::clicked, pageModel, &PaginationModel::previousPage);
    connect(ui->nextBtn, &QToolButton::clicked, pageModel, &PaginationModel::nextPage);
    connect(ui->lastBtn, &QToolButton::clicked, pageModel, &PaginationModel::lastPage);
    connect(ui->pageSpin, QOverload<int>::of(&QSpinBox::valueChanged), [this](int p){ pageModel->setCurrentPage(p - 1); });

    connect(pageModel, &PaginationModel::currentPageChanged, this, &DbResultsWidget::currentPageChanged);
    connect(pageModel, &PaginationModel::pageCountChanged, this, &DbResultsWidget::pageCountChanged);
    connect(pageModel, &PaginationModel::canGoBackChanged, this, &DbResultsWidget::updateButtons);
    connect(pageModel, &PaginationModel::canGoForwardChanged, this, &DbResultsWidget::updateButtons);

    // Colonna selezionata
    connect(ui->table->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &DbResultsWidget::onTableSelectionChanged);

    // Pulsanti sort
    connect(ui->ascButton, &QPushButton::clicked, this, &DbResultsWidget::onAscClicked);
    connect(ui->desButton, &QPushButton::clicked, this, &DbResultsWidget::onDesClicked);

    updateButtons();
}

DbResultsWidget::~DbResultsWidget()
{
    delete ui;
}

void DbResultsWidget::setResults(const QVector<CardType>& results)
{
    ScopedTimer timer("DbResultsWidget::setResults");

    sourceModel->removeRows(0, sourceModel->rowCount());
    sourceModel->setRowCount(results.size());

    sourceModel->blockSignals(true);
    for (int i = 0; i < results.size(); ++i) {
        const CardType& card = results[i];

        QString chemName = card.getChemicalName();
        const QString mineral = card.getMineralName();
        if (!mineral.isEmpty())
            chemName += QStringLiteral(" [") + mineral + QLatin1Char(']');

        auto *colorItem = new QStandardItem();
        colorItem->setBackground(QBrush(cardColor(card.getId())));
        colorItem->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
        sourceModel->setItem(i, 0, colorItem);
        sourceModel->setItem(i, 1, new QStandardItem(card.getQuality()));
        sourceModel->setItem(i, 2, new QStandardItem(card.getId()));
        sourceModel->setItem(i, 3, new QStandardItem(chemName));
        sourceModel->setItem(i, 4, new QStandardItem(card.getChemicalFormula()));
        const bool hasFom = card.isFomCalculated();
        auto numItem = [](double v) {
            auto *it = new QStandardItem();
            it->setData(QVariant(v), Qt::DisplayRole);
            return it;
        };
        auto naItem = []() { return new QStandardItem(QStringLiteral("-")); };

        sourceModel->setItem(i, 5, hasFom ? numItem(card.getFomPeakPos())   : naItem());
        sourceModel->setItem(i, 6, hasFom ? numItem(card.getFomIntensity()) : naItem());
        sourceModel->setItem(i, 7, hasFom ? numItem(card.getScale())        : naItem());
        sourceModel->setItem(i, 8, hasFom ? numItem(card.getFom())          : naItem());

        const QString rir = card.getRIR().trimmed();
        bool rirOk = false;
        const double rirVal = rir.toDouble(&rirOk);
        sourceModel->setItem(i, 9, (rirOk && rirVal != 0.0) ? numItem(rirVal) : naItem());
    }
    sourceModel->blockSignals(false);

    ui->maxRowSpin->setMaximum(qMax(1, sourceModel->rowCount()));
    pageModel->setCurrentPage(0);
    ui->table->resizeColumnsToContents();
}

void DbResultsWidget::currentPageChanged(int page)
{
    QSignalBlocker blocker(ui->pageSpin);
    ui->pageSpin->setValue(page + 1);
}

void DbResultsWidget::pageCountChanged(int count)
{
    QSignalBlocker blocker(ui->pageSpin);
    ui->pageSpin->setMaximum(qMax(1, count));
    ui->pageSpin->setSuffix(QString("/%1").arg(count));
}

void DbResultsWidget::updateButtons()
{
    bool canBack = pageModel->canGoBack();
    ui->firstBtn->setEnabled(canBack);
    ui->prevBtn->setEnabled(canBack);
    bool canFwd = pageModel->canGoForward();
    ui->nextBtn->setEnabled(canFwd);
    ui->lastBtn->setEnabled(canFwd);

    // Attiva/disattiva i pulsanti sort in base alla selezione colonna
    bool enableSort = selectedColumn >= 0;
    ui->ascButton->setEnabled(enableSort);
    ui->desButton->setEnabled(enableSort);
}

void DbResultsWidget::onTableSelectionChanged()
{
    // Prendi la colonna della selezione attiva
    auto selection = ui->table->selectionModel()->selectedColumns();
    if (!selection.isEmpty())
        selectedColumn = selection.first().column();
    else
        selectedColumn = -1;
    updateButtons();
}

void DbResultsWidget::onAscClicked()
{
    if (selectedColumn >= 0)
        sourceModel->sort(selectedColumn, Qt::AscendingOrder);
}

void DbResultsWidget::onDesClicked()
{
    if (selectedColumn >= 0)
        sourceModel->sort(selectedColumn, Qt::DescendingOrder);
}
