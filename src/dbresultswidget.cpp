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
#include <QSet>

#include <algorithm>
#include <cmath>

// Maps a card ID string to a visually distinct, stable HSV colour.
// Uses the golden-ratio method so sequential IDs spread evenly around
// the hue wheel.
QColor cardColor(const QString &id)
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

    ui->table->horizontalHeader()->setSortIndicatorShown(true);
    ui->table->horizontalHeader()->setSortIndicator(-1, Qt::AscendingOrder);

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

    // Sort per click sull'intestazione della colonna
    connect(ui->table->horizontalHeader(), &QHeaderView::sectionClicked,
            this, &DbResultsWidget::onHeaderSectionClicked);

    connect(ui->acceptButton, &QToolButton::clicked, this, &DbResultsWidget::onAcceptClicked);

    // Card selection: emit id (for info panel) and full CardType (for peak compare)
    connect(ui->table->selectionModel(), &QItemSelectionModel::currentRowChanged,
            this, [this](const QModelIndex &current, const QModelIndex &) {
        if (!current.isValid()) return;
        const QString id = ui->table->model()->index(current.row(), 2).data().toString();
        if (id.isEmpty()) return;
        emit cardSelected(id);
        const QVariant v = ui->table->model()->index(current.row(), 0).data(Qt::UserRole);
        if (v.isValid())
            emit cardDataSelected(v.value<CardType>());
    });

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
    for (int i = 0; i < results.size(); ++i)
        populateRow(i, results[i]);
    sourceModel->blockSignals(false);

    pageModel->setCurrentPage(0);
    ui->table->resizeColumnsToContents();
    emit hasResultsChanged(hasResults());
}

void DbResultsWidget::populateRow(int row, const CardType &card)
{
    QString chemName = card.getChemicalName();
    const QString mineral = card.getMineralName();
    if (!mineral.isEmpty()) {
        if (chemName.isEmpty())
            chemName = mineral;
        else
            chemName += QStringLiteral(" [") + mineral + QLatin1Char(']');
    }

    auto *colorItem = new QStandardItem();
    colorItem->setBackground(QBrush(cardColor(card.getId())));
    colorItem->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
    colorItem->setData(QVariant::fromValue(card), Qt::UserRole);
    sourceModel->setItem(row, 0, colorItem);
    sourceModel->setItem(row, 1, new QStandardItem(card.getQuality()));
    sourceModel->setItem(row, 2, new QStandardItem(card.getId()));
    sourceModel->setItem(row, 3, new QStandardItem(chemName));
    sourceModel->setItem(row, 4, new QStandardItem(card.getChemicalFormula()));

    const bool hasFom = card.isFomCalculated();
    auto numItem = [](double v) {
        auto *it = new QStandardItem();
        it->setData(QVariant(v), Qt::DisplayRole);
        return it;
    };
    auto naItem = []() { return new QStandardItem(QStringLiteral("-")); };

    sourceModel->setItem(row, 5, hasFom ? numItem(card.getFomPeakPos())   : naItem());
    sourceModel->setItem(row, 6, hasFom ? numItem(card.getFomIntensity()) : naItem());
    sourceModel->setItem(row, 7, hasFom ? numItem(card.getScale())        : naItem());
    sourceModel->setItem(row, 8, hasFom ? numItem(card.getFom())          : naItem());

    const QString rir = card.getRIR().trimmed();
    bool rirOk = false;
    const double rirVal = rir.toDouble(&rirOk);
    sourceModel->setItem(row, 9, (rirOk && rirVal != 0.0) ? numItem(rirVal) : naItem());
}

void DbResultsWidget::mergeResults(const QVector<CardType> &newCards)
{
    // Build set of existing IDs (column 2)
    QSet<QString> existingIds;
    for (int r = 0; r < sourceModel->rowCount(); ++r) {
        if (auto *item = sourceModel->item(r, 2))
            existingIds.insert(item->text());
    }

    // Filter to cards not already present
    QVector<CardType> toAdd;
    toAdd.reserve(newCards.size());
    for (const CardType &card : newCards) {
        if (!existingIds.contains(card.getId()))
            toAdd.append(card);
    }
    if (toAdd.isEmpty()) return;

    const int startRow = sourceModel->rowCount();
    sourceModel->setRowCount(startRow + toAdd.size());
    sourceModel->blockSignals(true);
    for (int i = 0; i < toAdd.size(); ++i)
        populateRow(startRow + i, toAdd[i]);
    sourceModel->blockSignals(false);

    pageModel->setCurrentPage(0);
    ui->table->resizeColumnsToContents();
    emit hasResultsChanged(hasResults());
}

bool DbResultsWidget::hasResults() const
{
    return sourceModel->rowCount() > 0;
}

void DbResultsWidget::selectFirstCard()
{
    if (pageModel->rowCount() == 0) return;
    const QModelIndex first = pageModel->index(0, 0);
    ui->table->selectionModel()->setCurrentIndex(
        first, QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows);
}

QVector<CardType> DbResultsWidget::allCards() const
{
    QVector<CardType> result;
    result.reserve(sourceModel->rowCount());
    for (int r = 0; r < sourceModel->rowCount(); ++r) {
        auto *item = sourceModel->item(r, 0);
        if (item)
            result.append(item->data(Qt::UserRole).value<CardType>());
    }
    return result;
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
    ui->firstBtn->setEnabled(pageModel->canGoBack());
    ui->prevBtn->setEnabled(pageModel->canGoBack());
    ui->nextBtn->setEnabled(pageModel->canGoForward());
    ui->lastBtn->setEnabled(pageModel->canGoForward());
}

void DbResultsWidget::onAcceptClicked()
{
    const auto selected = ui->table->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    // Map selected page-model indices → source model rows + card data
    QVector<QPair<int, CardType>> rowCards;
    rowCards.reserve(selected.size());
    for (const QModelIndex &idx : selected) {
        const QModelIndex srcIdx = filterModel->mapToSource(pageModel->mapToSource(idx));
        auto *item = sourceModel->item(srcIdx.row(), 0);
        if (item)
            rowCards.append({srcIdx.row(), item->data(Qt::UserRole).value<CardType>()});
    }
    if (rowCards.isEmpty()) return;

    // Remove rows from highest to lowest to keep indices valid
    std::sort(rowCards.begin(), rowCards.end(),
              [](const auto &a, const auto &b){ return a.first > b.first; });
    for (const auto &rc : rowCards)
        sourceModel->removeRow(rc.first);

    // Emit phaseAccepted in ascending row order
    std::sort(rowCards.begin(), rowCards.end(),
              [](const auto &a, const auto &b){ return a.first < b.first; });
    for (const auto &rc : rowCards)
        emit phaseAccepted(rc.second);

    emit hasResultsChanged(hasResults());
}

void DbResultsWidget::onHeaderSectionClicked(int column)
{
    if (column == m_sortColumn) {
        m_sortOrder = (m_sortOrder == Qt::AscendingOrder)
                      ? Qt::DescendingOrder : Qt::AscendingOrder;
    } else {
        m_sortColumn = column;
        m_sortOrder  = Qt::AscendingOrder;
    }
    sourceModel->sort(m_sortColumn, m_sortOrder);
    ui->table->horizontalHeader()->setSortIndicator(m_sortColumn, m_sortOrder);
    pageModel->setCurrentPage(0);
}
