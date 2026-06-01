#include "dbresultswidget.h"
#include "paginationmodel.h"
#include "ui_dbresultswidget.h"
#include "textfilterproxymodel.h"
#include "scopedtimer.h"

#include "floatdelegate.h"

#include <QColor>
#include <QColorDialog>
#include <QHash>
#include <QMenu>
#include <QStandardItemModel>
#include <QHeaderView>
#include <QSignalBlocker>
#include <QItemSelectionModel>
#include <QSet>

#include <algorithm>
#include <cmath>

static QHash<QString, QColor> s_colorOverrides;

QColor cardColor(const QString &id)
{
    if (s_colorOverrides.contains(id))
        return s_colorOverrides.value(id);
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

    connect(ui->table->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, [this]() {
        emit entrySelectionChanged(hasSelection());
        emit selectedCardsChanged(selectedCards());
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

void DbResultsWidget::addCard(const CardType &card)
{
    // If already present, just select it
    for (int r = 0; r < sourceModel->rowCount(); ++r) {
        if (auto *item = sourceModel->item(r, 2); item && item->text() == card.getId()) {
            selectCard(card.getId());
            return;
        }
    }

    const int row = sourceModel->rowCount();
    sourceModel->insertRow(row);
    populateRow(row, card);

    // Re-apply the current sort if any
    if (m_sortColumn >= 0)
        sourceModel->sort(m_sortColumn, m_sortOrder);

    emit hasResultsChanged(true);
    selectCard(card.getId());
}

QVector<CardType> DbResultsWidget::selectedCards() const
{
    const auto selected = ui->table->selectionModel()->selectedRows();
    QVector<CardType> result;
    result.reserve(selected.size());
    for (const QModelIndex &idx : selected) {
        const QModelIndex srcIdx = filterModel->mapToSource(pageModel->mapToSource(idx));
        if (auto *item = sourceModel->item(srcIdx.row(), 0))
            result.append(item->data(Qt::UserRole).value<CardType>());
    }
    return result;
}

void DbResultsWidget::selectCard(const QString &id)
{
    // Scan the source model to find the row with the given ID
    int sourceRow = -1;
    for (int r = 0; r < sourceModel->rowCount(); ++r) {
        if (auto *item = sourceModel->item(r, 2); item && item->text() == id) {
            sourceRow = r;
            break;
        }
    }
    if (sourceRow < 0) return;

    // Map through filter and page models
    const QModelIndex srcIdx    = sourceModel->index(sourceRow, 0);
    const QModelIndex filterIdx = filterModel->mapFromSource(srcIdx);
    if (!filterIdx.isValid()) return;

    // Ensure the page containing this row is visible
    const QModelIndex pageIdx = pageModel->mapFromSource(filterIdx);
    if (pageIdx.isValid()) {
        ui->table->selectionModel()->setCurrentIndex(
            pageIdx, QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows);
        ui->table->scrollTo(pageIdx);
    } else {
        // Row may be on a different page — navigate there first
        const int filterRow = filterIdx.row();
        const int targetPage = filterRow / pageModel->maxRows();
        pageModel->setCurrentPage(targetPage);
        const QModelIndex newPageIdx = pageModel->mapFromSource(filterIdx);
        if (newPageIdx.isValid()) {
            ui->table->selectionModel()->setCurrentIndex(
                newPageIdx, QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows);
            ui->table->scrollTo(newPageIdx);
        }
    }
}

bool DbResultsWidget::hasResults() const
{
    return sourceModel->rowCount() > 0;
}

bool DbResultsWidget::hasSelection() const
{
    return ui->table->selectionModel()->hasSelection();
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

void DbResultsWidget::setContextMenuActions(const QList<QAction *> &actions)
{
    ui->table->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(ui->table, &QWidget::customContextMenuRequested,
            this, [this, actions](const QPoint &pos) {
        QMenu menu(this);
        for (QAction *a : actions) {
            if (a)
                menu.addAction(a);
            else
                menu.addSeparator();
        }
        menu.exec(ui->table->viewport()->mapToGlobal(pos));
    });
}

void DbResultsWidget::setEntryToolBar(QToolBar *tb)
{
    tb->setMovable(false);
    tb->setFloatable(false);
    ui->topLayout->insertWidget(0, tb);
}

void DbResultsWidget::changeSelectedCardColor()
{
    const auto selected = ui->table->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    // Work on the first selected card only
    const QModelIndex srcIdx = filterModel->mapToSource(pageModel->mapToSource(selected.first()));
    auto *colorItem = sourceModel->item(srcIdx.row(), 0);
    if (!colorItem) return;

    const QString id = colorItem->data(Qt::UserRole).value<CardType>().getId();
    const QColor current = colorItem->background().color();

    const QColor chosen = QColorDialog::getColor(current, this, tr("Choose card color"));
    if (!chosen.isValid()) return;

    s_colorOverrides.insert(id, chosen);
    colorItem->setBackground(QBrush(chosen));

    emit cardColorChanged(id);
}

void DbResultsWidget::deleteSelectedCards()
{
    const auto selected = ui->table->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    // Map selected rows to source model row indices
    QSet<int> srcRowSet;
    for (const QModelIndex &idx : selected) {
        const QModelIndex srcIdx = filterModel->mapToSource(pageModel->mapToSource(idx));
        srcRowSet.insert(srcIdx.row());
    }

    const int minSrc = *std::min_element(srcRowSet.begin(), srcRowSet.end());
    const int maxSrc = *std::max_element(srcRowSet.begin(), srcRowSet.end());

    // Determine which card to select after deletion:
    // prefer the nearest row above the first deleted row; fall back to below the last.
    int targetSrcRow = -1;
    for (int r = minSrc - 1; r >= 0; --r) {
        if (!srcRowSet.contains(r)) { targetSrcRow = r; break; }
    }
    if (targetSrcRow < 0) {
        for (int r = maxSrc + 1; r < sourceModel->rowCount(); ++r) {
            if (!srcRowSet.contains(r)) { targetSrcRow = r; break; }
        }
    }

    // Save target ID so we can re-find it after row indices shift
    QString targetId;
    if (targetSrcRow >= 0) {
        if (auto *item = sourceModel->item(targetSrcRow, 2))
            targetId = item->text();
    }

    // Remove rows highest-first to keep lower indices valid
    QList<int> srcRows(srcRowSet.begin(), srcRowSet.end());
    std::sort(srcRows.begin(), srcRows.end(), std::greater<int>());
    for (int r : srcRows)
        sourceModel->removeRow(r);

    if (!targetId.isEmpty())
        selectCard(targetId);

    emit hasResultsChanged(hasResults());
}

void DbResultsWidget::acceptSelectedCards()
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

void DbResultsWidget::onAcceptClicked()
{
    acceptSelectedCards();
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
