#include "dbresultswidget.h"
#include "paginationmodel.h"
#include "ui_dbresultswidget.h"
#include "textfilterproxymodel.h"

#include <QStandardItemModel>
#include <QHeaderView>
#include <QSignalBlocker>
#include <QItemSelectionModel>
#include <QPushButton>

DbResultsWidget::DbResultsWidget(QWidget* parent)
    : QWidget(parent), ui(new Ui::DbResultsWidget)
{
    ui->setupUi(this);

    // 7 columns for all CardType members
    sourceModel = new QStandardItemModel(0, 7, this);
    sourceModel->setHeaderData(0, Qt::Horizontal, "ID");
    sourceModel->setHeaderData(1, Qt::Horizontal, "Chemical Name");
    sourceModel->setHeaderData(2, Qt::Horizontal, "Chemical Formula");
    sourceModel->setHeaderData(3, Qt::Horizontal, "Mineral Name");
    sourceModel->setHeaderData(4, Qt::Horizontal, "Quality");
    sourceModel->setHeaderData(5, Qt::Horizontal, "RIR");
    sourceModel->setHeaderData(6, Qt::Horizontal, "FOMD");

    ui->table->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);
    ui->table->horizontalHeader()->show();

    filterModel = new TextFilterProxyModel(this);
    filterModel->setSourceModel(sourceModel);
    filterModel->setFilterCaseSensitivity(Qt::CaseInsensitive);

    pageModel = new PaginationModel();
    pageModel->setSourceModel(filterModel);

    ui->table->setModel(pageModel);
    // NIENTE sortingEnabled qui!

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
    sourceModel->removeRows(0, sourceModel->rowCount());
    sourceModel->setRowCount(results.size());

    for (int i = 0; i < results.size(); ++i) {
        const CardType& card = results[i];
        sourceModel->setItem(i, 0, new QStandardItem(card.getId()));
        sourceModel->setItem(i, 1, new QStandardItem(card.getChemicalName()));
        sourceModel->setItem(i, 2, new QStandardItem(card.getChemicalFormula()));
        sourceModel->setItem(i, 3, new QStandardItem(card.getMineralName()));
        sourceModel->setItem(i, 4, new QStandardItem(card.getQuality()));
        sourceModel->setItem(i, 5, new QStandardItem(card.getRIR()));
        sourceModel->setItem(i, 6, new QStandardItem(QString::number(card.getFomd(), 'f', 2)));
    }

    ui->maxRowSpin->setMaximum(qMax(1, sourceModel->rowCount()));
    pageModel->setCurrentPage(0);
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
