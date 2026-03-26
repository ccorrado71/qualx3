#include "stringlistdialog.h"
#include "ui_stringlistdialog.h"

StringListDialog::StringListDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::StringListDialog),
    selectedRow(-1)
{
    ui->setupUi(this);

    ui->tableWidget->verticalHeader()->hide();
    ui->tableWidget->verticalHeader()->setDefaultSectionSize(22);
#ifndef _WIN32
    ui->tableWidget->setAlternatingRowColors(true);
#endif
    ui->tableWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui->tableWidget->setShowGrid(false);
    ui->tableWidget->setEditTriggers(QAbstractItemView::NoEditTriggers);    
    ui->tableWidget->horizontalHeader()->stretchLastSection();
    ui->tableWidget->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);
    ui->tableWidget->setFocusPolicy(Qt::NoFocus);

    connect(ui->tableWidget, &QTableWidget::cellClicked, this, &StringListDialog::onCellClicked);
}

void StringListDialog::setLabel(const QString &label)
{
    ui->label->setText(label);
}

void StringListDialog::setTableSize(int nrow, int ncol)
{
    ui->tableWidget->setRowCount(nrow);
    ui->tableWidget->setColumnCount(ncol);
}

void StringListDialog::setColumTitle(const QStringList &titles)
{
    ui->tableWidget->setHorizontalHeaderLabels(titles);
}

void StringListDialog::setTableItem(int row, int col, const QString &sitem)
{
    ui->tableWidget->setItem(row, col, new QTableWidgetItem(sitem));
}

void StringListDialog::setSelection(int selection)
{
    selectedRow = selection;
    ui->tableWidget->setCurrentCell(selection, 0);
}

int StringListDialog::getSelection()
{
    return ui->tableWidget->currentRow();
}

StringListDialog::~StringListDialog()
{
    delete ui;
}

void StringListDialog::onCellClicked(int row)
{
    if (selectedRow != row) {
        selectedRow = row;
        emit newRowSelected(row);        
    }
}
