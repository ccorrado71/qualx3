#include "managedatabasesdialog.h"
#include "ui_managedatabasesdialog.h"

#include <QCheckBox>
#include <QHeaderView>
#include <QTableWidgetItem>

ManageDatabasesDialog::ManageDatabasesDialog(QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::ManageDatabasesDialog)
    , mActiveIndex(-1)
{
    ui->setupUi(this);

    ui->tableWidget->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->tableWidget->horizontalHeader()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    ui->tableWidget->horizontalHeader()->setSectionResizeMode(2, QHeaderView::ResizeToContents);
    ui->tableWidget->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Stretch);
    ui->tableWidget->verticalHeader()->setVisible(false);

    connect(ui->quitButton,   &QPushButton::clicked, this, &QDialog::accept);
    connect(ui->renameButton, &QPushButton::clicked, this, &ManageDatabasesDialog::onRenameClicked);
    connect(ui->addButton,    &QPushButton::clicked, this, &ManageDatabasesDialog::onAddClicked);
    connect(ui->createButton, &QPushButton::clicked, this, &ManageDatabasesDialog::onCreateClicked);
    connect(ui->deleteButton, &QPushButton::clicked, this, &ManageDatabasesDialog::onDeleteClicked);
}

ManageDatabasesDialog::~ManageDatabasesDialog()
{
    delete ui;
}

void ManageDatabasesDialog::setDatabases(const QList<DatabaseEntry> &databases)
{
    mDatabases = databases;
    mActiveIndex = -1;
    for (int i = 0; i < mDatabases.size(); ++i) {
        if (mDatabases[i].inUse) {
            mActiveIndex = i;
            break;
        }
    }
    rebuildTable();
}

QList<DatabaseEntry> ManageDatabasesDialog::databases() const
{
    return mDatabases;
}

int ManageDatabasesDialog::activeDatabase() const
{
    return mActiveIndex;
}

void ManageDatabasesDialog::rebuildTable()
{
    ui->tableWidget->blockSignals(true);
    ui->tableWidget->setRowCount(mDatabases.size());

    for (int row = 0; row < mDatabases.size(); ++row) {
        const DatabaseEntry &db = mDatabases[row];

        // Column 0: Use (checkbox, centered)
        QWidget *checkWidget = new QWidget();
        QCheckBox *checkBox = new QCheckBox();
        checkBox->setChecked(row == mActiveIndex);
        QHBoxLayout *layout = new QHBoxLayout(checkWidget);
        layout->addWidget(checkBox);
        layout->setAlignment(Qt::AlignCenter);
        layout->setContentsMargins(0, 0, 0, 0);
        checkWidget->setLayout(layout);
        ui->tableWidget->setCellWidget(row, 0, checkWidget);

        connect(checkBox, &QCheckBox::toggled, this, [this, row](bool checked) {
            onCheckboxChanged(row, checked);
        });

        // Column 1: Database name
        QTableWidgetItem *nameItem = new QTableWidgetItem(db.name);
        nameItem->setFlags(nameItem->flags() & ~Qt::ItemIsEditable);
        ui->tableWidget->setItem(row, 1, nameItem);

        // Column 2: Entries
        QTableWidgetItem *entriesItem = new QTableWidgetItem(QString::number(db.entries));
        entriesItem->setFlags(entriesItem->flags() & ~Qt::ItemIsEditable);
        entriesItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        ui->tableWidget->setItem(row, 2, entriesItem);

        // Column 3: Path
        QTableWidgetItem *pathItem = new QTableWidgetItem(db.path);
        pathItem->setFlags(pathItem->flags() & ~Qt::ItemIsEditable);
        ui->tableWidget->setItem(row, 3, pathItem);
    }

    ui->tableWidget->blockSignals(false);
}

void ManageDatabasesDialog::onCheckboxChanged(int row, bool checked)
{
    if (!checked) {
        // Prevent unchecking without selecting another
        if (mActiveIndex == row) {
            // Re-check it silently
            QWidget *w = ui->tableWidget->cellWidget(row, 0);
            if (w) {
                QCheckBox *cb = w->findChild<QCheckBox *>();
                if (cb) {
                    cb->blockSignals(true);
                    cb->setChecked(true);
                    cb->blockSignals(false);
                }
            }
        }
        return;
    }

    // Uncheck the previously active row
    if (mActiveIndex >= 0 && mActiveIndex != row) {
        QWidget *w = ui->tableWidget->cellWidget(mActiveIndex, 0);
        if (w) {
            QCheckBox *cb = w->findChild<QCheckBox *>();
            if (cb) {
                cb->blockSignals(true);
                cb->setChecked(false);
                cb->blockSignals(false);
            }
        }
        mDatabases[mActiveIndex].inUse = false;
    }

    mActiveIndex = row;
    mDatabases[row].inUse = true;
}

int ManageDatabasesDialog::currentSelectedRow() const
{
    return ui->tableWidget->currentRow();
}

void ManageDatabasesDialog::onRenameClicked()
{
    emit renameRequested(currentSelectedRow());
}

void ManageDatabasesDialog::onAddClicked()
{
    emit addRequested();
}

void ManageDatabasesDialog::onCreateClicked()
{
    emit createRequested();
}

void ManageDatabasesDialog::onDeleteClicked()
{
    emit deleteRequested(currentSelectedRow());
}
