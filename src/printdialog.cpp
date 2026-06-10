#include "printdialog.h"
#include "ui_printdialog.h"

PrintDialog::PrintDialog(QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::PrintDialog)
{
    ui->setupUi(this);

    const QStringList areas = {
        tr("Report"),
        tr("Result List"),
        tr("Peak List"),
        tr("Pattern Graphics"),
        tr("Card"),
        tr("Quantitative Analysis"),
        tr("Compare"),
    };
    ui->listWidget->addItems(areas);
    ui->listWidget->setCurrentRow(0);

    connect(ui->listWidget, &QListWidget::itemDoubleClicked, this, &PrintDialog::accept);
}

PrintDialog::~PrintDialog()
{
    delete ui;
}

PrintDialog::PrintArea PrintDialog::selectedArea() const
{
    int row = ui->listWidget->currentRow();
    if (row < 0)
        return Report;
    return static_cast<PrintArea>(row);
}
