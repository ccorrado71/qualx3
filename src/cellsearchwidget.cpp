#include "cellsearchwidget.h"
#include "ui_cellsearchwidget.h"

#include <QLineEdit>
#include <QDoubleSpinBox>

CellSearchWidget::CellSearchWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::CellSearchWidget)
{
    ui->setupUi(this);
}

CellSearchWidget::~CellSearchWidget()
{
    delete ui;
}

QLineEdit *CellSearchWidget::lineEditA() const { return ui->lineEditA; }
QLineEdit *CellSearchWidget::lineEditB() const { return ui->lineEditB; }
QLineEdit *CellSearchWidget::lineEditC() const { return ui->lineEditC; }
QLineEdit *CellSearchWidget::lineEditAl() const { return ui->lineEditAl; }
QLineEdit *CellSearchWidget::lineEditBe() const { return ui->lineEditBe; }
QLineEdit *CellSearchWidget::lineEditGa() const { return ui->lineEditGa; }
QDoubleSpinBox *CellSearchWidget::doubleSpinLenTol() const { return ui->doubleSpinLenTol; }
QDoubleSpinBox *CellSearchWidget::doubleSpinAngTol() const { return ui->doubleSpinAngTol; }
