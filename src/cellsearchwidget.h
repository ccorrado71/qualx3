#ifndef CELLSEARCHWIDGET_H
#define CELLSEARCHWIDGET_H

#include <QWidget>

class QLineEdit;
class QDoubleSpinBox;

namespace Ui {
class CellSearchWidget;
}

class CellSearchWidget : public QWidget
{
    Q_OBJECT

public:
    explicit CellSearchWidget(QWidget *parent = nullptr);
    ~CellSearchWidget();

    QLineEdit *lineEditA() const;
    QLineEdit *lineEditB() const;
    QLineEdit *lineEditC() const;
    QLineEdit *lineEditAl() const;
    QLineEdit *lineEditBe() const;
    QLineEdit *lineEditGa() const;
    QDoubleSpinBox *doubleSpinLenTol() const;
    QDoubleSpinBox *doubleSpinAngTol() const;

private:
    Ui::CellSearchWidget *ui;
};

#endif // CELLSEARCHWIDGET_H
