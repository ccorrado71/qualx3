#ifndef LINESTYLESCOMBOBOX_H
#define LINESTYLESCOMBOBOX_H

#include <QComboBox>

class LineStylesComboBox : public QComboBox
{
    Q_OBJECT
public:
    explicit LineStylesComboBox(QWidget *parent = nullptr);
    void setStyle(const Qt::PenStyle &style);
    Qt::PenStyle style() const;

signals:
    void lineStyleChanged(Qt::PenStyle style);

private:
    int WI;
    int HE;
    Qt::PenStyle mStyle;
    void populateComboBox();
};

#endif // LINESTYLESCOMBOBOX_H
