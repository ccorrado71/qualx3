#ifndef COLORWIDGET_H
#define COLORWIDGET_H

#include <QWidget>

namespace Ui {
class ColorWidget;
}

class ColorWidget : public QWidget
{
    Q_OBJECT

public:
    explicit ColorWidget(QWidget *parent = nullptr);

    enum ColorType {TYPE_BLACK, TYPE_WHITE, TYPE_FLAT, TYPE_HGRAD, TYPE_VGRAD, TYPE_DGRAD, TYPE_DGRAD2, TYPE_RGRAD};

    void setColorWidget(const ColorType &type, const QColor &c, const QColor &c1, const QColor &c2);
    void getColorWidget(ColorType &type, QColor &c, QColor &c1, QColor &c2) const;
    bool isDifferent(const ColorType &type, const QColor &c, const QColor &c1, const QColor &c2) const;
    ~ColorWidget();

signals:
    void colorWidgetChanged();

private slots:
    void on_comboBox_currentIndexChanged(int index);

private:
    ColorType colorType;
    QColor color;
    QColor color1;
    QColor color2;
    Ui::ColorWidget *ui;
};

#endif // COLORWIDGET_H
