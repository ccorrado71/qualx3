#ifndef LINESTYLEWIDGET_H
#define LINESTYLEWIDGET_H

#include <QWidget>

namespace Ui {
class LineStyleWidget;
}

class LineStyleWidget : public QWidget
{
    Q_OBJECT

public:
    explicit LineStyleWidget(QWidget *parent = nullptr);
    void setLineStyle(const Qt::PenStyle &s, const QColor &c, int w);
    void setLineStyle(const QPen &pen);
    void setBoxName(const QString &name);
    bool isDifferent(const QPen &pen) const;
    Qt::PenStyle getStyle() const;
    QColor getColor() const;
    int getWidth() const;
    ~LineStyleWidget();

signals:
    void lineColorChanged(QColor c);
    void lineWidthChanged(int i);
    void lineStyleChanged(Qt::PenStyle s);

private:
    Ui::LineStyleWidget *ui;
};

#endif // LINESTYLEWIDGET_H
