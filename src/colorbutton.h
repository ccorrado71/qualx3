#ifndef COLORBUTTON_H
#define COLORBUTTON_H
#include <QWidget>
#include <QPushButton>
#include <QColor>
#include <QColorDialog>

class ColorButton: public QPushButton
{
    Q_OBJECT

public:
    explicit ColorButton(QWidget *parent = 0);
    ColorButton(QColor c, int k=0, QWidget *parent = 0);
    ~ColorButton() = default;
    void SetColor(QColor color_but);
    void SetSquare();
    void SetAlpha(bool val) {m_alpha = val;}
    QColor GetColor() {return m_color;}
private:
    int WI;
    int HE;
    bool m_square;
    bool   m_alpha;
    QColor m_color;
    void colorbutton_clicked(int k=-1);
signals:
    void colorChanged(QColor c, int k=-1);
};
#endif
