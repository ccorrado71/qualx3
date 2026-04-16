#pragma once
#include <QPushButton>
#include <QColor>
#include <QString>

class ElementButton : public QPushButton
{
    Q_OBJECT
public:
    ElementButton(int atomicNumber, const QString& symbol, const QString& name,
                  const QColor& baseColor, QWidget* parent = nullptr);

    int     atomicNumber()      const { return m_atomicNumber; }
    QString symbol()            const { return m_symbol; }
    bool    isElementSelected() const { return m_selected; }

    void setElementSelected(bool selected);

signals:
    void elementClicked(const QString& symbol, int atomicNumber);

protected:
    void paintEvent(QPaintEvent* event) override;

private:
    int     m_atomicNumber;
    QString m_symbol;
    QColor  m_baseColor;
    bool    m_selected = false;
};
