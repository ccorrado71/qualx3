#pragma once
#include <QWidget>
#include <QMap>
#include <QList>
#include <QStringList>

class ElementButton;
class QGridLayout;
class QPushButton;

class PeriodicTableWidget : public QWidget
{
    Q_OBJECT
public:
    enum class SelectionMode { Single, Multi };

    explicit PeriodicTableWidget(SelectionMode mode, QWidget* parent = nullptr);

    QStringList selectedSymbols() const;
    void        clearSelection();

    // Solo in modalità Multi: mostra/nasconde i tasti di selezione per periodo/gruppo/serie
    void setSelectionHelpersVisible(bool visible);
    bool selectionHelpersVisible() const { return m_helpersVisible; }

signals:
    void elementSelected(const QString& symbol, int atomicNumber);
    void selectionChanged(const QStringList& symbols);

private slots:
    void onElementClicked(const QString& symbol, int atomicNumber);

private:
    void         setupLayout();
    void         setupHelpers(QGridLayout* grid);
    QPushButton* createSelectorButton(const QString& label, const QColor& color);
    void         toggleSelection(const QStringList& symbols);

    SelectionMode                 m_selectionMode;
    QMap<QString, ElementButton*> m_buttons;
    QString                       m_lastSelected;

    QList<QWidget*>        m_helperWidgets;
    QMap<int, QStringList> m_periodElements; // periodo (1-7) → simboli
    QMap<int, QStringList> m_groupElements;  // gruppo  (1-18) → simboli
    QStringList            m_lanthanideSymbols;
    QStringList            m_actinideSymbols;
    bool                   m_helpersVisible = false;
};
