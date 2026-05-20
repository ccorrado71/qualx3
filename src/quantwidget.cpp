#include "quantwidget.h"
#include "ui_quantwidget.h"
#include "piechartwidget.h"
#include "floatdelegate.h"

#include <QStandardItemModel>
#include <QHeaderView>

#include <algorithm>
#include <cmath>

// Golden-ratio hue mapping (same algorithm as DbResultsWidget)
static QColor phaseColor(const QString &id)
{
    bool ok;
    quint64 n = id.toULongLong(&ok);
    if (!ok) n = static_cast<quint64>(qHash(id));
    const double hue = std::fmod(n * 0.618033988749895, 1.0);
    return QColor::fromHsvF(hue, 0.65, 0.88);
}

QuantWidget::QuantWidget(QWidget *parent)
    : QWidget(parent), ui(new Ui::QuantWidget)
{
    ui->setupUi(this);

    m_model = new QStandardItemModel(0, 4, this);
    m_model->setHeaderData(0, Qt::Horizontal, "");
    m_model->setHeaderData(1, Qt::Horizontal, "ID");
    m_model->setHeaderData(2, Qt::Horizontal, "Name");
    m_model->setHeaderData(3, Qt::Horizontal, "Quant.");

    ui->tableView->setModel(m_model);
    ui->tableView->verticalHeader()->setDefaultSectionSize(22);
    ui->tableView->verticalHeader()->hide();
    ui->tableView->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Fixed);
    ui->tableView->horizontalHeader()->resizeSection(0, 18);
    ui->tableView->horizontalHeader()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    ui->tableView->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    ui->tableView->horizontalHeader()->setSectionResizeMode(3, QHeaderView::ResizeToContents);
    ui->tableView->setSelectionBehavior(QAbstractItemView::SelectRows);

    ui->tableView->setItemDelegateForColumn(3, new FloatDelegate(this, 2));
}

QuantWidget::~QuantWidget()
{
    delete ui;
}

void QuantWidget::addPhase(const CardType &card)
{
    m_phases.append(card);
    updateQuant();
}

void QuantWidget::updateQuant()
{
    const int n = m_phases.size();

    // Compute rap[i] = max(intensity * scale) / rir  for each phase
    QVector<double> rap(n, 0.0);
    bool valid = true;
    for (int i = 0; i < n; ++i) {
        bool ok;
        const double rir = m_phases[i].getRIR().trimmed().toDouble(&ok);
        if (!ok || rir <= 0.0) { valid = false; break; }

        const QVector<double> &intens = m_phases[i].getIntensity();
        const double scale = m_phases[i].getScale();
        double maxI = 0.0;
        for (double v : intens) maxI = qMax(maxI, v * scale);
        rap[i] = maxI / rir;
    }

    double sum = 0.0;
    for (double r : rap) sum += r;

    QVector<double> quant(n, 0.0);
    if (valid && sum > 0.0)
        for (int i = 0; i < n; ++i)
            quant[i] = 100.0 * rap[i] / sum;

    // Rebuild table and pie slices
    m_model->setRowCount(n);
    QVector<QPair<QColor, double>> slices;
    slices.reserve(n);

    for (int i = 0; i < n; ++i) {
        const CardType &card = m_phases[i];
        const QColor color = phaseColor(card.getId());

        QString name = card.getChemicalName();
        if (!card.getMineralName().isEmpty())
            name += " [" + card.getMineralName() + "]";

        auto *colorItem = new QStandardItem();
        colorItem->setBackground(QBrush(color));
        colorItem->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);

        auto *quantItem = new QStandardItem();
        if (valid && sum > 0.0)
            quantItem->setData(QVariant(quant[i]), Qt::DisplayRole);
        else
            quantItem->setText("-");

        m_model->setItem(i, 0, colorItem);
        m_model->setItem(i, 1, new QStandardItem(card.getId()));
        m_model->setItem(i, 2, new QStandardItem(name));
        m_model->setItem(i, 3, quantItem);

        slices.append({color, (valid && sum > 0.0) ? quant[i] : (100.0 / n)});
    }

    ui->pieChart->setSlices(slices);
}
