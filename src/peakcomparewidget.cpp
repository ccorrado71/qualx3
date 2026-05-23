#include "peakcomparewidget.h"
#include "ui_peakcomparewidget.h"
#include "peakassoc.h"
#include "floatdelegate.h"
#include "dbresultswidget.h"

#include <QStandardItemModel>
#include <QHeaderView>
#include <QSet>
#include <algorithm>

PeakCompareWidget::PeakCompareWidget(QWidget *parent)
    : QWidget(parent), ui(new Ui::PeakCompareWidget)
{
    ui->setupUi(this);

    m_model = new QStandardItemModel(this);
    ui->tableView->setModel(m_model);
    ui->tableView->verticalHeader()->setDefaultSectionSize(22);
    ui->tableView->verticalHeader()->hide();
    ui->tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);

    updateDelegates(3);
}

PeakCompareWidget::~PeakCompareWidget()
{
    delete ui;
}

// ── Public API ────────────────────────────────────────────────────────────────

void PeakCompareWidget::setExperimentalPeaks(const ExperimentalPeaks &ep)
{
    m_ep = ep;
    rebuild();
}

void PeakCompareWidget::setSelectedCard(const CardType &card, const QString &cardId, double delta)
{
    m_card    = card;
    m_cardId  = cardId;
    m_delta   = delta;
    m_hasCard = true;
    rebuild();
}

void PeakCompareWidget::clearCard()
{
    m_hasCard = false;
    rebuild();
}

void PeakCompareWidget::addAcceptedPhase(const CardType &card)
{
    m_acceptedPhases.append(card);
    rebuild();
}

void PeakCompareWidget::clearAcceptedPhases()
{
    m_acceptedPhases.clear();
    rebuild();
}

// ── Helpers ───────────────────────────────────────────────────────────────────

void PeakCompareWidget::updateDelegates(int totalColumns)
{
    // Pattern: col 0 = 2θ exp (4 dec), col 1 = I exp (2 dec)
    // For col >= 2: even offset = 2theta (4 dec), odd offset = intensity (2 dec)
    for (int col = 0; col < totalColumns; ++col) {
        const int dec = (col == 1 || (col >= 2 && (col - 2) % 2 == 1)) ? 2 : 4;
        ui->tableView->setItemDelegateForColumn(col, new FloatDelegate(this, dec));
    }
}

// ── Core rebuild ─────────────────────────────────────────────────────────────

void PeakCompareWidget::rebuild()
{
    auto numItem = [](double v) {
        auto *it = new QStandardItem();
        it->setData(QVariant(v), Qt::DisplayRole);
        return it;
    };
    auto naItem = []() { return new QStandardItem(QStringLiteral("-")); };
    auto coloredNumItem = [](double v, const QColor &bg) {
        auto *it = new QStandardItem();
        it->setData(QVariant(v), Qt::DisplayRole);
        it->setBackground(QBrush(bg));
        return it;
    };

    const int nsp  = m_ep.tth.size();
    const int nAcc = m_acceptedPhases.size();

    // ── Scale + associate each accepted phase ──────────────────────────────
    struct ScaledCard {
        QVector<double>          cardI;
        QVector<PeakAssociation> assoc;
    };
    QVector<ScaledCard> acc(nAcc);
    for (int j = 0; j < nAcc; ++j) {
        const CardType  &phase = m_acceptedPhases[j];
        const auto      &rawI  = phase.getIntensity();
        const double     sc    = (phase.getScale() > 0.0) ? phase.getScale() : 1.0;
        acc[j].cardI.resize(rawI.size());
        for (int k = 0; k < rawI.size(); ++k)
            acc[j].cardI[k] = rawI[k] * sc;
        acc[j].assoc.resize(nsp);
        if (nsp > 0 && !phase.getTth().isEmpty())
            acc[j].assoc = associatePeaks(m_ep.tth, m_ep.intensity,
                                          phase.getTth(), acc[j].cardI, m_delta);
    }

    // ── Scale + associate selected card ────────────────────────────────────
    QVector<double>          selCardI;
    QVector<PeakAssociation> selAssoc(nsp);
    QSet<int>                usedSelPeaks;
    if (m_hasCard) {
        const auto  &rawI = m_card.getIntensity();
        const double sc   = (m_card.getScale() > 0.0) ? m_card.getScale() : 1.0;
        selCardI.resize(rawI.size());
        for (int k = 0; k < rawI.size(); ++k)
            selCardI[k] = rawI[k] * sc;
        if (nsp > 0 && !m_card.getTth().isEmpty())
            selAssoc = associatePeaks(m_ep.tth, m_ep.intensity,
                                      m_card.getTth(), selCardI, m_delta);
        for (const auto &a : selAssoc)
            if (a.dbPeakIndex >= 0) usedSelPeaks.insert(a.dbPeakIndex);
    }

    // ── Row descriptors ────────────────────────────────────────────────────
    struct Match { double tth = 0, intensity = 0, quality = 0; bool has = false; };
    struct Row {
        double         sortKey;
        double         tthExp = 0, iExp = 0;
        bool           hasExp      = false;
        bool           isSelAssoc  = false;
        bool           compensated = false; // exp intensity fully covered by accepted phases
        QVector<Match> accepted;   // one entry per accepted phase
        Match          selected;
    };

    QVector<Row> rows;
    rows.reserve(nsp + (m_hasCard ? m_card.getTth().size() : 0));

    // Type A: one row per experimental peak
    for (int i = 0; i < nsp; ++i) {
        Row r;
        r.tthExp = m_ep.tth[i];
        r.iExp   = m_ep.intensity[i];
        r.hasExp = true;
        r.accepted.resize(nAcc);

        double residual = m_ep.intensity[i];
        for (int j = 0; j < nAcc; ++j) {
            if (!acc[j].assoc.isEmpty() && acc[j].assoc[i].dbPeakIndex >= 0) {
                const int k = acc[j].assoc[i].dbPeakIndex;
                r.accepted[j] = { m_acceptedPhases[j].getTth()[k], acc[j].cardI[k],
                                   qBound(0.0, acc[j].assoc[i].quality, 1.0), true };
                residual -= acc[j].cardI[k];
            }
        }
        r.compensated = (nAcc > 0 && residual <= 0.0);
        if (m_hasCard && selAssoc[i].dbPeakIndex >= 0) {
            const int k = selAssoc[i].dbPeakIndex;
            r.selected   = { m_card.getTth()[k], selCardI[k],
                              qBound(0.0, selAssoc[i].quality, 1.0), true };
            r.isSelAssoc = true;
        }
        r.sortKey = r.tthExp;
        rows.append(r);
    }

    // Type B: unassociated peaks of the selected card
    if (m_hasCard) {
        for (int k = 0; k < m_card.getTth().size(); ++k) {
            if (usedSelPeaks.contains(k)) continue;
            Row r;
            r.accepted.resize(nAcc);
            r.selected = { m_card.getTth()[k], selCardI[k], 0.0, true };
            r.sortKey  = r.selected.tth;
            rows.append(r);
        }
    }

    std::stable_sort(rows.begin(), rows.end(),
                     [](const Row &a, const Row &b){ return a.sortKey < b.sortKey; });

    // ── Columns ────────────────────────────────────────────────────────────
    const int totalCols = 2 + 2 * nAcc + (m_hasCard ? 2 : 0);
    m_model->clear();
    m_model->setColumnCount(totalCols);

    QStringList headers = { "2θ exp", "I exp" };
    for (int j = 0; j < nAcc; ++j) {
        const QString id = m_acceptedPhases[j].getId();
        headers << "2θ [" + id + ']' << "I [" + id + ']';
    }
    if (m_hasCard)
        headers << "2θ [" + m_cardId + ']' << "I [" + m_cardId + ']';

    m_model->setHorizontalHeaderLabels(headers);
    m_model->setRowCount(rows.size());
    updateDelegates(totalCols);

    // Precompute HSV for each phase color
    struct PhaseColor { float h = 0, s = 0, v = 1; };
    QVector<PhaseColor> accColors(nAcc);
    for (int j = 0; j < nAcc; ++j)
        cardColor(m_acceptedPhases[j].getId()).getHsvF(
            &accColors[j].h, &accColors[j].s, &accColors[j].v);

    PhaseColor selColor;
    if (m_hasCard)
        cardColor(m_cardId).getHsvF(&selColor.h, &selColor.s, &selColor.v);

    auto assocColor = [](const PhaseColor &pc, double q) -> QColor {
        return QColor::fromHsvF(pc.h, float(pc.s * q), float(1.0f - (1.0f - pc.v) * q));
    };

    // ── Populate model ─────────────────────────────────────────────────────
    for (int r = 0; r < rows.size(); ++r) {
        const Row &row = rows[r];

        if (row.hasExp && row.compensated) {
            static const QColor gray(180, 180, 180);
            m_model->setItem(r, 0, coloredNumItem(row.tthExp, gray));
            m_model->setItem(r, 1, coloredNumItem(row.iExp,   gray));
        } else {
            m_model->setItem(r, 0, row.hasExp ? numItem(row.tthExp) : naItem());
            m_model->setItem(r, 1, row.hasExp ? numItem(row.iExp)   : naItem());
        }

        for (int j = 0; j < nAcc; ++j) {
            const int   col = 2 + 2 * j;
            const Match &m  = row.accepted[j];
            if (m.has) {
                const QColor c = assocColor(accColors[j], m.quality);
                m_model->setItem(r, col,     coloredNumItem(m.tth,       c));
                m_model->setItem(r, col + 1, coloredNumItem(m.intensity, c));
            } else {
                m_model->setItem(r, col,     naItem());
                m_model->setItem(r, col + 1, naItem());
            }
        }

        if (m_hasCard) {
            const int col = 2 + 2 * nAcc;
            if (row.isSelAssoc) {
                const QColor c = assocColor(selColor, row.selected.quality);
                m_model->setItem(r, col,     coloredNumItem(row.selected.tth,       c));
                m_model->setItem(r, col + 1, coloredNumItem(row.selected.intensity, c));
            } else if (row.selected.has) {
                m_model->setItem(r, col,     numItem(row.selected.tth));
                m_model->setItem(r, col + 1, numItem(row.selected.intensity));
            } else {
                m_model->setItem(r, col,     naItem());
                m_model->setItem(r, col + 1, naItem());
            }
        }
    }

    ui->tableView->resizeColumnsToContents();
}
