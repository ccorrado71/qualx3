#include "reportwidget.h"
#include "ui_reportwidget.h"
#include "dbresultswidget.h"   // cardColor
#include "searchoptionsdialog.h"

#include <QPainter>
#include <QBuffer>

ReportWidget::ReportWidget(QWidget *parent)
    : QWidget(parent), ui(new Ui::ReportWidget)
{
    ui->setupUi(this);
}

ReportWidget::~ReportWidget()
{
    delete ui;
}

// ── Public API ────────────────────────────────────────────────────────────────

void ReportWidget::updateReport(const ExperimentalPeaks &ep,
                                const QVector<CardType> &resultCards,
                                int maxCards)
{
    m_ep       = ep;
    m_cards    = resultCards;
    m_maxCards = maxCards;
    generateHtml();
}

void ReportWidget::updateQuantitative(const QVector<CardType> &phases,
                                      const QVector<double> &percentages)
{
    m_phases = phases;
    m_quant  = percentages;
    generateHtml();
}

void ReportWidget::clearQuantitative()
{
    m_phases.clear();
    m_quant.clear();
    generateHtml();
}

// ── Off-screen pie chart rendering ───────────────────────────────────────────

QPixmap ReportWidget::renderPieChart(const QVector<QPair<QColor, double>> &slices, int size)
{
    QPixmap pixmap(size, size);
    pixmap.fill(Qt::white);

    if (slices.isEmpty())
        return pixmap;

    QPainter p(&pixmap);
    p.setRenderHint(QPainter::Antialiasing);

    const int margin = 8;
    const QRectF rect(margin, margin, size - 2 * margin, size - 2 * margin);

    double startAngle = 90.0; // 12 o'clock
    for (const auto &slice : slices) {
        const double span = slice.second / 100.0 * 360.0;
        p.setBrush(QBrush(slice.first));
        p.setPen(QPen(Qt::white, 1));
        p.drawPie(rect, qRound(startAngle * 16), qRound(-span * 16));
        startAngle -= span;
    }
    return pixmap;
}

// ── HTML generation ───────────────────────────────────────────────────────────

void ReportWidget::generateHtml()
{
    QString html =
        "<html><body style='font-family:sans-serif;font-size:9pt'>"
        "<style>table{border-collapse:collapse} th,td{padding:3px} "
        "th{background:#e0e0e0}</style>";

    // ── Experimental peaks ──────────────────────────────────────────────────
    html += "<h3>Experimental Peaks</h3>";
    if (m_ep.tth.isEmpty()) {
        html += "<p><i>No experimental peaks loaded.</i></p>";
    } else {
        html += "<table border='1'>"
                "<tr><th>2&#x3B8; (&deg;)</th><th>Intensity</th></tr>";
        for (int i = 0; i < m_ep.tth.size(); ++i)
            html += QString("<tr><td align='right'>%1</td>"
                            "<td align='right'>%2</td></tr>")
                        .arg(m_ep.tth[i],       0, 'f', 4)
                        .arg(m_ep.intensity[i], 0, 'f', 1);
        html += "</table>";
    }

    // ── Search options ──────────────────────────────────────────────────────
    html += "<h3>Search Options</h3>"
            "<table border='0'>";
    auto optRow = [](const QString &label, const QString &val) {
        return QString("<tr><td>%1</td><td><b>%2</b></td></tr>").arg(label, val);
    };
    html += optRow("Min FOM",          QString::number(SearchOptionsDialog::savedMinFom(), 'f', 3));
    html += optRow("Weight 2&#x3B8;",  QString::number(SearchOptionsDialog::savedWeight2thetaD(), 'f', 2));
    html += optRow("Weight Intensity", QString::number(SearchOptionsDialog::savedWeightIntensity(), 'f', 2));
    html += optRow("Weight Phases",    QString::number(SearchOptionsDialog::savedWeightPhases(), 'f', 2));
    html += optRow("Delta 2&#x3B8;",   QString::number(SearchOptionsDialog::savedDelta2theta(), 'f', 4));
    html += optRow("Residual Search",  SearchOptionsDialog::savedResidualSearching() ? "Yes" : "No");
    html += optRow("Max Entries",      QString::number(SearchOptionsDialog::savedMaxEntries()));
    html += "</table>";

    // ── Result cards ────────────────────────────────────────────────────────
    const int shown = qMin(m_cards.size(), m_maxCards);
    html += QString("<h3>Search Results &ndash; %1 shown of %2</h3>")
                .arg(shown).arg(m_cards.size());
    if (m_cards.isEmpty()) {
        html += "<p><i>No results.</i></p>";
    } else {
        html += "<table border='1'>"
                "<tr><th>#</th><th>ID</th>"
                "<th>Chemical Name</th><th>Formula</th>"
                "<th>FOM</th><th>Scale</th></tr>";
        for (int i = 0; i < shown; ++i) {
            const CardType &card = m_cards[i];
            QString name = card.getChemicalName();
            if (!card.getMineralName().isEmpty())
                name += " [" + card.getMineralName() + "]";
            const QString bg = cardColor(card.getId()).name();
            html += QString("<tr>"
                            "<td align='right'>%1</td>"
                            "<td bgcolor='%2'>%3</td>"
                            "<td>%4</td><td>%5</td>"
                            "<td align='right'>%6</td>"
                            "<td align='right'>%7</td>"
                            "</tr>")
                        .arg(i + 1)
                        .arg(bg, card.getId())
                        .arg(name.toHtmlEscaped())
                        .arg(card.getChemicalFormula().toHtmlEscaped())
                        .arg(card.isFomCalculated() ? QString::number(card.getFom(),   'f', 4) : "-")
                        .arg(card.isFomCalculated() ? QString::number(card.getScale(), 'f', 4) : "-");
        }
        html += "</table>";
    }

    // ── Quantitative analysis ───────────────────────────────────────────────
    if (!m_phases.isEmpty()) {
        const bool valid = (m_quant.size() == m_phases.size());

        // Build slices for pie chart
        QVector<QPair<QColor, double>> slices;
        slices.reserve(m_phases.size());
        for (int i = 0; i < m_phases.size(); ++i) {
            const double pct = valid ? m_quant[i] : (100.0 / m_phases.size());
            slices.append({cardColor(m_phases[i].getId()), pct});
        }

        // Render pie chart and embed as base64 PNG
        const QPixmap pix = renderPieChart(slices, 280);
        QByteArray ba;
        QBuffer buf(&ba);
        buf.open(QIODevice::WriteOnly);
        pix.save(&buf, "PNG");
        const QString imgSrc = "data:image/png;base64," + QString::fromLatin1(ba.toBase64());

        html += "<h3>Quantitative Analysis</h3>";
        html += "<img src='" + imgSrc + "' width='280' height='280'/><br>";

        html += "<table border='1'>"
                "<tr><th></th><th>ID</th><th>Name</th><th>%</th></tr>";
        for (int i = 0; i < m_phases.size(); ++i) {
            const CardType &ph = m_phases[i];
            QString name = ph.getChemicalName();
            if (!ph.getMineralName().isEmpty())
                name += " [" + ph.getMineralName() + "]";
            const QString bg  = cardColor(ph.getId()).name();
            const QString pct = valid ? QString::number(m_quant[i], 'f', 2) + "%" : "-";
            html += QString("<tr>"
                            "<td bgcolor='%1'>&nbsp;&nbsp;&nbsp;</td>"
                            "<td>%2</td><td>%3</td>"
                            "<td align='right'>%4</td>"
                            "</tr>")
                        .arg(bg, ph.getId())
                        .arg(name.toHtmlEscaped(), pct);
        }
        html += "</table>";
    }

    html += "</body></html>";
    ui->textBrowser->setHtml(html);
}
