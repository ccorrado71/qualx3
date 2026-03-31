#include "qualxdbpopulator.h"

#include <QSqlDatabase>
#include <cmath>
#include <QSqlError>
#include <QDebug>
#include <QSet>
#include <QDate>
#include <algorithm>

// -----------------------------------------------------------------------
// Construction
// -----------------------------------------------------------------------

QualxDbPopulator::QualxDbPopulator(QualxDbCreator *db, QObject *parent)
    : QObject(parent)
    , m_db(db)
{
    m_chemQuery = QSqlQuery(m_db->mainDb());
    m_chemQuery.prepare(
        "INSERT OR IGNORE INTO chemical (id, chemical_element) VALUES (?, ?)");

    m_idQuery = QSqlQuery(m_db->mainDb());
    m_idQuery.prepare(
        "INSERT INTO id "
        "(id, name, mineralname, chemical_formula, spacegroup, quality, rir, "
        " nrec, dvalue, intensita, nd, bestdval, n) "
        "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)");

    m_subQuery = QSqlQuery(m_db->mainDb());
    m_subQuery.prepare(
        "INSERT OR IGNORE INTO subfiles (id, subfile) VALUES (?, ?)");

    m_infoQuery = QSqlQuery(m_db->mainDb());
    m_infoQuery.prepare(
        "INSERT OR IGNORE INTO info "
        "(id, authors, journal, journal_year, journal_volume, journal_issue, "
        " page_start, page_end, color, crystal_density, z, spacegroup, type, "
        " volume, density, \"mu(CuKa)\", a, b, c, alpha, beta, gamma, rir, "
        " h, k, l, mul) "
        "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)");

    m_topQuery = QSqlQuery(m_db->searchDb());
    m_topQuery.prepare(
        "INSERT OR IGNORE INTO top (id, n, dval) VALUES (?, ?, ?)");
}

// -----------------------------------------------------------------------
// Slot: called for every completed card
// -----------------------------------------------------------------------

void QualxDbPopulator::onCardReady(const Pdf2Card &card)
{
    if (!m_inTransaction)
        beginTransaction();

    const QVector<Pdf2Peak> top6 = selectTopPeaks(card.peaks, 6);
    insertChemical(card);
    insertId(card, top6);
    insertSubfiles(card);
    insertInfo(card);
    insertTop(card, top6);

    accumulateInfoStat(card);

    if (!card.spaceGroup.trimmed().isEmpty() && !card.crystalSystem.isNull()) {
        QString sysName;
        if (card.crystalSystem == QLatin1Char('R')) {
            // For trigonal space groups: distinguish axis setting from cell params.
            // Rhombohedral setting (R axes) stores only a and α.
            // Hexagonal setting (H axes) stores only a and c (α absent → -1.0).
            sysName = (card.alpha > 0.0)
                      ? QStringLiteral("Trigonal (rhombohedral axes)")
                      : QStringLiteral("Trigonal (hexagonal axes)");
        } else {
            sysName = card.crystalSystemName();
        }
        if (!sysName.isEmpty())
            m_spgrStat[{card.spaceGroup, sysName}].append(card.id);
    }

    ++m_cardCount;
    if ((m_cardCount % kTransactionSize) == 0) {
        commitTransaction();
        beginTransaction();
    }
}

// -----------------------------------------------------------------------
// Slot: called when Pdf2Reader has finished the file
// -----------------------------------------------------------------------

void QualxDbPopulator::onFinished(int totalCards)
{
    if (m_inTransaction)
        commitTransaction();

    // Populate infodb: one row with creation date, card count, type, source
    QSqlQuery q(m_db->mainDb());
    q.prepare("INSERT INTO infodb (id, date, ncard, type, source) VALUES (?, ?, ?, ?, ?)");
    q.addBindValue(0);
    q.addBindValue(QDate::currentDate().toString(Qt::ISODate));
    q.addBindValue(totalCards);
    q.addBindValue(QStringLiteral("PDF2"));
    q.addBindValue(QStringLiteral("you"));
    if (!q.exec())
        qWarning() << "infodb INSERT error:" << q.lastError().text();

    populateSpgrStat();
    populateInfoStat();

    qDebug() << "QualxDbPopulator: done." << totalCards << "cards processed.";
}

// -----------------------------------------------------------------------
// insertChemical
// -----------------------------------------------------------------------

void QualxDbPopulator::insertChemical(const Pdf2Card &card)
{
    if (card.chemicalFormula.isEmpty())
        return;

    const int numericId = card.id.toInt();
    const QStringList elements = extractElements(card.chemicalFormula);

    for (const QString &el : elements) {
        m_chemQuery.addBindValue(numericId);
        m_chemQuery.addBindValue(el);
        if (!m_chemQuery.exec())
            qWarning() << "chemical INSERT error:" << m_chemQuery.lastError().text()
                       << "id=" << card.id << "element=" << el;
    }
}

// -----------------------------------------------------------------------
// insertId
// -----------------------------------------------------------------------

// Helper: return a null QVariant if the string is empty, otherwise the string itself
static QVariant strOrNull(const QString &s)
{
    return s.isEmpty() ? QVariant() : QVariant(s);
}

void QualxDbPopulator::insertId(const Pdf2Card &card, const QVector<Pdf2Peak> &top6)
{
    ++m_rowCounter;

    int nd = 0;
    const QString dval  = buildDvalString(card.peaks, nd);
    const QString ival  = buildIvalString(card.peaks);
    const QString bestd = buildBestDval(top6);
    const QString qual  = card.quality.isNull() ? QString() : QString(card.quality);

    m_idQuery.addBindValue(card.id.toInt());
    m_idQuery.addBindValue(strOrNull(card.name));
    m_idQuery.addBindValue(strOrNull(card.mineralName));
    m_idQuery.addBindValue(strOrNull(card.chemicalFormula));
    m_idQuery.addBindValue(strOrNull(card.spaceGroup));
    m_idQuery.addBindValue(strOrNull(qual));
    m_idQuery.addBindValue(card.rir > 0.0f ? QVariant(double(card.rir)) : QVariant());
    m_idQuery.addBindValue(card.nrec);
    m_idQuery.addBindValue(strOrNull(dval));
    m_idQuery.addBindValue(strOrNull(ival));
    m_idQuery.addBindValue(nd);
    m_idQuery.addBindValue(strOrNull(bestd));
    m_idQuery.addBindValue(m_rowCounter);

    if (!m_idQuery.exec())
        qWarning() << "id INSERT error:" << m_idQuery.lastError().text() << "id=" << card.id;
}

// -----------------------------------------------------------------------
// insertInfo
// -----------------------------------------------------------------------

void QualxDbPopulator::insertInfo(const Pdf2Card &card)
{
    // All columns in the info table are NOT NULL.
    // For VARCHAR columns: use empty string when data is unavailable.
    // For REAL columns (a,b,c,alpha,beta,gamma): use 0.0.
    // For BLOB columns (h,k,l,mul): not available in PDF-2 powder data;
    //   Qt's SQLite driver treats QByteArray() as NULL, so use a 1-byte placeholder.
    static const QByteArray emptyBlob(1, '\0');

    // Helper: convert float to string; returns "" (not null) when unavailable.
    // Important: QString() (null) would be bound as SQL NULL by Qt's SQLite driver,
    // violating the NOT NULL constraints on all info columns.
    auto floatStr = [](float v) -> QString {
        return v > 0.0f ? QString::number(double(v), 'f', 4) : QStringLiteral("");
    };
    // Helper: return empty string instead of null for any QString field.
    auto str = [](const QString &s) -> QString {
        return s.isNull() ? QStringLiteral("") : s;
    };

    const QString qual = card.quality.isNull() ? QStringLiteral("") : QString(card.quality);

    m_infoQuery.addBindValue(card.id);
    m_infoQuery.addBindValue(str(card.authors));                      // VARCHAR NOT NULL
    m_infoQuery.addBindValue(str(card.journal));                      // VARCHAR NOT NULL
    m_infoQuery.addBindValue(str(card.journalYear));                  // VARCHAR NOT NULL
    m_infoQuery.addBindValue(str(card.journalVolume));                // VARCHAR NOT NULL
    m_infoQuery.addBindValue(QStringLiteral(""));                     // journal_issue – not in PDF-2
    m_infoQuery.addBindValue(str(card.pageStart));                    // VARCHAR NOT NULL
    m_infoQuery.addBindValue(QStringLiteral(""));                     // page_end – not in PDF-2
    m_infoQuery.addBindValue(str(card.color));                        // VARCHAR NOT NULL
    m_infoQuery.addBindValue(floatStr(card.calcDensity));             // crystal_density VARCHAR NOT NULL
    m_infoQuery.addBindValue(card.z > 0 ? QString::number(card.z) : QStringLiteral("")); // z VARCHAR NOT NULL
    m_infoQuery.addBindValue(str(card.spaceGroup));                   // VARCHAR NOT NULL
    m_infoQuery.addBindValue(qual);                                   // type VARCHAR NOT NULL
    m_infoQuery.addBindValue(floatStr(card.volume));                  // volume VARCHAR NOT NULL
    m_infoQuery.addBindValue(floatStr(card.density));                 // density VARCHAR NOT NULL
    m_infoQuery.addBindValue(card.muCuKa >= 0.0f                     // mu(CuKa) VARCHAR NOT NULL
                             ? QString::number(double(card.muCuKa), 'f', 4) : QStringLiteral(""));
    m_infoQuery.addBindValue(card.a > 0.0 ? card.a : 0.0);  // REAL NOT NULL
    m_infoQuery.addBindValue(card.b > 0.0 ? card.b : 0.0);
    m_infoQuery.addBindValue(card.c > 0.0 ? card.c : 0.0);
    m_infoQuery.addBindValue(card.alpha > 0.0 ? card.alpha : 0.0);
    m_infoQuery.addBindValue(card.beta  > 0.0 ? card.beta  : 0.0);
    m_infoQuery.addBindValue(card.gamma > 0.0 ? card.gamma : 0.0);
    m_infoQuery.addBindValue(card.rir > 0.0f                         // rir VARCHAR NOT NULL
                             ? QString::number(double(card.rir), 'f', 4) : QStringLiteral(""));
    m_infoQuery.addBindValue(emptyBlob);  // h   BLOB NOT NULL
    m_infoQuery.addBindValue(emptyBlob);  // k   BLOB NOT NULL
    m_infoQuery.addBindValue(emptyBlob);  // l   BLOB NOT NULL
    m_infoQuery.addBindValue(emptyBlob);  // mul BLOB NOT NULL

    if (!m_infoQuery.exec())
        qWarning() << "info INSERT error:" << m_infoQuery.lastError().text() << "id=" << card.id;
}

// -----------------------------------------------------------------------
// insertSubfiles
// -----------------------------------------------------------------------

void QualxDbPopulator::insertSubfiles(const Pdf2Card &card)
{
    if (card.subfiles.isEmpty())
        return;

    const int numericId = card.id.toInt();
    for (const QString &code : card.subfiles) {
        m_subQuery.addBindValue(numericId);
        m_subQuery.addBindValue(code);
        if (!m_subQuery.exec())
            qWarning() << "subfiles INSERT error:" << m_subQuery.lastError().text()
                       << "id=" << card.id << "code=" << code;
    }
}

// -----------------------------------------------------------------------
// buildDvalString / buildIvalString
//
// The peaks in Pdf2Card are already sorted by d descending (by Pdf2Reader).
// We deduplicate consecutive peaks with identical (d, intensity) pairs,
// mirroring the 'ordina' function in the original C code.
// -----------------------------------------------------------------------

QString QualxDbPopulator::buildDvalString(const QVector<Pdf2Peak> &peaks, int &ndOut)
{
    QString result;
    ndOut = 0;
    for (int i = 0; i < peaks.size(); ++i) {
        if (i > 0 && peaks[i].d == peaks[i-1].d && peaks[i].intensity == peaks[i-1].intensity)
            continue;  // skip duplicate
        result += QString::asprintf("%f,", double(peaks[i].d));
        ++ndOut;
    }
    return result;
}

QString QualxDbPopulator::buildIvalString(const QVector<Pdf2Peak> &peaks)
{
    QString result;
    for (int i = 0; i < peaks.size(); ++i) {
        if (i > 0 && peaks[i].d == peaks[i-1].d && peaks[i].intensity == peaks[i-1].intensity)
            continue;
        result += QString::asprintf("%f,", double(peaks[i].intensity));
    }
    return result;
}

// -----------------------------------------------------------------------
// selectTopPeaks  (shared core — sort done exactly once per card)
//
//  1. Deduplicate consecutive peaks with identical (d, intensity).
//  2. Sort by intensity descending (stable).
//  3. Keep the top min(n, size) peaks.
//  4. Sort those by d descending.
//
// Both buildBestDval (n=6) and buildTopDval (n=kNbestd) call this.
// -----------------------------------------------------------------------

QVector<Pdf2Peak> QualxDbPopulator::selectTopPeaks(const QVector<Pdf2Peak> &peaks, int n)
{
    QVector<Pdf2Peak> unique;
    unique.reserve(peaks.size());
    for (int i = 0; i < peaks.size(); ++i) {
        if (i > 0 && peaks[i].d == peaks[i-1].d && peaks[i].intensity == peaks[i-1].intensity)
            continue;
        unique.append(peaks[i]);
    }

    // Sort by intensity descending — the single shared sort.
    // The d-sort is NOT applied here: each formatter (buildBestDval / buildTopDval)
    // applies it on its own subset after selecting the desired number of peaks.
    std::stable_sort(unique.begin(), unique.end(),
                     [](const Pdf2Peak &a, const Pdf2Peak &b) {
                         return a.intensity > b.intensity;
                     });

    unique.resize(qMin(unique.size(), n));
    return unique;
}

// -----------------------------------------------------------------------
// buildBestDval  — formats pre-selected top-6 peaks as "%f," (trailing comma)
// -----------------------------------------------------------------------

QString QualxDbPopulator::buildBestDval(const QVector<Pdf2Peak> &topPeaks)
{
    // topPeaks is sorted by intensity — sort a local copy by d descending before formatting.
    QVector<Pdf2Peak> byD = topPeaks;
    std::sort(byD.begin(), byD.end(),
              [](const Pdf2Peak &a, const Pdf2Peak &b) { return a.d > b.d; });
    QString result;
    for (const Pdf2Peak &p : byD)
        result += QString::asprintf("%f,", double(p.d));
    return result;
}

// -----------------------------------------------------------------------
// buildTopDval  — formats the first min(kNbestd, size) of the pre-selected
//                peaks as "%f" joined by commas (no trailing comma).
// -----------------------------------------------------------------------

QString QualxDbPopulator::buildTopDval(const QVector<Pdf2Peak> &topPeaks, int &nOut)
{
    // topPeaks is sorted by intensity descending.
    // Store the d-values in that same intensity order — no d-sort.
    nOut = qMin(topPeaks.size(), kNbestd);
    QStringList parts;
    parts.reserve(nOut);
    for (int i = 0; i < nOut; ++i)
        parts.append(QString::asprintf("%f", double(topPeaks[i].d)));
    return parts.join(QLatin1Char(','));
}

// -----------------------------------------------------------------------
// insertTop
// -----------------------------------------------------------------------

void QualxDbPopulator::insertTop(const Pdf2Card &card, const QVector<Pdf2Peak> &top6)
{
    int n = 0;
    const QString dval = buildTopDval(top6, n);

    m_topQuery.addBindValue(card.id.toInt());
    m_topQuery.addBindValue(n);
    m_topQuery.addBindValue(dval);
    if (!m_topQuery.exec())
        qWarning() << "top INSERT error:" << m_topQuery.lastError().text() << "id=" << card.id;
}

// -----------------------------------------------------------------------
// extractElements
//
// Mirrors the original C logic (splitinfochemical + eliminaduplicati):
//  1. Replace digits, brackets, and punctuation with spaces.
//  2. Remove standalone variable characters: 'x', 'z', and 'n' when the
//     previous character was not a letter (i.e. not part of a symbol like Sn, Mn).
//  3. Split on whitespace and deduplicate.
// -----------------------------------------------------------------------

QStringList QualxDbPopulator::extractElements(const QString &formula)
{
    QString cleaned;
    cleaned.reserve(formula.size());

    for (int i = 0; i < formula.size(); ++i) {
        const QChar c = formula.at(i);
        const char  a = c.toLatin1();

        if (c.isDigit()
            || a == '(' || a == ')' || a == '[' || a == ']'
            || a == ',' || a == '+' || a == '-' || a == '.'
            || a == '!' || a == '/')
        {
            cleaned += QLatin1Char(' ');
            continue;
        }

        if (a == 'x' || a == 'z') {
            cleaned += QLatin1Char(' ');
            continue;
        }
        if (a == 'n') {
            const bool partOfSymbol = (i > 0 && formula.at(i - 1).isLetter());
            if (!partOfSymbol) {
                cleaned += QLatin1Char(' ');
                continue;
            }
        }

        cleaned += c;
    }

    const QStringList tokens = cleaned.split(QLatin1Char(' '), Qt::SkipEmptyParts);

    QSet<QString>  seen;
    QStringList    result;
    for (const QString &tok : tokens) {
        if (!seen.contains(tok)) {
            seen.insert(tok);
            result.append(tok);
        }
    }
    return result;
}

// -----------------------------------------------------------------------
// Transaction helpers
// -----------------------------------------------------------------------

void QualxDbPopulator::beginTransaction()
{
    m_db->mainDb().transaction();
    m_db->searchDb().transaction();
    m_inTransaction = true;
}

void QualxDbPopulator::commitTransaction()
{
    m_db->mainDb().commit();
    m_db->searchDb().commit();
    m_inTransaction = false;
}

// -----------------------------------------------------------------------
// populateSpgrStat
//
// Inserts one row per (spacegroup, crystalSystem) pair into spgrstat
// in the .sq.info database. The ids column contains comma-separated
// card IDs (6-digit strings) in file order (ascending).
// -----------------------------------------------------------------------

void QualxDbPopulator::populateSpgrStat()
{
    QSqlDatabase db = m_db->infoDb();
    QSqlQuery q(db);
    q.prepare("INSERT INTO spgrstat (spacegroup, type, ids, n) VALUES (?, ?, ?, ?)");

    db.transaction();
    for (auto it = m_spgrStat.cbegin(); it != m_spgrStat.cend(); ++it) {
        const QString     &sg  = it.key().first;
        const QString     &sys = it.key().second;
        const QStringList &ids = it.value();

        q.addBindValue(sg);
        q.addBindValue(sys);
        q.addBindValue(ids.join(QLatin1Char(',')));
        q.addBindValue(ids.size());

        if (!q.exec())
            qWarning() << "spgrstat INSERT error:" << q.lastError().text() << sg;
    }
    db.commit();

    qDebug() << "populateSpgrStat: inserted" << m_spgrStat.size() << "rows.";
}

// -----------------------------------------------------------------------
// accumulateInfoStat
//
// Called for every card. Accumulates data into the 12 infostat maps.
// Cell parameters are "completed" using crystal system symmetry rules:
//   - cubic (C):              b=c=a
//   - tetragonal (T):         b=a
//   - hexagonal (H):          b=a,  γ=120°
//   - trigonal hex (R,α=0):   b=a,  γ=120°
//   - trigonal rhomb (R,α>0): b=c=a, β=γ=α
//   - others: only explicit values used
// Note: 90° angles are never filled in (standard default, not stored).
// stat_dens  ← calcDensity (Dx)   |  stat_cdens ← density (Dm)
// -----------------------------------------------------------------------

void QualxDbPopulator::accumulateInfoStat(const Pdf2Card &card)
{
    const QString &id = card.id;

    // ---- Determine crystal system ----
    const char cs = card.crystalSystem.isNull() ? 0 : card.crystalSystem.toLatin1();
    const bool isTrigHex   = (cs == 'R' && card.alpha <= 0.0);
    const bool isTrigRhomb = (cs == 'R' && card.alpha >  0.0);

    // ---- Complete cell parameters ----
    double a     = card.a     > 0.0 ? card.a     : 0.0;
    double b     = card.b     > 0.0 ? card.b     : 0.0;
    double c     = card.c     > 0.0 ? card.c     : 0.0;
    double alpha = card.alpha > 0.0 ? card.alpha : 0.0;
    double beta  = card.beta  > 0.0 ? card.beta  : 0.0;
    double gamma = card.gamma > 0.0 ? card.gamma : 0.0;

    if (a > 0.0) {
        // Fill b = a for systems where b is not independent
        if (b == 0.0 && (cs == 'C' || cs == 'T' || cs == 'H' || isTrigHex || isTrigRhomb))
            b = a;
        // Fill c = a for cubic and trigonal rhombohedral (a=b=c)
        if (c == 0.0 && (cs == 'C' || isTrigRhomb))
            c = a;
        // Fill γ = 120° for hexagonal and trigonal (hexagonal axes)
        if (gamma == 0.0 && (cs == 'H' || isTrigHex))
            gamma = 120.0;
        // Fill β = γ = α for trigonal rhombohedral (α=β=γ)
        if (isTrigRhomb && alpha > 0.0) {
            if (beta  == 0.0) beta  = alpha;
            if (gamma == 0.0) gamma = alpha;
        }
    }

    // ---- Accumulate cell parameters ----
    if (a     > 0.0) m_statA    [a    ].append(id);
    if (b     > 0.0) m_statB    [b    ].append(id);
    if (c     > 0.0) m_statC    [c    ].append(id);
    // Exactly 90° is the default (not stored in the reference).
    // For monoclinic (M), alpha and gamma are crystallographically defined as 90°
    // and are excluded from stat_alpha and stat_gamma even when explicitly stored.
    if (alpha > 0.0 && alpha != 90.0 && cs != 'M') m_statAlpha[alpha].append(id);
    if (beta  > 0.0 && beta  != 90.0)               m_statBeta [beta ].append(id);
    if (gamma > 0.0 && gamma != 90.0 && cs != 'M') m_statGamma[gamma].append(id);

    // ---- Volume (F9.2 → 2 decimal places) ----
    // Include 0.0 volumes (cards with explicit "0.000" in the type-3 record volume field).
    // Cards with no type-3 record have volume = -1.0f (excluded).
    if (card.volume >= 0.0f) {
        const double vol = std::round(double(card.volume) * 100.0) / 100.0;
        m_statVol[vol].append(id);
    }

    // ---- Densities: use raw float-as-double as the map key (no rounding). ----
    // stat_dens  = calcDensity (Dx): read from pos 38, 5 chars → max 9.999
    // stat_cdens = density (Dm):     read from pos 30, 4 chars → max 9.990
    if (card.calcDensity >= 0.0f)
        m_statDens [double(card.calcDensity)].append(id);
    if (card.density >= 0.0f)
        m_statCdens[double(card.density)   ].append(id);

    // ---- Color: split by comma, take first word of each segment + trailing space ----
    // E.g. "Black, brown" → adds card to "Black " and "brown " buckets.
    // Trailing punctuation (. ; : !) on the first word is stripped (normalisation).
    if (!card.color.isEmpty()) {
        const QStringList segments = card.color.split(QLatin1Char(','));
        for (const QString &seg : segments) {
            const QString trimmed = seg.trimmed();
            if (trimmed.isEmpty()) continue;
            const int spacePos = trimmed.indexOf(QLatin1Char(' '));
            QString word = (spacePos > 0 ? trimmed.left(spacePos) : trimmed);
            // Strip trailing punctuation
            while (!word.isEmpty()) {
                const QChar last = word.back();
                if (last == QLatin1Char('.') || last == QLatin1Char(';')
                    || last == QLatin1Char(':') || last == QLatin1Char('!'))
                    word.chop(1);
                else
                    break;
            }
            if (!word.isEmpty())
                m_statColor[word + QLatin1Char(' ')].append(id);
        }
    }

    // ---- Space group ----
    // For stat_spgr the reference uses normalised keys: left 7 chars, internal
    // spaces removed (e.g. "C 2/c" → "C2/c"). The raw string (with spaces) is
    // kept on the card for spgrstat and the id/info tables.
    if (!card.spaceGroup.trimmed().isEmpty()) {
        const QString sgNorm = card.spaceGroup.left(7).remove(QLatin1Char(' '));
        if (!sgNorm.isEmpty())
            m_statSpgr[sgNorm].append(id);
    }

    // ---- Crystal system (type) — same split logic as spgrstat ----
    if (cs != 0 && cs != 'X') {
        QString sysName;
        if (cs == 'R')
            sysName = isTrigRhomb ? QStringLiteral("Trigonal (rhombohedral axes)")
                                  : QStringLiteral("Trigonal (hexagonal axes)");
        else
            sysName = card.crystalSystemName();

        if (!sysName.isEmpty())
            m_statType[sysName].append(id);
    }
}

// -----------------------------------------------------------------------
// populateInfoStat
// -----------------------------------------------------------------------

void QualxDbPopulator::populateInfoStat()
{
    QSqlDatabase db = m_db->infoStatDb();

    insertNumTable(db, QStringLiteral("stat_a"),     m_statA);
    insertNumTable(db, QStringLiteral("stat_b"),     m_statB);
    insertNumTable(db, QStringLiteral("stat_c"),     m_statC);
    insertNumTable(db, QStringLiteral("stat_alpha"), m_statAlpha);
    insertNumTable(db, QStringLiteral("stat_beta"),  m_statBeta);
    insertNumTable(db, QStringLiteral("stat_gamma"), m_statGamma);
    insertNumTable(db, QStringLiteral("stat_vol"),   m_statVol);
    insertNumTable(db, QStringLiteral("stat_dens"),  m_statDens);
    insertNumTable(db, QStringLiteral("stat_cdens"), m_statCdens);

    insertTxtTable(db, QStringLiteral("stat_color"), m_statColor);
    insertTxtTable(db, QStringLiteral("stat_spgr"),  m_statSpgr);
    insertTxtTable(db, QStringLiteral("stat_type"),  m_statType);

    qDebug() << "populateInfoStat: done."
             << "a=" << m_statA.size() << "spgr=" << m_statSpgr.size()
             << "type=" << m_statType.size();
}

void QualxDbPopulator::insertNumTable(QSqlDatabase &db, const QString &table,
                                       const QMap<double, QStringList> &data)
{
    QSqlQuery q(db);
    q.prepare(QStringLiteral("INSERT INTO \"%1\" (val, ids, n) VALUES (?, ?, ?)").arg(table));

    db.transaction();
    for (auto it = data.cbegin(); it != data.cend(); ++it) {
        q.addBindValue(it.key());
        q.addBindValue(it.value().join(QLatin1Char(',')));
        q.addBindValue(it.value().size());
        if (!q.exec())
            qWarning() << table << "INSERT error:" << q.lastError().text();
    }
    db.commit();
}

void QualxDbPopulator::insertTxtTable(QSqlDatabase &db, const QString &table,
                                       const QMap<QString, QStringList> &data)
{
    QSqlQuery q(db);
    q.prepare(QStringLiteral("INSERT INTO \"%1\" (val, ids, n) VALUES (?, ?, ?)").arg(table));

    db.transaction();
    for (auto it = data.cbegin(); it != data.cend(); ++it) {
        q.addBindValue(it.key());
        q.addBindValue(it.value().join(QLatin1Char(',')));
        q.addBindValue(it.value().size());
        if (!q.exec())
            qWarning() << table << "INSERT error:" << q.lastError().text();
    }
    db.commit();
}
