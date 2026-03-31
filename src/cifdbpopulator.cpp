#include "cifdbpopulator.h"

#include <QFileInfo>
#include <QDate>
#include <QSqlError>
#include <QDebug>
#include <algorithm>
#include <numeric>

// -----------------------------------------------------------------------
// Construction
// -----------------------------------------------------------------------

CifDbPopulator::CifDbPopulator(QualxDbCreator *db, QObject *parent)
    : QObject(parent)
    , m_db(db)
{
    // id table: CifFiles variant has natoms + nreflections instead of bestdval
    m_idQuery = QSqlQuery(m_db->mainDb());
    m_idQuery.prepare(
        "INSERT INTO id "
        "(id, name, mineralname, chemical_formula, spacegroup, quality, rir, "
        " nrec, natoms, nreflections, nd, dvalue, intensita, n) "
        "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)");

    m_chemQuery = QSqlQuery(m_db->mainDb());
    m_chemQuery.prepare(
        "INSERT OR IGNORE INTO chemical (id, chemical_element) VALUES (?, ?)");

    m_subQuery = QSqlQuery(m_db->mainDb());
    m_subQuery.prepare(
        "INSERT OR IGNORE INTO subfiles (id, subfile) VALUES (?, ?)");

    // info table is in .sq.info for CifFiles
    m_infoQuery = QSqlQuery(m_db->infoDb());
    m_infoQuery.prepare(
        "INSERT OR IGNORE INTO info "
        "(id, authors, journal, journal_year, journal_volume, journal_issue, "
        " page_start, page_end, color, crystal_density, z, spacegroup, type, "
        " volume, density, \"mu(CuKa)\", natoms, nreflections, "
        " a, b, c, alpha, beta, gamma, rir, h, k, l, mul) "
        "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)");

    m_topQuery = QSqlQuery(m_db->searchDb());
    m_topQuery.prepare(
        "INSERT OR IGNORE INTO top (id, n, dval) VALUES (?, ?, ?)");
}

// -----------------------------------------------------------------------
// Slot: called for every CIF file successfully read
// -----------------------------------------------------------------------

void CifDbPopulator::onCifReady(const QString &filePath, const CifCrystalInfo &info)
{
    if (!m_inTransaction)
        beginTransaction();

    const int id = cifIdFromPath(filePath);

    insertChemical(id, info);
    insertId      (id, info);
    insertSubfile (id, info);
    insertInfo    (id, info);
    insertTop     (id, info);

    accumulateInfoStat(id, info);

    const QString spg = QString::fromLatin1(info.spg_sym).trimmed();
    const QString sys = QString::fromLatin1(info.crysys).trimmed();
    if (!spg.isEmpty() && !sys.isEmpty())
        m_spgrStat[{spg, sys}].append(QString::number(id));

    ++m_cardCount;
    if ((m_cardCount % kTransactionSize) == 0) {
        commitTransaction();
        beginTransaction();
    }
}

// -----------------------------------------------------------------------
// Slot: called when CifReader has finished scanning
// -----------------------------------------------------------------------

void CifDbPopulator::onFinished(int totalFiles)
{
    if (m_inTransaction)
        commitTransaction();

    QSqlQuery q(m_db->mainDb());
    q.prepare("INSERT INTO infodb (id, date, ncard, type, source) VALUES (?, ?, ?, ?, ?)");
    q.addBindValue(QStringLiteral("0"));
    q.addBindValue(QDate::currentDate().toString(Qt::ISODate));
    q.addBindValue(m_cardCount);
    q.addBindValue(QStringLiteral("COD"));
    q.addBindValue(QStringLiteral("you"));
    if (!q.exec())
        qWarning() << "infodb INSERT error:" << q.lastError().text();

    populateSpgrStat();
    populateInfoStat();

    qDebug() << "CifDbPopulator: done." << m_cardCount << "of" << totalFiles << "cards inserted.";
}

// -----------------------------------------------------------------------
// insertChemical
// -----------------------------------------------------------------------

void CifDbPopulator::insertChemical(int id, const CifCrystalInfo &info)
{
    for (int i = 0; i < info.nelem; ++i) {
        const QString el = QString::fromLatin1(info.specie_label[i]).trimmed();
        if (el.isEmpty()) continue;
        m_chemQuery.addBindValue(id);
        m_chemQuery.addBindValue(el);
        if (!m_chemQuery.exec())
            qWarning() << "chemical INSERT error:" << m_chemQuery.lastError().text()
                       << "id=" << id << "element=" << el;
    }
}

// -----------------------------------------------------------------------
// insertId
// -----------------------------------------------------------------------

void CifDbPopulator::insertId(int id, const CifCrystalInfo &info)
{
    ++m_rowCounter;

    int nd = 0;
    const QString dval = buildDvalString(info, nd);
    const QString ival = buildIvalString(info);

    m_idQuery.addBindValue(id);
    m_idQuery.addBindValue(QVariant());                            // name – not available
    m_idQuery.addBindValue(QVariant());                            // mineralname – not available
    m_idQuery.addBindValue(QString::fromLatin1(info.sform).trimmed());
    m_idQuery.addBindValue(QString::fromLatin1(info.spg_sym).trimmed());
    m_idQuery.addBindValue(QVariant());                            // quality – not available
    m_idQuery.addBindValue(info.rir > 0.0f ? QVariant(double(info.rir)) : QVariant());
    m_idQuery.addBindValue(0);                                     // nrec – not applicable for CIF
    m_idQuery.addBindValue(info.nat);
    m_idQuery.addBindValue(info.nrefl);
    m_idQuery.addBindValue(nd);
    m_idQuery.addBindValue(dval.isEmpty() ? QVariant() : QVariant(dval));
    m_idQuery.addBindValue(ival.isEmpty() ? QVariant() : QVariant(ival));
    m_idQuery.addBindValue(m_rowCounter);

    if (!m_idQuery.exec())
        qWarning() << "id INSERT error:" << m_idQuery.lastError().text() << "id=" << id;
}

// -----------------------------------------------------------------------
// insertSubfile
// -----------------------------------------------------------------------

void CifDbPopulator::insertSubfile(int id, const CifCrystalInfo &info)
{
    const QString sub = QString::fromLatin1(info.subfile).trimmed();
    if (sub.isEmpty()) return;
    m_subQuery.addBindValue(id);
    m_subQuery.addBindValue(sub);
    if (!m_subQuery.exec())
        qWarning() << "subfiles INSERT error:" << m_subQuery.lastError().text() << "id=" << id;
}

// -----------------------------------------------------------------------
// insertInfo  (goes into .sq.info for CifFiles)
// -----------------------------------------------------------------------

void CifDbPopulator::insertInfo(int id, const CifCrystalInfo &info)
{
    const QByteArray hBlob   = intsToBlob(info.refl_h,    info.nrefl_print);
    const QByteArray kBlob   = intsToBlob(info.refl_k,    info.nrefl_print);
    const QByteArray lBlob   = intsToBlob(info.refl_l,    info.nrefl_print);
    const QByteArray mulBlob = intsToBlob(info.refl_mult, info.nrefl_print);

    m_infoQuery.addBindValue(QString::number(id));               // id VARCHAR(15)
    m_infoQuery.addBindValue(QStringLiteral(""));                // authors – not available
    m_infoQuery.addBindValue(QStringLiteral(""));                // journal – not available
    m_infoQuery.addBindValue(QStringLiteral(""));                // journal_year – not available
    m_infoQuery.addBindValue(QStringLiteral(""));                // journal_volume – not available
    m_infoQuery.addBindValue(QStringLiteral(""));                // journal_issue – not available
    m_infoQuery.addBindValue(QStringLiteral(""));                // page_start – not available
    m_infoQuery.addBindValue(QStringLiteral(""));                // page_end – not available
    m_infoQuery.addBindValue(QStringLiteral(""));                // color – not available
    m_infoQuery.addBindValue(0.0);                               // crystal_density (measured Dx) – not available
    m_infoQuery.addBindValue(info.zval > 0 ? QString::number(info.zval) : QStringLiteral(""));
    m_infoQuery.addBindValue(QString::fromLatin1(info.spg_sym).trimmed());
    m_infoQuery.addBindValue(QString::fromLatin1(info.crysys).trimmed());
    m_infoQuery.addBindValue(double(info.vol));                  // volume REAL
    m_infoQuery.addBindValue(double(info.dens));                 // density (calculated Dc)
    m_infoQuery.addBindValue(info.mu >= 0.0f
                             ? QString::number(double(info.mu), 'f', 4)
                             : QStringLiteral(""));
    m_infoQuery.addBindValue(info.nat);
    m_infoQuery.addBindValue(info.nrefl);
    m_infoQuery.addBindValue(double(info.cellpar[0]));           // a
    m_infoQuery.addBindValue(double(info.cellpar[1]));           // b
    m_infoQuery.addBindValue(double(info.cellpar[2]));           // c
    m_infoQuery.addBindValue(double(info.cellpar[3]));           // alpha
    m_infoQuery.addBindValue(double(info.cellpar[4]));           // beta
    m_infoQuery.addBindValue(double(info.cellpar[5]));           // gamma
    m_infoQuery.addBindValue(info.rir > 0.0f
                             ? QString::number(double(info.rir), 'f', 4)
                             : QStringLiteral(""));
    m_infoQuery.addBindValue(hBlob);
    m_infoQuery.addBindValue(kBlob);
    m_infoQuery.addBindValue(lBlob);
    m_infoQuery.addBindValue(mulBlob);

    if (!m_infoQuery.exec())
        qWarning() << "info INSERT error:" << m_infoQuery.lastError().text() << "id=" << id;
}

// -----------------------------------------------------------------------
// insertTop
// -----------------------------------------------------------------------

void CifDbPopulator::insertTop(int id, const CifCrystalInfo &info)
{
    int n = 0;
    const QString dval = buildTopDval(info, n);

    m_topQuery.addBindValue(id);
    m_topQuery.addBindValue(n);
    m_topQuery.addBindValue(dval);
    if (!m_topQuery.exec())
        qWarning() << "top INSERT error:" << m_topQuery.lastError().text() << "id=" << id;
}

// -----------------------------------------------------------------------
// accumulateInfoStat
// -----------------------------------------------------------------------

void CifDbPopulator::accumulateInfoStat(int id, const CifCrystalInfo &info)
{
    const QString sid = QString::number(id);

    const double a     = double(info.cellpar[0]);
    const double b     = double(info.cellpar[1]);
    const double c     = double(info.cellpar[2]);
    const double alpha = double(info.cellpar[3]);
    const double beta  = double(info.cellpar[4]);
    const double gamma = double(info.cellpar[5]);

    const QString sys = QString::fromLatin1(info.crysys).trimmed();
    const bool isTrigRhomb = sys.contains(QStringLiteral("rhombohedral"), Qt::CaseInsensitive);
    const bool isTrigHex   = sys.contains(QStringLiteral("hexagonal"),   Qt::CaseInsensitive)
                          && sys.contains(QStringLiteral("trigonal"),     Qt::CaseInsensitive);
    const bool isCubic     = sys.compare(QStringLiteral("Cubic"),        Qt::CaseInsensitive) == 0;
    const bool isTetra     = sys.compare(QStringLiteral("Tetragonal"),   Qt::CaseInsensitive) == 0;
    const bool isHex       = sys.compare(QStringLiteral("Hexagonal"),    Qt::CaseInsensitive) == 0;

    // Complete cell parameters using crystal system symmetry
    double bFull = b, cFull = c, gammaFull = gamma, betaFull = beta, alphaFull = alpha;
    if (a > 0.0) {
        if (bFull == 0.0 && (isCubic || isTetra || isHex || isTrigHex || isTrigRhomb))
            bFull = a;
        if (cFull == 0.0 && (isCubic || isTrigRhomb))
            cFull = a;
        if (gammaFull == 0.0 && (isHex || isTrigHex))
            gammaFull = 120.0;
        if (isTrigRhomb && alphaFull > 0.0) {
            if (betaFull  == 0.0) betaFull  = alphaFull;
            if (gammaFull == 0.0) gammaFull = alphaFull;
        }
    }

    if (a      > 0.0) m_statA    [a          ].append(sid);
    if (bFull  > 0.0) m_statB    [bFull      ].append(sid);
    if (cFull  > 0.0) m_statC    [cFull      ].append(sid);
    if (alphaFull > 0.0 && alphaFull != 90.0) m_statAlpha[alphaFull].append(sid);
    if (betaFull  > 0.0 && betaFull  != 90.0) m_statBeta [betaFull ].append(sid);
    if (gammaFull > 0.0 && gammaFull != 90.0) m_statGamma[gammaFull].append(sid);

    if (info.vol > 0.0f) {
        const double vol = std::round(double(info.vol) * 100.0) / 100.0;
        m_statVol[vol].append(sid);
    }

    // Only calculated density available
    if (info.dens > 0.0f)
        m_statDens[double(info.dens)].append(sid);

    const QString spg = QString::fromLatin1(info.spg_sym).trimmed();
    if (!spg.isEmpty()) {
        const QString sgNorm = spg.left(7).remove(QLatin1Char(' '));
        if (!sgNorm.isEmpty())
            m_statSpgr[sgNorm].append(sid);
    }

    if (!sys.isEmpty())
        m_statType[sys].append(sid);
}

// -----------------------------------------------------------------------
// populateSpgrStat
// -----------------------------------------------------------------------

void CifDbPopulator::populateSpgrStat()
{
    QSqlDatabase db = m_db->infoDb();
    QSqlQuery q(db);
    q.prepare("INSERT INTO spgrstat (spacegroup, type, ids, n) VALUES (?, ?, ?, ?)");

    db.transaction();
    for (auto it = m_spgrStat.cbegin(); it != m_spgrStat.cend(); ++it) {
        q.addBindValue(it.key().first);
        q.addBindValue(it.key().second);
        q.addBindValue(it.value().join(QLatin1Char(',')));
        q.addBindValue(it.value().size());
        if (!q.exec())
            qWarning() << "spgrstat INSERT error:" << q.lastError().text();
    }
    db.commit();

    qDebug() << "populateSpgrStat: inserted" << m_spgrStat.size() << "rows.";
}

// -----------------------------------------------------------------------
// populateInfoStat
// -----------------------------------------------------------------------

void CifDbPopulator::populateInfoStat()
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

    // stat_cdens (measured density) not available for CIF source

    insertTxtTable(db, QStringLiteral("stat_spgr"),  m_statSpgr);
    insertTxtTable(db, QStringLiteral("stat_type"),  m_statType);

    // stat_color not available for CIF source

    qDebug() << "populateInfoStat: done."
             << "a=" << m_statA.size() << "spgr=" << m_statSpgr.size()
             << "type=" << m_statType.size();
}

void CifDbPopulator::insertNumTable(QSqlDatabase &db, const QString &table,
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

void CifDbPopulator::insertTxtTable(QSqlDatabase &db, const QString &table,
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

// -----------------------------------------------------------------------
// Transaction helpers
// -----------------------------------------------------------------------

void CifDbPopulator::beginTransaction()
{
    m_db->mainDb().transaction();
    m_db->infoDb().transaction();
    m_db->searchDb().transaction();
    m_inTransaction = true;
}

void CifDbPopulator::commitTransaction()
{
    m_db->mainDb().commit();
    m_db->infoDb().commit();
    m_db->searchDb().commit();
    m_inTransaction = false;
}

// -----------------------------------------------------------------------
// Static helpers
// -----------------------------------------------------------------------

int CifDbPopulator::cifIdFromPath(const QString &filePath)
{
    return QFileInfo(filePath).baseName().toInt();
}

QVector<int> CifDbPopulator::sortedByD(const CifCrystalInfo &info)
{
    const int n = info.nrefl_print;
    QVector<int> idx(n);
    std::iota(idx.begin(), idx.end(), 0);
    std::stable_sort(idx.begin(), idx.end(), [&](int a, int b) {
        return info.refl_d[a] > info.refl_d[b];
    });
    return idx;
}

QString CifDbPopulator::buildDvalString(const CifCrystalInfo &info, int &ndOut)
{
    const QVector<int> idx = sortedByD(info);
    ndOut = idx.size();
    QString result;
    for (int i : idx)
        result += QString::asprintf("%f,", double(info.refl_d[i]));
    return result;
}

QString CifDbPopulator::buildIvalString(const CifCrystalInfo &info)
{
    const QVector<int> idx = sortedByD(info);
    QString result;
    for (int i : idx)
        result += QString::asprintf("%f,", double(info.refl_ipct[i]));
    return result;
}

QString CifDbPopulator::buildTopDval(const CifCrystalInfo &info, int &nOut)
{
    // refl_d is already sorted by intensity descending from Fortran —
    // take the first kNbestd as the "top" d-values.
    nOut = qMin(info.nrefl_print, kNbestd);
    QStringList parts;
    parts.reserve(nOut);
    for (int i = 0; i < nOut; ++i)
        parts.append(QString::asprintf("%f", double(info.refl_d[i])));
    return parts.join(QLatin1Char(','));
}

QByteArray CifDbPopulator::intsToBlob(const int *arr, int n)
{
    if (n <= 0) return QByteArray(1, '\0');
    QByteArray blob(n * static_cast<int>(sizeof(qint32)), Qt::Uninitialized);
    qint32 *dst = reinterpret_cast<qint32 *>(blob.data());
    for (int i = 0; i < n; ++i)
        dst[i] = static_cast<qint32>(arr[i]);
    return blob;
}
