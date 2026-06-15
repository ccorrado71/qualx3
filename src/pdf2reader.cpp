#include "pdf2reader.h"

#include <QFile>
#include <QFileInfo>
#include <QDebug>

// -----------------------------------------------------------------------
// Pdf2Card
// -----------------------------------------------------------------------

QString Pdf2Card::crystalSystemName() const
{
    switch (crystalSystem.toLatin1()) {
    case 'A': return QStringLiteral("Triclinic");
    case 'M': return QStringLiteral("Monoclinic");
    case 'O': return QStringLiteral("Orthorhombic");
    case 'T': return QStringLiteral("Tetragonal");
    case 'H': return QStringLiteral("Hexagonal");
    case 'C': return QStringLiteral("Cubic");
    case 'F': return QStringLiteral("Trigonal (hexagonal axes)");
    case 'R': return QStringLiteral("Trigonal (rhombohedral axes)");
    default:  return {};
    }
}

// -----------------------------------------------------------------------
// Pdf2Reader – static helpers
// -----------------------------------------------------------------------

/*
 * Extracts `length` characters from `line` starting at `start`,
 * strips leading and trailing whitespace.
 */
QString Pdf2Reader::field(const QByteArray &line, int start, int length)
{
    if (start >= line.size()) return {};
    const int end = qMin(start + length, line.size());
    return QString::fromLatin1(line.constData() + start, end - start).trimmed();
}

float Pdf2Reader::toFloat(const QString &s)
{
    if (s.isEmpty()) return -1.0f;
    bool ok = false;
    const float v = s.toFloat(&ok);
    return ok ? v : -1.0f;
}

double Pdf2Reader::toDouble(const QString &s)
{
    if (s.isEmpty()) return -1.0;
    bool ok = false;
    const double v = s.toDouble(&ok);
    return ok ? v : -1.0;
}

// -----------------------------------------------------------------------
// Pdf2Reader
// -----------------------------------------------------------------------

Pdf2Reader::Pdf2Reader(QObject *parent) : QObject(parent) {}

bool Pdf2Reader::parse(const QString &filePath)
{
    QFile file(filePath);
    if (!file.open(QIODevice::ReadOnly)) {
        qWarning() << "Pdf2Reader: cannot open" << filePath;
        return false;
    }

    loadCodensFile(filePath);

    m_current           = {};
    m_hasCurrentCard    = false;
    m_formulaContinues  = false;
    m_cancelled         = false;
    m_cardsEmitted      = 0;
    m_recordCount       = 0;
    m_cardStartRecord   = 0;
    m_fileSize          = file.size();

    // The file is a continuous stream of exactly 80-byte records with no line separators.
    QByteArray line(80, '\0');

    while (!file.atEnd() && !m_cancelled) {
        const qint64 bytesRead = file.read(line.data(), 80);
        if (bytesRead < 80) break; // end of file or incomplete record

        ++m_recordCount;
        processLine(line);
    }

    // Flush the last card in case it has no terminating 'K' record
    if (m_hasCurrentCard && !m_cancelled)
        finalizeCard();

    emit finished(m_cardsEmitted);
    return !m_cancelled;
}

// -----------------------------------------------------------------------
// loadCodensFile – CODEN -> full journal name lookup table
// -----------------------------------------------------------------------

/*
 * "codens.dat" is expected in the same folder as the PDF-2 file. It is a
 * stream of 80-byte records (no line separators), each containing:
 *   [0 - 5]  journal CODEN (6 chars, A6)
 *   [6]      separator space
 *   [7 -79]  full journal name (73 chars, free text)
 */
void Pdf2Reader::loadCodensFile(const QString &pdf2FilePath)
{
    m_codenMap.clear();

    const QFileInfo fi(pdf2FilePath);
    QFile file(fi.absolutePath() + QStringLiteral("/codens.dat"));
    if (!file.open(QIODevice::ReadOnly)) {
        qWarning() << "Pdf2Reader: codens.dat not found in" << fi.absolutePath();
        return;
    }

    QByteArray record(80, '\0');
    while (!file.atEnd()) {
        const qint64 bytesRead = file.read(record.data(), 80);
        if (bytesRead < 80) break;

        const QString coden  = field(record, 0, 6);
        const QString journal = field(record, 7, 73);
        if (!coden.isEmpty())
            m_codenMap.insert(coden, journal);
    }
}

// -----------------------------------------------------------------------
// processLine
// -----------------------------------------------------------------------

/*
 * Record layout (80 bytes, 0-based indices):
 *   [ 0 .. 71]  record-type-specific data
 *   [72 .. 77]  card ID (6-digit number, e.g. "010001")
 *   [78]        auxiliary flag (crystal system code for type-1 records)
 *   [79]        record type character
 */
void Pdf2Reader::processLine(const QByteArray &line)
{
    const char recType = line.at(79);

    // 'K' = end of card: emit the current card and reset state
    if (recType == 'K') {
        if (m_hasCurrentCard)
            finalizeCard();
        return;
    }

    // Card ID is always at positions 72-77
    const QString cardId = field(line, 72, 6);
    if (cardId.isEmpty()) return;

    // Safety: if the ID changes without a preceding 'K', finalize the previous card
    if (m_hasCurrentCard && cardId != m_current.id)
        finalizeCard();

    if (!m_hasCurrentCard) {
        m_current          = {};
        m_current.id       = cardId;
        m_current.nrec     = m_recordCount;   // first record of this card
        m_cardStartRecord  = m_recordCount;
        m_formulaContinues = false;
        m_hasCurrentCard   = true;
    }

    switch (recType) {
    case '1': parseRecordType1(line); break;
    case '3': parseRecordType3(line); break;
    case '4': parseRecordType4(line); break;
    case '5': parseRecordType5(line); break;
    case '6': parseRecordType6(line); break;
    case '7': parseRecordType7(line); break;
    case '9': parseRecordType9(line); break;
    case 'G': parseRecordTypeG(line); break;
    case 'I': parseRecordTypeI(line); break;
    case 'B': parseRecordTypeB(line); break;
    default:  break;
    }
}

// -----------------------------------------------------------------------
// finalizeCard – sort peaks and emit the signal
// -----------------------------------------------------------------------

void Pdf2Reader::finalizeCard()
{
    // Sort peaks by d-spacing descending
    std::sort(m_current.peaks.begin(), m_current.peaks.end(),
              [](const Pdf2Peak &a, const Pdf2Peak &b) { return a.d > b.d; });

    emit cardReady(std::move(m_current));

    ++m_cardsEmitted;
    if ((m_cardsEmitted % 10000) == 0)
        emit progress(m_cardsEmitted, m_recordCount * 80LL, m_fileSize);

    m_current          = {};
    m_hasCurrentCard   = false;
    m_formulaContinues = false;
}

// -----------------------------------------------------------------------
// Record-type parsers
// -----------------------------------------------------------------------

/*
 * Type '1' – Unit cell parameters
 * NBS spec (1-based): chars 1-9=a, 10-18=b, 19-27=c (F9.5);
 *                     chars 28-35=alpha, 36-43=beta, 44-51=gamma (F8.3);
 *                     char 79=crystal system code
 *   [0 - 8]   a  (9 chars, F9.5)
 *   [9 -17]   b  (9 chars, F9.5)
 *   [18-26]   c  (9 chars, F9.5)
 *   [27-34]   alpha (8 chars, F8.3)
 *   [35-42]   beta  (8 chars, F8.3)
 *   [43-50]   gamma (8 chars, F8.3)
 *   [78]      crystal system code (A/M/O/T/H/C/R/F)
 */
void Pdf2Reader::parseRecordType1(const QByteArray &line)
{
    m_current.a     = toDouble(field(line,  0, 9));
    m_current.b     = toDouble(field(line,  9, 9));
    m_current.c     = toDouble(field(line, 18, 9));
    m_current.alpha = toDouble(field(line, 27, 8));
    m_current.beta  = toDouble(field(line, 35, 8));
    m_current.gamma = toDouble(field(line, 43, 8));
    m_current.crystalSystem = QChar(QLatin1Char(line.at(78)));
}

/*
 * Type '3' – Space group, density, volume
 * NBS spec (1-based): chars 1-8=space group (8A1), 30-35=Dm (F6.3),
 *                     38-43=Dx (F6.3), 61-69=volume (F9.2)
 *   [0 - 7]   space group (8 chars, left-justified, Hermann-Mauguin notation)
 *   [30-35]   measured density Dm (6 chars, F6.3, g/cm³)
 *   [38-43]   calculated density Dx (6 chars, F6.3, g/cm³)
 *   [61-69]   unit cell volume (9 chars, F9.2, Å³)
 */
void Pdf2Reader::parseRecordType3(const QByteArray &line)
{
    // Read 8 chars from position 0 (raw Hermann-Mauguin notation, may contain
    // internal spaces like "C 2/c"). Normalization (space removal, 7-char truncation)
    // is applied selectively in QualxDbPopulator::accumulateInfoStat() for stat_spgr
    // only; spgrstat uses the raw string to preserve all distinct representations.
    const QString sg = field(line, 0, 8);
    if (!sg.isEmpty())
        m_current.spaceGroup = sg;

    // NBS*AIDS83: Dm (measured density) at 1-based positions 30-35 = 0-based [29-34], F6.3.
    // The original C code read 5 chars from position 30 (0-based), giving F5.3-style values
    // (max 9.990); high-density values >= 10.0 are read as their fractional part.
    const float dens  = toFloat(field(line, 30, 5));
    if (dens  >= 0.0f) m_current.density     = dens;

    // NBS*AIDS83: Dx (calculated density) at 1-based positions 38-43 = 0-based [37-42], F6.3.
    // The original C code read 5 chars from position 38 (0-based), giving F5.3-style values.
    const float cdens = toFloat(field(line, 38, 5));
    if (cdens >= 0.0f) m_current.calcDensity = cdens;

    const float vol   = toFloat(field(line, 60, 9));
    if (vol   >= 0.0f) m_current.volume     = vol;
}

/*
 * Type '4' – Crystal data
 * NBS*AIDS83 layout (0-based):
 *   [0 - 9]  space group symbol (not used: already parsed from type-3)
 *   [11-13]  space group number (ITA)
 *   [14]     space group variant letter
 *   [22-27]  Z – number of formula units per unit cell (right-justified integer)
 *   [30-33]  crystal density Dx (F4.2, g/cm³); 'A' at [34]
 *   [38-42]  absorption coefficient mu(CuKa) (F5.3, cm⁻¹); sometimes 'G' at [43]
 */
void Pdf2Reader::parseRecordType4(const QByteArray &line)
{
    const QString zStr = field(line, 22, 6);
    if (!zStr.isEmpty()) {
        bool ok = false;
        const int z = zStr.toInt(&ok);
        if (ok && z > 0) m_current.z = z;
    }

    const float mu = toFloat(field(line, 38, 5));
    if (mu >= 0.0f) m_current.muCuKa = mu;
}

/*
 * Type '9' – Bibliographic reference
 * NBS*AIDS83 layout (0-based):
 * Type-9 record layout (0-based, data area [0-71]):
 *   [0 - 5]  journal CODEN (6 chars, e.g. "ANCHAM"); resolved to the full
 *            journal name via codens.dat (see loadCodensFile)
 *   [6 - 9]  volume number (4 chars, right-justified)
 *   [10-14]  first page (5 chars, right-justified)
 *   [15]     separator space
 *   [16-19]  publication year (4 chars)
 *   [20-68]  author list (49 chars, free text)
 *   [69]     occurrence index: blank or '1' = primary reference,
 *            '2', '3', … = additional references (skipped)
 *   [70]     card type marker ('P'=preliminary, 'D'=deleted)
 *   [71]     usually blank
 */
void Pdf2Reader::parseRecordType9(const QByteArray &line)
{
    // Skip secondary references (sequence digit '2' or higher at position 69)
    if (line.size() > 69) {
        const char seq = line.at(69);
        if (seq >= '2' && seq <= '9') return;
    }

    // Only store the first bibliography record encountered for this card
    if (!m_current.journal.isEmpty()) return;

    const QString coden = field(line, 0, 6);
    const auto it = m_codenMap.constFind(coden);
    m_current.journal       = (it != m_codenMap.constEnd()) ? it.value() : coden;
    m_current.journalVolume = field(line,  6,  4);
    m_current.pageStart     = field(line, 10,  5);
    m_current.journalYear   = field(line, 16,  4);  // [15] is separator space
    m_current.authors       = field(line, 20, 49);  // [20-68], excludes markers at [69-71]
}

/*
 * Type '5' – Material class and registration indicators (NBS Crystal Data)
 *            / Subfile codes (PDF-2)
 * PDF-2 layout:
 *   [0-3]    4 single-character codes
 *   [5-7]    subfile code 1 (3 chars)
 *   [9-11]   subfile code 2 (3 chars)
 *   [13-15]  subfile code 3 (3 chars)
 *   [17-19]  subfile code 4 (3 chars)
 */
void Pdf2Reader::parseRecordType5(const QByteArray &line)
{
    for (int i = 0; i < 4 && i < line.size(); ++i) {
        if (line.at(i) != ' ')
            m_current.subfiles.append(QString(QChar(QLatin1Char(line.at(i)))));
    }
    for (int i = 0; i < 4; ++i) {
        const QString code = field(line, 5 + i * 4, 3);
        if (code.size() >= 2 && code.at(1).isUpper())
            m_current.subfiles.append(code);
    }
}

/*
 * Type '6' – Compound name or mineral name
 * NBS spec (1-based): chars 1-67=name (67A1), char 69=index code, char 70=continuation
 *   [0 -66]  name text (67 chars, left-justified)
 *   [68]     index code: blank/P/F = compound name,  M = mineral name
 *   [69]     continuation code: 'C' = name continues on the next record
 */
void Pdf2Reader::parseRecordType6(const QByteArray &line)
{
    if (line.size() < 69) return;
    const char flag = line.at(68);
    const QString name = field(line, 0, 67);

    if (flag == 'P' || flag == 'F' || flag == ' ')
        m_current.name = name;
    else if (flag == 'M')
        m_current.mineralName = name;
}

/*
 * Type '7' – Chemical formula (may span multiple consecutive records)
 * NBS spec (1-based): chars 1-67=formula (67A1), char 70=continuation code
 *   [0 -66]  formula text (67 chars, left-justified)
 *   [69]     continuation code: 'C' = formula continues on the next record
 *
 * Cards often have a second type-7 record containing the expanded chemical
 * name (e.g. "Pb(C2H3O2)2·Pb(OH)2·H2O"). Only the first record (and any
 * explicit continuations) should be used for element extraction.
 * This mirrors the 'continuaformula' logic in the original C code.
 */
void Pdf2Reader::parseRecordType7(const QByteArray &line)
{
    // Accept this record only if it is the first type-7 for this card,
    // or if the previous type-7 explicitly flagged a continuation.
    if (!m_current.chemicalFormula.isEmpty() && !m_formulaContinues)
        return;

    m_current.chemicalFormula += field(line, 0, 67);
    m_formulaContinues = (line.size() > 69 && line.at(69) == 'C');
}

/*
 * Type 'G' – Quality code and Reference Intensity Ratio (PDF-2 specific)
 *   [4 - 9]  RIR value (6 chars)
 *   [24]     quality code character
 *   [71]     'D' if the card is deleted/obsolete (overrides quality)
 */
void Pdf2Reader::parseRecordTypeG(const QByteArray &line)
{
    if (line.size() > 71 && line.at(71) == 'D')
        m_current.quality = QLatin1Char('D');
    else if (line.size() > 24)
        m_current.quality = QChar(QLatin1Char(line.at(24)));

    const float rir = toFloat(field(line, 4, 6));
    if (rir > 0.0f) m_current.rir = rir;
}

/*
 * Type 'I' – Diffraction peaks (PDF-2 specific): up to 3 (d, intensity) pairs per record
 * A pair is absent when the second byte of its d field is a space.
 *   Pair 1:  d [0 - 6]   I [7 - 9]
 *   Pair 2:  d [23-29]   I [30-32]
 *   Pair 3:  d [46-52]   I [53-55]
 */
void Pdf2Reader::parseRecordTypeI(const QByteArray &line)
{
    static constexpr int dStarts[3] = {  0, 23, 46 };
    static constexpr int iStarts[3] = {  7, 30, 53 };

    for (int k = 0; k < 3; ++k) {
        const int ds = dStarts[k];
        if (ds + 1 >= line.size()) break;
        if (line.at(ds + 1) == ' ') continue; // field absent

        const float d = toFloat(field(line, ds,         7));
        const float i = toFloat(field(line, iStarts[k], 3));
        if (d > 0.0f)
            m_current.peaks.append({d, i});
    }
}

/*
 * Type 'B' – Color description (PDF-2 specific; present only when pos 67-68 == "CL")
 *   [0-65]  color text (66 chars)
 */
void Pdf2Reader::parseRecordTypeB(const QByteArray &line)
{
    if (line.size() >= 69 && line.at(67) == 'C' && line.at(68) == 'L')
        m_current.color = field(line, 0, 66);
}
