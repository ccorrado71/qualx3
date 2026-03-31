#pragma once

#include <QObject>
#include <QString>
#include <QVector>
#include <QStringList>

// A single diffraction peak: d-spacing and relative intensity
struct Pdf2Peak {
    float d         = 0.0f; // d-spacing in Angstroms
    float intensity = 0.0f; // relative intensity (0-999)
};

// A card (entry) from the PDF-2 / NBS*AIDS83 database
struct Pdf2Card {
    QString id;              // 6-digit numeric card ID ("010001")
    QString name;            // Compound name (record type 6, flag P/F)
    QString mineralName;     // Mineral name (record type 6, flag M)
    QString chemicalFormula; // Chemical formula (record type 7)
    QString spaceGroup;      // Space group in Hermann-Mauguin notation (record type 3, pos 0-7)
    QChar   crystalSystem;   // Crystal system code (pos 78 of type-1 record):
                             // A=Triclinic, M=Monoclinic, O=Orthorhombic, T=Tetragonal,
                             // H=Hexagonal, C=Cubic, F/R=Trigonal
    double a     = -1.0;    // Unit cell parameter a (Å)
    double b     = -1.0;    // Unit cell parameter b (Å)
    double c     = -1.0;    // Unit cell parameter c (Å)
    double alpha = -1.0;    // Cell angle α (°)
    double beta  = -1.0;    // Cell angle β (°)
    double gamma = -1.0;    // Cell angle γ (°)
    float density     = -1.0f; // Measured density (g/cm³)
    float calcDensity = -1.0f; // Calculated density (g/cm³)
    float volume      = -1.0f; // Unit cell volume (Å³)
    QChar  quality;          // Quality code: ' ' '*' 'I' 'O' 'C' 'B' 'D'
    float  rir    = 0.0f;    // Reference Intensity Ratio (I/I_corundum)
    QVector<Pdf2Peak> peaks; // Diffraction peaks, sorted by d descending
    QStringList subfiles;    // Subfile codes (record type 5)
    QString color;           // Color description (record type B, tag CL)

    // From record type 4 (crystal data)
    int   z      = 0;       // Number of formula units per unit cell
    float muCuKa = -1.0f;  // Absorption coefficient mu(CuKa) (cm⁻¹)

    // From record type 9 (bibliographic reference – first occurrence only)
    QString journal;        // Journal CODEN (e.g. "ANCHAM")
    QString journalVolume;  // Volume number
    QString journalYear;    // Publication year (4 chars)
    QString pageStart;      // First page
    QString authors;        // Author list

    // Returns the crystal system as a human-readable string
    QString crystalSystemName() const;

    int nrec = 0;  // 1-based record number of the first record of this card in the .dat file

    bool isValid() const { return !id.isEmpty() && !peaks.isEmpty(); }
};

// -----------------------------------------------------------------------
// Streaming reader for PDF-2 / NBS*AIDS83 files (80-byte fixed-width records)
//
// Usage:
//   1. Connect cardReady() to a slot that processes/stores the card
//   2. Connect progress() to update a progress bar
//   3. Call parse()
//
// Each card is emitted and then discarded: RAM usage stays constant (~a few MB).
// -----------------------------------------------------------------------
class Pdf2Reader : public QObject
{
    Q_OBJECT

public:
    explicit Pdf2Reader(QObject *parent = nullptr);

    // Reads the file in streaming mode. Returns false if the file cannot be opened.
    // Emits cardReady() for each completed card.
    // Emits progress() every ~10000 cards.
    bool parse(const QString &filePath);

signals:
    // Emitted for each completed card. The card is passed by value:
    // the receiver is the sole owner and may move it wherever needed.
    void cardReady(Pdf2Card card);

    // Emitted periodically: cardsEmitted = cards emitted so far, fileSize = total bytes
    void progress(int cardsEmitted, qint64 fileSize);

    // Emitted when parsing is complete
    void finished(int totalCards);

private:
    static QString field(const QByteArray &line, int start, int length);
    static float   toFloat(const QString &s);
    static double  toDouble(const QString &s);

    void processLine(const QByteArray &line);
    void finalizeCard();

    void parseRecordType1(const QByteArray &line);
    void parseRecordType3(const QByteArray &line);
    void parseRecordType4(const QByteArray &line);
    void parseRecordType5(const QByteArray &line);
    void parseRecordType6(const QByteArray &line);
    void parseRecordType7(const QByteArray &line);
    void parseRecordType9(const QByteArray &line);
    void parseRecordTypeG(const QByteArray &line);
    void parseRecordTypeI(const QByteArray &line);
    void parseRecordTypeB(const QByteArray &line);

    Pdf2Card m_current;
    bool m_hasCurrentCard   = false;
    bool m_formulaContinues = false;
    int  m_cardsEmitted     = 0;
    int  m_recordCount      = 0;  // total 80-byte records read so far
    int  m_cardStartRecord  = 0;  // record number when the current card started
};
