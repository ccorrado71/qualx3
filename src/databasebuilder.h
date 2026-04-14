#pragma once

#include <QString>
#include <functional>

class QWidget;

// -----------------------------------------------------------------------
// DatabaseBuilder
//
// Static helpers to build Qualx databases from different sources.
// Shared between the command-line (main.cpp) and the GUI dialogs so that
// the construction logic lives in exactly one place.
// -----------------------------------------------------------------------
class DatabaseBuilder
{
public:
    // Progress callback for PDF-2: (cardsProcessed, bytesRead, totalBytes).
    using ProgressFn = std::function<void(int, qint64, qint64)>;

    // Progress callback for CIF: (ok, skipped, errors, currentFilePath).
    using CifProgressFn = std::function<void(int, int, int, const QString &)>;

    // Counters returned by the CLI CIF overload.
    struct CifBuildStats {
        int ok      = 0;
        int skipped = 0;
        int errors  = 0;
    };

    // ---- PDF-2 ---------------------------------------------------------

    // Builds a PDF-2 (NBS*AIDS83) database at basePath.
    static bool buildPdfDatabase(const QString &basePath,
                                 const QString &pdf2FilePath,
                                 ProgressFn progress = nullptr);

    // Same, but shows a modal QProgressDialog owned by parent.
    static bool buildPdfDatabase(const QString &basePath,
                                 const QString &pdf2FilePath,
                                 QWidget *parent,
                                 bool *outCancelled = nullptr);

    // ---- CIF -----------------------------------------------------------

    // Builds a CIF database at basePath from *.cif files in cifDir.
    // inorganicOnly: skip organic structures (ier==1 from Fortran).
    // progress: called for each file; pass nullptr for no reporting.
    // stats: if non-null, filled with ok/skipped/error counts.
    static bool buildCifDatabase(const QString &basePath,
                                 const QString &cifDir,
                                 bool recursive,
                                 bool inorganicOnly,
                                 CifProgressFn progress = nullptr,
                                 CifBuildStats *stats = nullptr);

    // Same, but shows a modal QProgressDialog owned by parent.
    // inorganicOnly is always false in this overload.
    static bool buildCifDatabase(const QString &basePath,
                                 const QString &cifDir,
                                 bool recursive,
                                 QWidget *parent,
                                 bool *outCancelled = nullptr);

    // ---- Utilities -----------------------------------------------------

    // Queries infodb.ncard from basePath + ".sq".
    // Returns -1 on error (file missing, query failed, …).
    static int queryEntries(const QString &basePath);
};
