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
//
// Usage (CLI):
//   DatabaseBuilder::buildPdfDatabase(basePath, pdf2File,
//       [](int cards, qint64) {
//           printf("\r  %d cards processed...", cards);
//           fflush(stdout);
//       });
//
// Usage (GUI):
//   DatabaseBuilder::buildPdfDatabase(basePath, pdf2File, parentWidget);
// -----------------------------------------------------------------------
class DatabaseBuilder
{
public:
    // Progress callback: (cardsProcessed, bytesRead, totalBytes).
    // Called periodically during construction; pass nullptr for no reporting.
    using ProgressFn = std::function<void(int, qint64, qint64)>;

    // Builds a PDF-2 (NBS*AIDS83) database at basePath.
    // Returns true on success.
    static bool buildPdfDatabase(const QString &basePath,
                                 const QString &pdf2FilePath,
                                 ProgressFn progress = nullptr);

    // Same, but shows a modal QProgressDialog owned by parent.
    // If the user cancels, *outCancelled is set to true (when not nullptr)
    // and the function returns false.
    static bool buildPdfDatabase(const QString &basePath,
                                 const QString &pdf2FilePath,
                                 QWidget *parent,
                                 bool *outCancelled = nullptr);

    // Queries infodb.ncard from basePath + ".sq".
    // Returns -1 on error (file missing, query failed, …).
    static int queryEntries(const QString &basePath);
};
