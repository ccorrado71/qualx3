#pragma once

#include <QObject>
#include <QString>

// -----------------------------------------------------------------------
// CifReader
//
// Scans a folder for .cif files and processes them one at a time.
// Uses QDirIterator internally, so only one directory entry is held in
// memory at any given moment — suitable for very large CIF collections.
//
// Usage:
//   CifReader reader;
//   QObject::connect(&reader, &CifReader::cifFound,
//                    [](const QString &path) { qDebug() << path; });
//   QObject::connect(&reader, &CifReader::finished,
//                    [](int n) { qDebug() << "Total:" << n; });
//   reader.scan("/path/to/cif/folder", /*recursive=*/true);
// -----------------------------------------------------------------------
class CifReader : public QObject
{
    Q_OBJECT

public:
    explicit CifReader(QObject *parent = nullptr);

    // Scans folder for *.cif files (recursively if recursive=true).
    // Emits cifFound() for each file found, then finished().
    void scan(const QString &folder, bool recursive);

signals:
    // Emitted for each .cif file found, in filesystem order.
    void cifFound(const QString &filePath);

    // Emitted once all files have been processed.
    // totalFiles is the total number of .cif files found.
    void finished(int totalFiles);
};
