#include "cifreader.h"

#include <QDirIterator>

CifReader::CifReader(QObject *parent)
    : QObject(parent)
{}

void CifReader::scan(const QString &folder, bool recursive)
{
    QDirIterator::IteratorFlags flags = QDirIterator::NoIteratorFlags;
    if (recursive)
        flags |= QDirIterator::Subdirectories;

    // QDirIterator is lazy: it fetches one entry at a time from the OS,
    // so memory usage is O(1) regardless of how many files are in the tree.
    QDirIterator it(folder, QStringList() << QStringLiteral("*.cif"),
                    QDir::Files, flags);

    int count = 0;
    while (it.hasNext()) {
        emit cifFound(it.next());
        ++count;
    }

    emit finished(count);
}
