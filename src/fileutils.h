#ifndef FILEUTILS_H
#define FILEUTILS_H

#include <QSettings>
#include <QAction>

namespace fileutils {

QString getDirDataFiles(QStringList &dataFiles, QString &err);
QString setCurrentDirFromFile(const QString &file);
//void findFiles(QStringList &files, const QString &path, const QString &filter);
QStringList findFiles(const QString &path, const QStringList &filters);
//void findFilesInPATH(QStringList &files, const QString &filter);
QStringList findFilesInPATH(const QStringList &filters);
QString findFolderContainingFile(const QString &fileName, const QStringList &folders);
bool copyDirectoryFiles(const QString &fromDir, const QString &toDir, bool coverFileIfExist);
QString removeExtension(const QString &filename);
QString replaceExtension(const QString &filename, const QString &newExtension);
bool isFileOpen(const QString &fileName);
#ifdef _WIN32
bool isFileWritable(const QString& filePath);
QString generateNewFileName(const QString &fileName);
#endif
void setFileForWritable(QString &fileName);
bool findExternalProgram(bool screen, const QString &appName, const QString &keySettingsName, const QString &folderName, QString &path);
std::optional<QPair<int, QString>> findFirstLineStartingWith(
    const QString &filePath,
    const QString &word,
    Qt::CaseSensitivity caseSensitivity,
    bool wholeWord,
    bool allowLeadingWhitespace);
}
#endif // FILEUTILS_H
