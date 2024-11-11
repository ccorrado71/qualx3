#ifndef FILEUTILS_H
#define FILEUTILS_H

#include <QSettings>
#include <QAction>

namespace fileutils {

QString getDirDataFiles(QStringList &dataFiles, QString &err);
QString setCurrentDirFromFile(const QString &file);
void findFiles(QStringList &files, const QString &path, const QString &filter);
void findFiles(QStringList &files, const QString &filter);
bool copyDirectoryFiles(const QString &fromDir, const QString &toDir, bool coverFileIfExist);
//void updateRecentFileActions(QList<QAction*> &recentFileActionList, int maxFileNr, const QString &keyRecentFiles, bool strippedName = false);
//void setRecentFiles(const QString &fullFileName, QList<QAction*> &recentFileActionList, int maxFileNr, const QString &keyRecentFiles);
QString removeExtension(const QString &filename);
}
#endif // FILEUTILS_H
