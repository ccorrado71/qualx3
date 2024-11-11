#include <QSettings>
#include <QAction>
#include <QApplication>
#include <QMainWindow>
#include <QFileInfo>
#include <QDir>
#include <QDirIterator>

namespace fileutils {

QString getDirDataFiles(QStringList &dataFiles, QString &err)
{
    //List of locations where find files
    QStringList pathList;
    QFileInfo path_info(QCoreApplication::applicationDirPath());
    pathList << path_info.absolutePath() + "/share/" + qApp->applicationName().toLower() + "/"
             << QCoreApplication::applicationDirPath() + "/" + qApp->applicationName().toLower() + "dir/";

    err = "";
    bool found = false;
    foreach (QString path, pathList) {
        QDir dir(path);
        if (dir.exists()) {
            err = "";
            found = true;
            foreach (QString file, dataFiles) {
                if (!dir.exists(file)) {
                    if (!err.isEmpty()) err+="\n";
                    err+="Program file not found: " + path + file;
                    found = false;
                }
            }
            if (found)
                return dir.absolutePath()+QDir::separator();
        }
    }

    if (err.isEmpty()) {
        err = "Folder not found: " + pathList[0];
    }

    return QString();
}

QString setCurrentDirFromFile(const QString &file) {
    //Set current directory from file name

    if (file.isEmpty()) return QString();

    QFileInfo fi(file);
    QString path = fi.absolutePath();
    QDir::setCurrent(path);
    return path;
}

void findFiles(QStringList &files, const QString &path, const QString &filter) {
    QStringList filters;
    filters << filter;
    QDirIterator it(path, filters, QDir::AllEntries | QDir::NoSymLinks | QDir::NoDotAndDotDot, QDirIterator::Subdirectories);
    while (it.hasNext())
        files << it.next();
}

void findFiles(QStringList &files, const QString &filter)
{
#ifdef _WIN32
    QString path = qEnvironmentVariable("PATH");
    const char separator = ';';
#else
    QByteArray pathByte = qgetenv("PATH");
    QString path(pathByte);
    const char separator = ':';
#endif
    if (!path.isEmpty()) {
        QStringList pathDirs = path.split(separator);
        QStringList filters;
        filters << filter;
        for (int i = 0; i < pathDirs.size(); i++) {
            QDirIterator it(pathDirs.at(i), filters, QDir::Files);
            while (it.hasNext())
                files << it.next();
        }
    }
}

bool copyDirectoryFiles(const QString &fromDir, const QString &toDir, bool coverFileIfExist)
{
    QDir sourceDir(fromDir);
    QDir targetDir(toDir);
    if(!targetDir.exists()){    /* if directory don't exists, build it */
        if(!targetDir.mkpath(targetDir.absolutePath()))
            return false;
    }

    QFileInfoList fileInfoList = sourceDir.entryInfoList();
    foreach(QFileInfo fileInfo, fileInfoList){
        if(fileInfo.fileName() == "." || fileInfo.fileName() == "..")
            continue;

        if(fileInfo.isDir()){    /* if it is directory, copy recursively*/
            if(!copyDirectoryFiles(fileInfo.filePath(),
                targetDir.filePath(fileInfo.fileName()),
                coverFileIfExist))
                return false;
        }
        else{            /* if coverFileIfExist == true, remove old file first */
            if(coverFileIfExist && targetDir.exists(fileInfo.fileName())){
                targetDir.remove(fileInfo.fileName());
            }

            // files copy
            if(!QFile::copy(fileInfo.filePath(),
                targetDir.filePath(fileInfo.fileName()))){
                    return false;
            }
        }
    }
    return true;
}

QString removeExtension(const QString &filename) {
    int index = filename.lastIndexOf(".");
    if (index > 0) {
        return filename.left(index);
    }
    return filename;
}

}
