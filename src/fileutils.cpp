#include <QSettings>
#include <QAction>
#include <QApplication>
#include <QMainWindow>
#include <QFileInfo>
#include <QDir>
#include <QDirIterator>
#include <QMessageBox>
#include <QRegularExpression>
#ifdef _WIN32
#include <windows.h>
#endif

namespace fileutils {

QString getDirDataFiles(QStringList &dataFiles, QString &err)
{
    //List of locations where find files
    QStringList pathList;
    QFileInfo path_info(QCoreApplication::applicationDirPath());
    pathList << path_info.absolutePath() + "/share/" + qApp->applicationName().toLower() + "/files/"
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

QStringList findFiles(const QString &path, const QStringList &filters) {
    QStringList files;
    QDirIterator it(path, filters, QDir::Files | QDir::NoSymLinks | QDir::NoDotAndDotDot, QDirIterator::Subdirectories);
    while (it.hasNext()) {
        files << it.next();
    }
    return files;
}

QStringList findFilesInPATH(const QStringList &filters)
{
#ifdef _WIN32
    QString path = qEnvironmentVariable("PATH");
    const char separator = ';';
#else
    QString path = QString::fromLocal8Bit(qgetenv("PATH"));
    const char separator = ':';
#endif
    QStringList files;
    if (!path.isEmpty()) {
        const QStringList pathDirs = path.split(separator, Qt::SkipEmptyParts);
        for (const QString &dir : pathDirs) {
            QDirIterator it(dir, filters, QDir::Files);
            while (it.hasNext())
                files << it.next();
        }
    }
    return files;
}

QString findFolderContainingFile(const QString &fileName, const QStringList &folders)
{
    for (const QString &folder : folders) {
        QDir dir(folder);
        if (dir.exists(fileName)) {
            return folder;
        }
    }
    return QString(); // Not found!
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

// Function to remove the extension of a file
QString removeExtension(const QString &filename) {
    int index = filename.lastIndexOf(".");
    if (index > 0) {
        return filename.left(index);
    }
    return filename;
}

// Function to replace the extension of a file
QString replaceExtension(const QString &filename, const QString &newExt) {
    int index = filename.lastIndexOf(".");
    if (index >= 0) {
        return filename.left(index) + "." + newExt;
    }
    return filename + "." + newExt;
}

// Function to check if a file is open by another program
bool isFileOpen(const QString &fileName) {
    QFile file(fileName);
    bool isOpen = file.open(QIODevice::ReadWrite);
    if (isOpen) {
        file.close();
    }
    return !isOpen;
}

#ifdef _WIN32
// Check if file is writable on Windows
bool isFileWritable(const QString& filePath) {
    DWORD fileAttributes = GetFileAttributesW(reinterpret_cast<LPCWSTR>(filePath.utf16()));

    if (fileAttributes == INVALID_FILE_ATTRIBUTES) {
        // If the file does not exist or there is an error, return false
        return false;
    }

    // Check if the file is read-only
    if (fileAttributes & FILE_ATTRIBUTE_READONLY) {
        return false;
    }

    // Try to open the file in write mode
    QFile file(filePath);
    bool writable = file.open(QIODevice::WriteOnly);
    file.close();

    return writable;
}

// Generate a new file name on Windows
QString generateNewFileName(const QString &fileName) {
    QFileInfo fileInfo(fileName);
    QString baseName = fileInfo.completeBaseName();
    QString extension = fileInfo.suffix();
    QDir dir = fileInfo.dir();

    int counter = 1;
    QString newFileName;
    do {
        newFileName = dir.filePath(QString("%1(%2).%3").arg(baseName).arg(counter).arg(extension));
        counter++;
    } while (QFileInfo::exists(newFileName) && !isFileWritable(newFileName));

    return newFileName;
}
#endif

void setFileForWritable(QString &fileName) {
#ifdef _WIN32
    if (QFileInfo::exists(fileName) && !fileutils::isFileWritable(fileName)) {
        fileName = fileutils::generateNewFileName(fileName);
    }
#endif
}

/**
 * @brief Finds an external program by searching in QSettings, system PATH, and a specific folder.
 * @param screen If true, display a QMessageBox in case of error; otherwise, log to stderr.
 * @param appName The name of the program/executable to search for (e.g., "python").
 * @param keySettingsName The key name in QSettings where the found path should be stored.
 * @param folderName An additional folder to search in; can be empty.
 * @param path [out] Will be set to the found path if the program is located.
 * @return true if found, false otherwise.
 */
bool findExternalProgram(bool screen, const QString &appName, const QString &keySettingsName, const QString &folderName, QString &path)
{
    QSettings settings;

    // 1. Search in QSettings
    if (settings.contains(keySettingsName)) {
        QString candidate = settings.value(keySettingsName).toString();
        if (QFile::exists(candidate)) {
            path = candidate;
            return true;
        } else {
            settings.remove(keySettingsName);
        }
    }

    // 2. Search in PATH
    const QStringList files = fileutils::findFilesInPATH({appName});
    if (!files.isEmpty()) {
        path = files.first();
        settings.setValue(keySettingsName, path);
        return true;
    }

    // 3. Search in the specified folder
    if (!folderName.isEmpty()) {
        QDir dir(folderName);
        QString filePath = dir.filePath(appName);
        if (QFile::exists(filePath)) {
            path = filePath;
            settings.setValue(keySettingsName, path);
            return true;
        }
    }

    // 4. Error reporting
    const QString msg = folderName.isEmpty()
        ? QObject::tr("The program %1 was not found.\nPlease make sure it is installed and available in the system PATH.").arg(appName)
        : QObject::tr("The program %1 was not found.\nPlease install it or copy it to the folder:\n%2").arg(appName, folderName);
    if (screen) {
        QMessageBox::warning(nullptr, QObject::tr("Program not found"), msg);
    } else {
        qCritical() << msg;
    }

    return false;
}

/**
 * @brief Find the first line in a text file that starts with a given word.
 *
 * The search is performed line-by-line (memory efficient). The function can operate in several modes:
 * - literal "starts with" comparison or whole-word matching using a regular expression,
 * - case sensitive or case insensitive matching,
 * - optional ignoring of leading whitespace before checking the start of the line.
 *
 * @param filePath Path to the text file to search.
 * @param word The word to search for. This is treated as a literal string (not a regex) and will be escaped when building the internal regex for whole-word matching.
 * @param caseSensitivity Use Qt::CaseSensitive or Qt::CaseInsensitive to control case handling. Default is Qt::CaseSensitive.
 * @param wholeWord If true, the function requires the word to be a whole word at the start of the (logical) line:
 *                  the word must be followed by a word boundary or end-of-line. Default is false.
 * @param allowLeadingWhitespace If true, leading whitespace on the line is ignored when checking whether the line starts with the word.
 *                              For example, if allowLeadingWhitespace==true the line "    hello world" is considered to start with "hello".
 *                              Default is false.
 *
 * @return std::optional<QPair<int, QString>> If a matching line is found, returns an optional containing a QPair:
 *         - first: the 1-based line number,
 *         - second: the full line text (including any leading/trailing whitespace).
 *         If the file cannot be opened or no matching line is found, returns std::nullopt.
 *
 * @note QTextStream in Qt6 uses UTF-8 by default; if you need to handle BOM explicitly, handle it in the caller or adjust the implementation.
 * @note When wholeWord==true the implementation constructs a regex anchored to the beginning of the line, optionally allowing leading whitespace
 *       (\\s*), then the escaped literal word, followed by (\\b|$) to ensure a word boundary or end-of-line.
 */

std::optional<QPair<int, QString>> findFirstLineStartingWith(
    const QString &filePath,
    const QString &word,
    Qt::CaseSensitivity caseSensitivity,
    bool wholeWord,
    bool allowLeadingWhitespace
)
{
    // Return empty if search word is empty
    if (word.isEmpty())
        return std::nullopt;

    QFile file(filePath);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return std::nullopt;

    QTextStream in(&file);
    // In Qt6 QTextStream uses UTF-8 by default.

    int lineNumber = 0;

    // Prepare regular expression if whole-word matching is requested
    QRegularExpression re;
    if (wholeWord) {
        // Build a pattern anchored at the start of the line.
        // If allowLeadingWhitespace is true, allow optional leading whitespace before the word.
        // Use QRegularExpression::escape to treat the word literally.
        QString pattern = QStringLiteral("^");
        if (allowLeadingWhitespace)
            pattern += QStringLiteral("\\s*");
        pattern += QRegularExpression::escape(word);
        // Ensure the word ends with a word boundary or end of line
        pattern += QStringLiteral("(\\b|$)");

        QRegularExpression::PatternOptions opts = QRegularExpression::UseUnicodePropertiesOption;
        if (caseSensitivity == Qt::CaseInsensitive)
            opts |= QRegularExpression::CaseInsensitiveOption;

        re = QRegularExpression(pattern, opts);
    }

    // Read file line by line (memory efficient)
    while (!in.atEnd()) {
        QString line = in.readLine();
        ++lineNumber;

        if (wholeWord) {
            QRegularExpressionMatch match = re.match(line);
            if (match.hasMatch()) {
                return QPair<int, QString>(lineNumber, line);
            }
        } else {
            // Non-regex simple "starts with" comparison
            if (allowLeadingWhitespace) {
                // Find first non-whitespace character
                int firstNonWs = 0;
                const int len = line.size();
                while (firstNonWs < len && line.at(firstNonWs).isSpace())
                    ++firstNonWs;

                // If the entire line is whitespace, skip it
                if (firstNonWs >= len)
                    continue;

                // Compare starting from the first non-whitespace character
                QString sub = line.mid(firstNonWs);
                if (sub.startsWith(word, caseSensitivity))
                    return QPair<int, QString>(lineNumber, line);
            } else {
                // Directly check if the line starts with the word
                if (line.startsWith(word, caseSensitivity))
                    return QPair<int, QString>(lineNumber, line);
            }
        }
    }

    // Not found
    return std::nullopt;
}

}


