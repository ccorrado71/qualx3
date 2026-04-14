#ifndef DBMANAGER_H
#define DBMANAGER_H

#include <QString>

class QSqlDatabase;

class DbManager
{
public:
    DbManager();
    DbManager(const QString &path);
    ~DbManager();

    bool openDb(const QString &path);
    void closeDb();
    bool isOpen() const;
    int queryForCount(const QString &queryString);
    QSqlDatabase db() const;

private:
    // Store only the connection name, not a QSqlDatabase value member.
    // A QSqlDatabase value member in a class with static storage duration
    // would be constructed before QApplication exists, causing Qt warnings.
    QString m_connName;
    QString queryElementString(const QString &elString);
    void debugDatabaseInfo();
};

#endif // DBMANAGER_H
