#ifndef DBQUERYBUILDER_H
#define DBQUERYBUILDER_H

#include <QStringList>

class DbQueryBuilder
{
public:
    enum boolOperator {AND_OR_OP, ONLY_OP, JUST_OP};

    DbQueryBuilder();
    void initialize();
    void setNames(const QString &newNames);
    void setSubfiles(const QStringList &newSubfiles);
    void setElements(const QString &newElements);
    void buildQuery();
    QString getQueryString() const;
    void setPrintEnabled(bool newPrintEnabled);    
    void setBOperator(boolOperator newBOperator);

private:
    QString queryString;
    QString names;
    QStringList subfiles;
    QString elString;
    bool printEnabled;
    boolOperator bOperator;

    QString queryNameString();
    QString querySubfilesString();
    QString queryElementString();
};

#endif // DBQUERYBUILDER_H
