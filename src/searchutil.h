#pragma once
#include <QVector>

class SearchUtil
{
public:
    // Returns true if 'target' is within tolerance of any element in 'values' (descending order)
    static bool findNearby(const QVector<double>& values, const QVector<double>& tolerances, double target);

    // Returns true if each element in 'strongValues' has at least one nearby value in 'values' within tolerance
    static bool checkStrongValuesWithTolerance(const QVector<double>& values, const QVector<double>& tolerances, const QVector<double>& strongValues);

    // Extracts numbers from a comma-separated string
    static QVector<double> extractNumbers(const QString& input, int n, int m);
};
