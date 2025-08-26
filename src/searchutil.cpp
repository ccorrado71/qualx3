#include "searchutil.h"
#include <cmath>
#include <algorithm>

bool SearchUtil::findNearby(const QVector<double>& values, const QVector<double>& tolerances, double target) {
    int left = 0;
    int right = values.size() - 1;
    while (left <= right) {
        int mid = (left + right) / 2;
        double diff = std::fabs(target - values[mid]);
        if (diff <= tolerances[mid])
            return true;
        // values is sorted in descending order
        if (values[mid] < target)
            right = mid - 1;
        else
            left = mid + 1;
    }
    // Check neighbors for boundary cases (cast to int for std::min)
    for (int k = std::max(0, left-1); k <= std::min(int(values.size()-1), left+1); ++k) {
        if (std::fabs(target - values[k]) <= tolerances[k])
            return true;
    }
    return false;
}

bool SearchUtil::checkStrongValuesWithTolerance(const QVector<double>& values, const QVector<double>& tolerances, const QVector<double>& strongValues) {
    if (values.size() != tolerances.size() || strongValues.size() > values.size())
        return false;
    for (double s : strongValues) {
        if (!findNearby(values, tolerances, s))
            return false;
    }
    return true;
}

QVector<double> SearchUtil::extractNumbers(const QString &input, int n, int m)
{
    // n: Number of numbers in the string
    // m: How many numbers you want to extract

    QVector<double> result;
    // Split the string into a list using comma as separator
    QStringList numberStrings = input.split(',', Qt::SkipEmptyParts);

    // Check: if n does not match the actual number of elements, adjust n
    //if (n > numberStrings.size())
    //    n = numberStrings.size();

    // Extract up to m numbers, but not more than those available
    int count = qMin(m, n);

    for (int i = 0; i < count; ++i) {
        bool ok;
        double number = numberStrings[i].toDouble(&ok);
        if (ok)
            result.append(number);
    }
    return result;
}
