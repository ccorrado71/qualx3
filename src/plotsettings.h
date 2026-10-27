#ifndef PLOTSETTINGS_H
#define PLOTSETTINGS_H

#include "colorwidget.h"
#include "customplotzoom.h"
#include "xpdutils.h"

using namespace xpdutils;

class PlotSettings
{
public:
    // Intensity scale of the y axis. For square root and logarithm the intensities are
    // transformed (see toPlotY/fromPlotY) and drawn on a linear axis, so ticks and grid
    // lines are equally spaced in sqrt(I) / log10(I) units.
    enum YScale { YLinear = 0, YSquareRoot, YLogarithmic };

    PlotSettings();
    QColor backColor, backColor1, backColor2;
    ColorWidget::ColorType backColorType;
    QColor layerColor, layerColor1, layerColor2;
    ColorWidget::ColorType layerColorType;

    QPen xAxisPen, yAxisPen, xAxis2Pen, yAxis2Pen;
    QPen xGridPen, yGridPen;

    double yOffset;
    bool hasOffset;
    bool legendVisible;
    YScale yScale;
    bool xTickLabels, yTickLabels;

    void read();
    void write();
    void applyTo(CustomPlotZoom *plot) const;
    void applyYScaleTo(CustomPlotZoom *plot) const;

    // Intensity (counts) <-> y plot coordinate for the given scale. The square root
    // keeps the sign (sign(y)*sqrt(|y|)), as difference curves can be negative. The
    // logarithm is log10(y) above 0.01; lower (also negative) values are squeezed
    // just below log10(0.01) = -2, invertibly, so switching back restores them.
    static double toPlotY(YScale scale, double counts);
    static double fromPlotY(YScale scale, double value);
    void restoreDefaults();

    xAbscissaType getAbscissa() const;
    void setAbscissa(const xAbscissaType &value);
    QString getYLabel() const;
    void applyToBackground(CustomPlotZoom *plot) const;
    void applyToLayer(CustomPlotZoom *plot) const;

private:
    QString prefix;

    QColor defBackColor, defBackColor1, defBackColor2;
    ColorWidget::ColorType defBackColorType;
    QColor defLayerColor, defLayerColor1, defLayerColor2;
    ColorWidget::ColorType defLayerColorType;

    QPen defXAxisPen, defYAxisPen, defXAxis2Pen, defYAxis2Pen;
    QPen defXGridPen, defYGridPen;

    double defYOffset;
    bool defHasOffset;
    bool defLegendVisible;

    xAbscissaType abscissa;    

    void fillBackgroundGradient(QLinearGradient &gradient) const;
    void fillBackgroundGradient(QRadialGradient &gradient) const;
    void fillLayerGradient(QLinearGradient &gradient) const;
    void fillLayerGradient(QRadialGradient &gradient) const;
    static void fillGradient(QLinearGradient &gradient, ColorWidget::ColorType type, const QColor &color1, const QColor &color2);
    static void fillGradient(QRadialGradient &gradient, ColorWidget::ColorType type, const QColor &color1, const QColor &color2);
};

#endif // PLOTSETTINGS_H
