#ifndef PLOTSETTINGS_H
#define PLOTSETTINGS_H

#include "colorwidget.h"
#include "customplotzoom.h"
#include "xpdutils.h"

using namespace xpdutils;

class PlotSettings
{
public:
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
    QCPAxis::ScaleType yScaleType;
    bool xTickLabels, yTickLabels;

    void read();
    void write();
    void applyTo(CustomPlotZoom *plot) const;
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
