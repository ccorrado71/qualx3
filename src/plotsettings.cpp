#include "plotsettings.h"

#include <QSettings>
#include <QDebug>

#if (QT_VERSION >= QT_VERSION_CHECK(5, 12, 0))
  #define OBJECT_MODE QGradient::ObjectMode
#else
#define OBJECT_MODE QGradient::ObjectBoundingMode
#endif

#define BACKGROUND_COLOR "backgroundcolor"
#define BACKGROUND_COLOR1 "backgroundcolor1"
#define BACKGROUND_COLOR2 "backgroundcolor2"
#define BACKGROUND_COLOR_TYPE "backgroundcolortype"
#define LAYER_COLOR "layercolor"
#define LAYER_COLOR1 "layercolor1"
#define LAYER_COLOR2 "layercolor2"
#define LAYER_COLOR_TYPE "layercolortype"
#define XAXIS_PEN "xaxispen"
#define YAXIS_PEN "yaxispen"
#define XAXIS2_PEN "xaxis2pen"
#define YAXIS2_PEN "yaxis2pen"
#define XGRID_PEN "xgridpen"
#define YGRID_PEN "ygridpen"
#define LEGEND_VISIBLE "legendvisible"
#define YOFFSET "yoffset"

PlotSettings::PlotSettings() :
    hasOffset(false),
    yScaleType(QCPAxis::stLinear),
    xTickLabels(true),
    yTickLabels(true),
    prefix("PlotSettings"),
    defBackColor(Qt::white),
    defBackColor1(Qt::white),
    defBackColor2(Qt::black),
    defBackColorType(ColorWidget::ColorType::TYPE_WHITE),
    defLayerColor(Qt::white),
    defLayerColor1(Qt::white),
    defLayerColor2(Qt::black),
    defLayerColorType(ColorWidget::ColorType::TYPE_WHITE),
    defXAxisPen(QPen(Qt::black)),
    defYAxisPen(QPen(Qt::black)),
    defXAxis2Pen(QPen(Qt::black)),
    defYAxis2Pen(QPen(Qt::black)),
    defXGridPen(QPen(Qt::gray)),
    defYGridPen(QPen(Qt::gray)),
    defYOffset(1000),
    defHasOffset(false),    
    defLegendVisible(true),
    abscissa(TTHETA)
{
    defXGridPen.setStyle(Qt::DotLine);
    defYGridPen.setStyle(Qt::DotLine);
}

void PlotSettings::restoreDefaults()
{
    backColor = defBackColor;
    backColor1 = defBackColor1;
    backColor2 = defBackColor2;
    backColorType = defBackColorType;
    layerColor = defLayerColor;
    layerColor1 = defLayerColor1;
    layerColor2 = defLayerColor2;
    layerColorType = defLayerColorType;
    xAxisPen = defXAxisPen;
    yAxisPen = defYAxisPen;
    xAxis2Pen = defXAxis2Pen;
    yAxis2Pen = defYAxis2Pen;
    xGridPen = defXGridPen;
    yGridPen = defYGridPen;
    yOffset = defYOffset;
    hasOffset = defHasOffset;
    legendVisible = defLegendVisible;
    abscissa = TTHETA;
    yScaleType = QCPAxis::stLinear;
    xTickLabels = true;
    yTickLabels = true;
}

xpdutils::xAbscissaType PlotSettings::getAbscissa() const
{
    return abscissa;
}

void PlotSettings::setAbscissa(const xAbscissaType &value)
{
    abscissa = value;
}

QString PlotSettings::getYLabel() const
{
    return (yScaleType == QCPAxis::stLinear ? "Counts" : "LOG(Counts)");
}

void PlotSettings::read()
{
    QSettings settings;
    settings.beginGroup(prefix);
    backColor = settings.value(BACKGROUND_COLOR, defBackColor).value<QColor>();
    backColor1 = settings.value(BACKGROUND_COLOR1, defBackColor1).value<QColor>();
    backColor2 = settings.value(BACKGROUND_COLOR2, defBackColor2).value<QColor>();
    backColorType = ColorWidget::ColorType(settings.value(BACKGROUND_COLOR_TYPE, defBackColorType).toInt());
    layerColor = settings.value(LAYER_COLOR, defLayerColor).value<QColor>();
    layerColor1 = settings.value(LAYER_COLOR1, defLayerColor1).value<QColor>();
    layerColor2 = settings.value(LAYER_COLOR2, defLayerColor2).value<QColor>();
    layerColorType = ColorWidget::ColorType(settings.value(LAYER_COLOR_TYPE, defLayerColorType).toInt());
    xAxisPen = settings.value(XAXIS_PEN, defXAxisPen).value<QPen>();
    yAxisPen = settings.value(YAXIS_PEN, defYAxisPen).value<QPen>();
    xAxis2Pen = settings.value(XAXIS2_PEN, defXAxis2Pen).value<QPen>();
    yAxis2Pen = settings.value(YAXIS2_PEN, defYAxis2Pen).value<QPen>();
    xGridPen = settings.value(XGRID_PEN, defXGridPen).value<QPen>();
    yGridPen = settings.value(YGRID_PEN, defYGridPen).value<QPen>();
    legendVisible = settings.value(LEGEND_VISIBLE, defLegendVisible).value<bool>();
    yOffset = settings.value(YOFFSET, defYOffset).toDouble();
    settings.endGroup();
}

void PlotSettings::write()
{
    QSettings settings;    
    settings.beginGroup(prefix);
    settings.setValue(BACKGROUND_COLOR, backColor);
    settings.setValue(BACKGROUND_COLOR1, backColor1);
    settings.setValue(BACKGROUND_COLOR2, backColor2);
    settings.setValue(BACKGROUND_COLOR_TYPE, backColorType);
    settings.setValue(LAYER_COLOR, layerColor);
    settings.setValue(LAYER_COLOR1, layerColor1);
    settings.setValue(LAYER_COLOR2, layerColor2);
    settings.setValue(LAYER_COLOR_TYPE, layerColorType);
    settings.setValue(XAXIS_PEN, xAxisPen);
    settings.setValue(YAXIS_PEN, yAxisPen);
    settings.setValue(XAXIS2_PEN, xAxis2Pen);
    settings.setValue(YAXIS2_PEN, yAxis2Pen);
    settings.setValue(XGRID_PEN, xGridPen);
    settings.setValue(YGRID_PEN, yGridPen);
    settings.setValue(LEGEND_VISIBLE, legendVisible);
    settings.setValue(YOFFSET, yOffset);
    settings.endGroup();
}

void PlotSettings::applyToBackground(CustomPlotZoom *plot) const
{
    switch(backColorType) {
    case ColorWidget::TYPE_FLAT:
    case ColorWidget::TYPE_BLACK:
    case ColorWidget::TYPE_WHITE:
        plot->setBackground(backColor);
        break;
    case ColorWidget::TYPE_DGRAD:
    case ColorWidget::TYPE_DGRAD2:
    case ColorWidget::TYPE_HGRAD:
    case ColorWidget::TYPE_VGRAD: {
        QLinearGradient linearGradient;
        fillBackgroundGradient(linearGradient);
        plot->setBackground(linearGradient);
        break;
    }
    case ColorWidget::TYPE_RGRAD:
        QRadialGradient radialGradient;
        fillBackgroundGradient(radialGradient);
        plot->setBackground(radialGradient);
        break;
    }
}

void PlotSettings::applyToLayer(CustomPlotZoom *plot) const
{
    switch(layerColorType) {
    case ColorWidget::TYPE_FLAT:
    case ColorWidget::TYPE_BLACK:
    case ColorWidget::TYPE_WHITE:
        plot->axisRect()->setBackground(layerColor);
        break;
    case ColorWidget::TYPE_DGRAD:
    case ColorWidget::TYPE_DGRAD2:
    case ColorWidget::TYPE_HGRAD:
    case ColorWidget::TYPE_VGRAD: {
        QLinearGradient linearGradient;
        fillLayerGradient(linearGradient);
        plot->axisRect()->setBackground(linearGradient);
        break;
    }
    case ColorWidget::TYPE_RGRAD:
        QRadialGradient radialGradient;
        fillLayerGradient(radialGradient);
        plot->axisRect()->setBackground(radialGradient);
        break;
    }
}

void PlotSettings::applyTo(CustomPlotZoom *plot) const
{
    applyToBackground(plot);

    applyToLayer(plot);

    plot->xAxis->setBasePen(xAxisPen);
    plot->xAxis->setTickPen(xAxisPen);
    plot->xAxis->setSubTickPen(xAxisPen);
    plot->yAxis->setBasePen(yAxisPen);
    plot->yAxis->setTickPen(yAxisPen);
    plot->yAxis->setSubTickPen(yAxisPen);
    plot->xAxis2->setBasePen(xAxis2Pen);
    plot->xAxis2->setTickPen(xAxis2Pen);
    plot->xAxis2->setSubTickPen(xAxis2Pen);
    plot->yAxis2->setBasePen(yAxis2Pen);
    plot->yAxis2->setTickPen(yAxis2Pen);
    plot->yAxis2->setSubTickPen(yAxis2Pen);

    plot->xAxis->grid()->setPen(xGridPen);
    plot->yAxis->grid()->setPen(yGridPen);

    plot->legend->setVisible(legendVisible);

    plot->xAxis->setLabel(abscissaString(abscissa));
    plot->yAxis->setScaleType(yScaleType);
    plot->yAxis->setLabel(getYLabel());

    plot->xAxis->setTickLabels(xTickLabels);
    plot->yAxis->setTickLabels(yTickLabels);
}

void PlotSettings::fillBackgroundGradient(QLinearGradient &gradient) const
{
    fillGradient(gradient, backColorType, backColor1, backColor2);
}

void PlotSettings::fillBackgroundGradient(QRadialGradient &gradient) const
{
    fillGradient(gradient, backColorType, backColor1, backColor2);
}

void PlotSettings::fillLayerGradient(QLinearGradient &gradient) const
{
    fillGradient(gradient, layerColorType, layerColor1, layerColor2);
}

void PlotSettings::fillLayerGradient(QRadialGradient &gradient) const
{
    fillGradient(gradient, layerColorType, layerColor1, layerColor2);
}

void PlotSettings::fillGradient(QLinearGradient &gradient, ColorWidget::ColorType type, const QColor &color1, const QColor &color2)
{
    if (type == ColorWidget::TYPE_DGRAD) {
        gradient.setCoordinateMode(OBJECT_MODE);
        gradient.setStart(0, 0);
        gradient.setFinalStop(1,1);
        gradient.setColorAt(0,color1);
        gradient.setColorAt(1,color2);
    } else if (type == ColorWidget::TYPE_DGRAD2) {
        gradient.setCoordinateMode(OBJECT_MODE);
        gradient.setStart(1, 0);
        gradient.setFinalStop(0,1);
        gradient.setColorAt(0,color1);
        gradient.setColorAt(1,color2);
    } else if (type == ColorWidget::TYPE_HGRAD) {
        gradient.setCoordinateMode(OBJECT_MODE);
        gradient.setStart(0, 0.5);
        gradient.setFinalStop(1,0.5);
        gradient.setColorAt(0,color1);
        gradient.setColorAt(1,color2);
    } else if (type == ColorWidget::TYPE_VGRAD) {
        gradient.setCoordinateMode(OBJECT_MODE);
        gradient.setStart(0.5, 0);
        gradient.setFinalStop(0.5,1);
        gradient.setColorAt(0,color1);
        gradient.setColorAt(1,color2);
    }
}

void PlotSettings::fillGradient(QRadialGradient &gradient, ColorWidget::ColorType type, const QColor &color1, const QColor &color2)
{
    if (type == ColorWidget::TYPE_RGRAD) {
        gradient.setCoordinateMode(OBJECT_MODE);
        gradient.setCenter(0.5,0.5);
        gradient.setRadius(1);
        gradient.setColorAt(0,color1);
        gradient.setColorAt(1,color2);
    }
}
