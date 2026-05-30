#include "plotstyledialog.h"
#include "ui_plotstyledialog.h"
#include "xpdutils.h"

//#include <QDebug>

PlotStyleDialog::PlotStyleDialog(QWidget *parent) :
    QDialog(parent),    
    ui(new Ui::PlotStyleDialog),
    mYOffset(0)
{
    ui->setupUi(this);

    ui->tthetaRadioButton->setText('2'+QChar(0x03B8)); //2theta    
    ui->oneOverD2RadioButton->setText(QString("q=1/d")+QChar(0x00B2)); //1/d**2

    //Observed    
    connect(ui->obsCheckBox, &QCheckBox::clicked, this, [=](bool state){
        obsVisible[ui->observedComboBox->currentIndex()] = state;
        enableApplyButton();
    });
    connect(ui->observedLineWidget,&LineStyleWidget::lineColorChanged,this,[=](QColor c){
        obsPen[ui->observedComboBox->currentIndex()].setColor(c);        
        enableApplyButton();
    });
    connect(ui->observedLineWidget,&LineStyleWidget::lineWidthChanged,this,[=](int i){
        obsPen[ui->observedComboBox->currentIndex()].setWidth(i);        
        enableApplyButton();
    });
    connect(ui->observedLineWidget,&LineStyleWidget::lineStyleChanged,this,[=](Qt::PenStyle style) {
        obsPen[ui->observedComboBox->currentIndex()].setStyle(style);        
        enableApplyButton();
    });
    connect(ui->observedMarkerWidget,&MarkerStyleWidget::markerShapeChanged, this, [=](QCPScatterStyle::ScatterShape shape) {
        obsScatter[ui->observedComboBox->currentIndex()].setShape(shape);        
        enableApplyButton();
    });
    connect(ui->observedMarkerWidget, &MarkerStyleWidget::markerColorChanged, this, [=](QColor c) {
        QPen pen = obsScatter[ui->observedComboBox->currentIndex()].pen();
        pen.setColor(c);
        obsScatter[ui->observedComboBox->currentIndex()].setPen(pen);        
        enableApplyButton();
    });
    connect(ui->observedMarkerWidget, &MarkerStyleWidget::markerSizeChanged, this, [=](int size) {
        obsScatter[ui->observedComboBox->currentIndex()].setSize(size);        
        enableApplyButton();
    });

    //Background
    connect(ui->backCheckBox, &QCheckBox::clicked, this, &PlotStyleDialog::enableApplyButton);
    connectLineWidget(ui->backgroundLineWidget);
    connect(ui->backpCheckBox, &QCheckBox::clicked, this, &PlotStyleDialog::enableApplyButton);
    connect(ui->backgroundPointWidget, &MarkerStyleWidget::markerShapeChanged, this, &PlotStyleDialog::enableApplyButton);
    connect(ui->backgroundPointWidget, &MarkerStyleWidget::markerColorChanged, this, &PlotStyleDialog::enableApplyButton);
    connect(ui->backgroundPointWidget, &MarkerStyleWidget::markerSizeChanged, this, &PlotStyleDialog::enableApplyButton);

    //Calculated
    connect(ui->calcCheckBox, &QCheckBox::clicked, this, &PlotStyleDialog::enableApplyButton);
    connectLineWidget(ui->calculatedLineWidget);

    //Difference
    connect(ui->diffCheckBox, &QCheckBox::clicked, this, &PlotStyleDialog::enableApplyButton);
    connectLineWidget(ui->differenceLineWidget);

    //Cumulative Difference
    connect(ui->cDiffCheckBox, &QCheckBox::clicked, this, &PlotStyleDialog::enableApplyButton);
    connectLineWidget(ui->cumulativeLineWidget);

    //Peaks
    connect(ui->peaksCheckBox, &QCheckBox::clicked, this, &PlotStyleDialog::enableApplyButton);
    connectLineWidget(ui->peaksLineWidget);

    //Reflections
    connect(ui->reflCheckBox, &QCheckBox::clicked, this, [=](bool state){
        reflVisible[ui->reflComboBox->currentIndex()] = state;        
        enableApplyButton();
    });
    connect(ui->reflectionsLineWidget, &LineStyleWidget::lineColorChanged, this, [=](QColor c){
        reflPen[ui->reflComboBox->currentIndex()].setColor(c);        
        enableApplyButton();
    });
    connect(ui->reflectionsLineWidget, &LineStyleWidget::lineWidthChanged, this, [=](int i) {
        reflPen[ui->reflComboBox->currentIndex()].setWidth(i);        
        enableApplyButton();
    });
    connect(ui->reflectionsLineWidget, &LineStyleWidget::lineStyleChanged, this, [=](Qt::PenStyle style) {
        reflPen[ui->reflComboBox->currentIndex()].setStyle(style);        
        enableApplyButton();
    });

    //Axis
    connect(ui->tthetaRadioButton, &QRadioButton::clicked, this, [=](){
       if (mAbscissa != xpdutils::TTHETA) {
           enableApplyButton();
           mAbscissa = xpdutils::TTHETA;
       }
    });
    connect(ui->dRadioButton, &QRadioButton::clicked, this, [=](){
       if (mAbscissa != xpdutils::DVALUE) {
           enableApplyButton();
           mAbscissa = xpdutils::DVALUE;
       }
    });
    connect(ui->oneOverDRadioButton, &QRadioButton::clicked, this, [=](){
       if (mAbscissa != xpdutils::ONE_OVER_DVALUE) {
           enableApplyButton();
           mAbscissa = xpdutils::ONE_OVER_DVALUE;
       }
    });
    connect(ui->oneOverD2RadioButton, &QRadioButton::clicked, this, [=](){
       if (mAbscissa != xpdutils::ONE_OVER_DVALUE2) {
           enableApplyButton();
           mAbscissa = xpdutils::ONE_OVER_DVALUE2;
       }
    });
    connect(ui->linearRadioButton, &QRadioButton::clicked, this, [=](){
       if (mYScaleType != QCPAxis::stLinear) {
           enableApplyButton();
           mYScaleType = QCPAxis::stLinear;
       }
    });
    connect(ui->logarithmicRadioButton, &QRadioButton::clicked, this, [=](){
       if (mYScaleType != QCPAxis::stLogarithmic) {
           enableApplyButton();
           mYScaleType = QCPAxis::stLogarithmic;
       }
    });
    connect(ui->axisComboBox, QOverload<int>::of(&QComboBox::currentIndexChanged), ui->axisStackedWidget, &QStackedWidget::setCurrentIndex);
    connectLineWidget(ui->xAxisLSWidget);
    connectLineWidget(ui->yAxisLSWidget);
    connectLineWidget(ui->xAxis2LSWidget);
    connectLineWidget(ui->yAxis2LSWidget);
    ui->yOffsetLineEdit->setValidator(new QDoubleValidator);    
    connect(ui->yOffsetCheckBox, &QCheckBox::clicked, this, [=](bool state){
        ui->yOffsetLineEdit->setEnabled(state);
        if (state) ui->yOffsetLineEdit->setFocus();
        enableApplyButton();
    });
    connect(ui->yOffsetLineEdit, &QLineEdit::textEdited, this, &PlotStyleDialog::enableApplyButton);
    connect(ui->xAxisTickLabelsCheckBox, &QCheckBox::clicked, this, &PlotStyleDialog::enableApplyButton);
    connect(ui->yAxisTickLabelsCheckBox, &QCheckBox::clicked, this, &PlotStyleDialog::enableApplyButton);

    //Grids
    ui->xGridLSWidget->setBoxName("Grid");
    ui->yGridLSWidget->setBoxName("Grid");
    connectLineWidget(ui->xGridLSWidget);
    connectLineWidget(ui->yGridLSWidget);

    //Background and layout colors
    connect(ui->backgroundWidget, &ColorWidget::colorWidgetChanged, this, &PlotStyleDialog::enableApplyButton);
    connect(ui->layerWidget, &ColorWidget::colorWidgetChanged, this, &PlotStyleDialog::enableApplyButton);

    //Legend
    connect(ui->legendCheckBox, &QCheckBox::clicked, this, &PlotStyleDialog::enableApplyButton);
}

PlotStyleDialog::~PlotStyleDialog()
{
    delete ui;
}

void PlotStyleDialog::setOptions(const XpdViewWidget *plot)
{
    changeApplied = false;

    //Observed
    obsPen.clear();
    obsScatter.clear();
    obsPen.reserve(plot->obs.count());
    obsScatter.reserve(plot->obs.count());
    obsVisible.reserve(plot->obs.count());
    for (int i = 0; i< plot->obs.count(); i++) {
        obsPen.push_back(plot->obs.at(i).getPen());
        obsScatter.push_back(plot->obs.at(i).getScatter());
        obsVisible.push_back(plot->obs.at(i).isVisible());
    }
    obsVisible0 = obsVisible;
    obsPen0 = obsPen;
    obsScatter0 = obsScatter;

    ui->observedComboBox->clear();
    for (int i = 0; i < plot->obs.count(); i++) {
        ui->observedComboBox->addItem(plot->obs[i].getName());
    }

    //Background    
    backPen0 = plot->back.getPen();
    backVisible0 = plot->back.isVisible();

    //Background Points    
    backScatter0 = plot->bpoints.getScatter();
    backpVisible0 = plot->bpoints.isVisible();

    //Calculated    
    calcPen0 = plot->calc.getPen();
    calcVisible0 = plot->calc.isVisible();

    //Difference    
    diffPen0 = plot->diff.getPen();
    diffVisible0 = plot->diff.isVisible();

    //Cumulative difference    
    cDiffPen0 = plot->cdiff.getPen();
    cDiffVisible0 = plot->cdiff.isVisible();

    //Peaks    
    peaksPen0 = plot->peaks.getPen();
    peaksVisible0 = plot->peaks.isVisible();

    //Reflections
    reflPen.clear();
    if (plot->refl.count() == 0)  {
        ui->reflComboBox->setDisabled(true);
        ui->reflCheckBox->setDisabled(true);
        ui->reflectionsLineWidget->setDisabled(true);
    } else {
        reflPen.reserve(plot->refl.count());
        reflVisible.resize(plot->refl.count());
        for (int i = 0; i< plot->refl.count(); i++) {
            reflPen.push_back(plot->refl[i].getPen());
            reflVisible[i] = plot->refl[i].isVisible();
        }
        reflVisible0 = reflVisible;

        ui->reflComboBox->clear();
        for (int i = 0; i < plot->refl.count(); i++) {
            ui->reflComboBox->addItem(plot->refl[i].getName());
        }
    }
    reflPen0 = reflPen;

    settings0 = plot->pSettings();

    setWidgets(true, backPen0, backScatter0, calcPen0, diffPen0, cDiffPen0, peaksPen0, plot->pSettings(),
               backVisible0, backpVisible0, calcVisible0, diffVisible0, cDiffVisible0, peaksVisible0);

    ui->buttonBox->button(QDialogButtonBox::Apply)->setEnabled(false);
}

void PlotStyleDialog::setWidgets(bool init, const QPen &backPen, const QCPScatterStyle &backScatter, const QPen &calcPen, const QPen &diffPen,
                                 const QPen &cDiffPen, const QPen &peaksPen, const PlotSettings &psettings,
                                 bool backVisible, bool backpVisible, bool calcVisible, bool diffVisible, bool cDiffVisible, bool peaksVisible)
{
    //Background
    ui->backgroundLineWidget->setLineStyle(backPen);
    ui->backCheckBox->setChecked(backVisible);

    //Background Points
    ui->backgroundPointWidget->setMarkerStyle(backScatter);
    ui->backpCheckBox->setChecked(backpVisible);

    //Calculated
    ui->calculatedLineWidget->setLineStyle(calcPen);
    ui->calcCheckBox->setChecked(calcVisible);

    //Difference
    ui->differenceLineWidget->setLineStyle(diffPen);
    ui->diffCheckBox->setChecked(diffVisible);

    //Cumulative difference
    ui->cumulativeLineWidget->setLineStyle(cDiffPen);
    ui->cDiffCheckBox->setChecked(cDiffVisible);

    //Peaks
    ui->peaksLineWidget->setLineStyle(peaksPen);
    ui->peaksCheckBox->setChecked(peaksVisible);

    //Layout colors
    ui->backgroundWidget->setColorWidget(psettings.backColorType, psettings.backColor, psettings.backColor1, psettings.backColor2);
    ui->layerWidget->setColorWidget(psettings.layerColorType, psettings.layerColor, psettings.layerColor1, psettings.layerColor2);

    //Axis
    mAbscissa = psettings.getAbscissa();
    if (mAbscissa == xpdutils::DVALUE) {
        ui->dRadioButton->setChecked(true);
    } else if (mAbscissa == xpdutils::TTHETA) {
        ui->tthetaRadioButton->setChecked(true);
    } else if (mAbscissa == xpdutils::ONE_OVER_DVALUE) {
        ui->oneOverDRadioButton->setChecked(true);
    } else if (mAbscissa == xpdutils::ONE_OVER_DVALUE2) {
        ui->oneOverD2RadioButton->setChecked(true);
    }
    mYScaleType = psettings.yScaleType;
    if (mYScaleType == QCPAxis::stLinear) {
        ui->linearRadioButton->setChecked(true);
    } else if (mYScaleType == QCPAxis::stLinear) {
        ui->logarithmicRadioButton->setChecked(true);
    }
    ui->xAxisLSWidget->setLineStyle(psettings.xAxisPen);
    ui->yAxisLSWidget->setLineStyle(psettings.yAxisPen);
    ui->xAxis2LSWidget->setLineStyle(psettings.xAxis2Pen);
    ui->yAxis2LSWidget->setLineStyle(psettings.yAxis2Pen);
    ui->yOffsetLineEdit->setText(QString::number(psettings.yOffset));
    ui->yOffsetLineEdit->setEnabled(psettings.hasOffset);
    ui->yOffsetCheckBox->setChecked(psettings.hasOffset);
    ui->xAxisTickLabelsCheckBox->setChecked(psettings.xTickLabels);
    ui->yAxisTickLabelsCheckBox->setChecked(psettings.yTickLabels);

    //Grid
    ui->xGridLSWidget->setLineStyle(psettings.xGridPen);
    ui->yGridLSWidget->setLineStyle(psettings.yGridPen);

    //Legend
    ui->legendCheckBox->setChecked(psettings.legendVisible);

    //Observed and reflections
    if (init) {
        ui->observedComboBox->setCurrentIndex(0);
        if (reflPen.count() > 0) ui->reflComboBox->setCurrentIndex(0);
    } else {
        setObserved(ui->observedComboBox->currentIndex());
        if (reflPen.count() > 0) setReflections(ui->reflComboBox->currentIndex());
    }
}

void PlotStyleDialog::apply(XpdViewWidget *plot, bool update)
{
    //update = true:  1) widget to psettings, obs, ... 2) replot according to psettings
    //update = false: only replot according to psettings, obs, ... (for Cancel)

    bool plotChanged = false;    // only style, colors have been changed
    bool redrawRequired = false; // visibility has been changed

    for (int i = 0; i < plot->obs.count(); i++) {
        QPen pen = plot->obs[i].getPen();
        if (pen != obsPen[i]) {
            if (update) plot->obs[i].setPen(obsPen[i]);
            int index = plot->obs[i].getGraphIndex();
            plot->graph(index)->setPen(plot->obs[i].getPen());
            plotChanged = true;
        }

        QCPScatterStyle scatter = plot->obs[i].getScatter();
        if (scatter.shape() != obsScatter[i].shape() || scatter.size() != obsScatter[i].size() || scatter.pen() != obsScatter[i].pen()) {
            if (update) plot->obs[i].setScatter(obsScatter[i]);
            int index = plot->obs[i].getGraphIndex();
            plot->graph(index)->setScatterStyle(plot->obs[i].getScatter());
            plotChanged = true;
        }

        if (obsVisible.at(i) != plot->obs.at(i).isVisible()) {
            if (update) plot->obs[i].setVisible(obsVisible.at(i));
            int index = plot->obs[i].getGraphIndex();
            plot->graph(index)->setVisible(plot->obs.at(i).isVisible());
            if (plot->obs.at(i).isVisible())
                plot->graph(index)->addToLegend();
            else
                plot->graph(index)->removeFromLegend();
            redrawRequired = true;
        }
    }

    if (ui->backCheckBox->isChecked() != plot->back.isVisible()) {
        if (update) plot->back.setVisible(ui->backCheckBox->isChecked());
        int indexB = plot->back.getGraphIndex();
        if (indexB != -1) {
            plot->graph(indexB)->setVisible(plot->back.isVisible());
            if (plot->back.isVisible())
                plot->graph(indexB)->addToLegend();
            else
                plot->graph(indexB)->removeFromLegend();
            redrawRequired = true;
        }
    }

    if (ui->backgroundLineWidget->isDifferent(plot->back.getPen())) {
        if (update) {
            plot->back.setLineStyle(ui->backgroundLineWidget->getStyle());
            plot->back.setLineColor(ui->backgroundLineWidget->getColor());
            plot->back.setLineWidth(ui->backgroundLineWidget->getWidth());
        }
        int indexB = plot->back.getGraphIndex();
        if (indexB != -1) {
            plot->graph(indexB)->setPen(plot->back.getPen());
            plotChanged = true;
        }
    }    

    if (ui->backpCheckBox->isChecked() != plot->bpoints.isVisible()) {
        if (update) plot->bpoints.setVisible(ui->backpCheckBox->isChecked());
        int indexBP = plot->bpoints.getGraphIndex();
        if (indexBP != -1) {
            plot->graph(indexBP)->setVisible(plot->bpoints.isVisible());
            if (plot->bpoints.isVisible())
                plot->graph(indexBP)->addToLegend();
            else
                plot->graph(indexBP)->removeFromLegend();
            redrawRequired = true;
        }
    }

    QCPScatterStyle scatter = plot->bpoints.getScatter();
    if (ui->backgroundPointWidget->isDifferent(scatter)) {
        if (update) {
            scatter.setShape(ui->backgroundPointWidget->getShape());
            QPen pen = scatter.pen();
            pen.setColor(ui->backgroundPointWidget->getColor());
            scatter.setPen(pen);
            scatter.setSize(ui->backgroundPointWidget->getSize());
            plot->bpoints.setScatter(scatter);
        }
        int indexBP = plot->bpoints.getGraphIndex();
        if (indexBP != -1) {
            plot->graph(indexBP)->setScatterStyle(plot->bpoints.getScatter());
            plotChanged = true;
        }
    }    

    if (ui->calcCheckBox->isChecked() != plot->calc.isVisible()) {
        if (update) plot->calc.setVisible(ui->calcCheckBox->isChecked());
        int indexC = plot->calc.getGraphIndex();
        if (indexC != -1) {
            plot->graph(indexC)->setVisible(plot->calc.isVisible());
            if (plot->calc.isVisible())
                plot->graph(indexC)->addToLegend();
            else
                plot->graph(indexC)->removeFromLegend();
            redrawRequired = true;
        }
    }

    if (ui->calculatedLineWidget->isDifferent(plot->calc.getPen())) {
        if (update) {
            plot->calc.setLineStyle(ui->calculatedLineWidget->getStyle());
            plot->calc.setLineColor(ui->calculatedLineWidget->getColor());
            plot->calc.setLineWidth(ui->calculatedLineWidget->getWidth());
        }
        int indexC = plot->calc.getGraphIndex();
        if (indexC != -1) {
            plot->graph(indexC)->setPen(plot->calc.getPen());
            plotChanged = true;
        }
    }

    if (ui->diffCheckBox->isChecked() != plot->diff.isVisible()) {
        if (update) plot->diff.setVisible(ui->diffCheckBox->isChecked());
        int indexD = plot->diff.getGraphIndex();
        if (indexD != -1) {
            plot->graph(indexD)->setVisible(plot->diff.isVisible());
            if (plot->diff.isVisible())
                plot->graph(indexD)->addToLegend();
            else
                plot->graph(indexD)->removeFromLegend();
            redrawRequired = true;
        }
    }

    if (ui->differenceLineWidget->isDifferent(plot->diff.getPen())) {
        if (update) {
            plot->diff.setLineStyle(ui->differenceLineWidget->getStyle());
            plot->diff.setLineColor(ui->differenceLineWidget->getColor());
            plot->diff.setLineWidth(ui->differenceLineWidget->getWidth());
        }
        int indexD = plot->diff.getGraphIndex();
        if (indexD != -1) {
            plot->graph(indexD)->setPen(plot->diff.getPen());
            plotChanged = true;
        }
    }

    if (ui->cDiffCheckBox->isChecked() != plot->cdiff.isVisible()) {
        if (update) plot->cdiff.setVisible(ui->cDiffCheckBox->isChecked());
        int indexCD = plot->cdiff.getGraphIndex();
        if (indexCD != -1) {
            plot->graph(indexCD)->setVisible(plot->cdiff.isVisible());
            if (plot->diff.isVisible())
                plot->graph(indexCD)->addToLegend();
            else
                plot->graph(indexCD)->removeFromLegend();
            redrawRequired = true;
        }
    }

    if (ui->cumulativeLineWidget->isDifferent(plot->cdiff.getPen())) {
        if (update) {
            plot->cdiff.setLineStyle(ui->cumulativeLineWidget->getStyle());
            plot->cdiff.setLineColor(ui->cumulativeLineWidget->getColor());
            plot->cdiff.setLineWidth(ui->cumulativeLineWidget->getWidth());
        }
        int indexCD = plot->diff.getGraphIndex();
        if (indexCD != -1) {
            plot->graph(indexCD)->setPen(plot->cdiff.getPen());
            plotChanged = true;
        }
    }

    for (int i = 0; i < plot->refl.count(); i++) {
        if (reflVisible.at(i) != plot->refl.at(i).isVisible()) {
            if (update) plot->refl[i].setVisible(reflVisible.at(i));
            if (plot->refl[i].xSize() > 0) {
                for (int ind = plot->refl[i].itemIndexStart; ind <= plot->refl[i].itemIndexEnd; ind++) {
                    QCPItemLine *line = dynamic_cast<QCPItemLine *> (plot->item(ind));
                    line->setVisible(plot->refl.at(i).isVisible());
                }
                int index = plot->refl[i].getGraphIndex();
                plot->graph(index)->setVisible(plot->refl.at(i).isVisible());
                if (plot->refl.at(i).isVisible()) {
                    plot->graph(index)->setVisible(true);
                    plot->graph(index)->addToLegend();
                } else {
                    plot->graph(index)->setVisible(false);
                    plot->graph(index)->removeFromLegend();
                }
                redrawRequired = true;
            }
        }
    }

    for (int i = 0; i < plot->refl.count(); i++) {
        QPen pen = plot->refl[i].getPen();
        if (pen != reflPen[i]) {
            if (update) plot->refl[i].setPen(reflPen[i]);
            int index = plot->refl[i].getGraphIndex();
            QPen penref = plot->refl[i].getPen();
            plot->graph(index)->setPen(penref);

            //Update scatter for legend
            QPainterPath customScatterPath(QPointF(0,-10));
            customScatterPath.lineTo(0,10);
            plot->graph(index)->setScatterStyle(QCPScatterStyle(customScatterPath, penref));

            if (plot->refSet[i].ref.size() > 0) {
                for (int ind = plot->refl[i].itemIndexStart; ind <= plot->refl[i].itemIndexEnd; ind++) {
                    QCPItemLine *line = dynamic_cast<QCPItemLine *> (plot->item(ind));
                    line->setPen(penref);
                }
                plotChanged = true;
            }
        }
    }

    if (ui->peaksCheckBox->isChecked() != plot->peaks.isVisible()) {
        if (update) plot->peaks.setVisible(ui->peaksCheckBox->isChecked());
        int indexP = plot->peaks.getGraphIndex();
        if (indexP != -1) {
            plot->graph(indexP)->setVisible(plot->peaks.isVisible());
            if (plot->peaks.isVisible())
                plot->graph(indexP)->addToLegend();
            else
                plot->graph(indexP)->removeFromLegend();
            redrawRequired = true;
        }
    }

    if (ui->peaksLineWidget->isDifferent(plot->peaks.getPen())) {
        if (update) {
            plot->peaks.setLineStyle(ui->peaksLineWidget->getStyle());
            plot->peaks.setLineColor(ui->peaksLineWidget->getColor());
            plot->peaks.setLineWidth(ui->peaksLineWidget->getWidth());
        }
        int indexP = plot->peaks.getGraphIndex();
        if (indexP != -1) {
            plot->graph(indexP)->setPen(plot->peaks.getPen());
            plotChanged = true;
        }
    }

    PlotSettings pSettings = plot->pSettings();

    if (ui->backgroundWidget->isDifferent(pSettings.backColorType,pSettings.backColor,
                                          pSettings.backColor1,pSettings.backColor2)) {
        if (update) {
            ui->backgroundWidget->getColorWidget(pSettings.backColorType,pSettings.backColor,
                                                 pSettings.backColor1,pSettings.backColor2);
        }
        pSettings.applyToBackground(plot);
        plotChanged = true;
    }

    if (ui->layerWidget->isDifferent(pSettings.layerColorType,pSettings.layerColor,
                                     pSettings.layerColor1,pSettings.layerColor2)) {
        if (update) {
            ui->layerWidget->getColorWidget(pSettings.layerColorType,pSettings.layerColor,
                                            pSettings.layerColor1,pSettings.layerColor2);
        }
        pSettings.applyToLayer(plot);
        plotChanged = true;
    }

    if (ui->xAxisLSWidget->isDifferent(pSettings.xAxisPen)) {
        if (update) {
            pSettings.xAxisPen.setColor(ui->xAxisLSWidget->getColor());
            pSettings.xAxisPen.setStyle(ui->xAxisLSWidget->getStyle());
            pSettings.xAxisPen.setWidth(ui->xAxisLSWidget->getWidth());
        }
        plot->xAxis->setBasePen(pSettings.xAxisPen);
        plot->xAxis->setTickPen(pSettings.xAxisPen);
        plot->xAxis->setSubTickPen(pSettings.xAxisPen);
        plotChanged = true;
    }

    if (ui->yAxisLSWidget->isDifferent(pSettings.yAxisPen)) {
        if (update) {
            pSettings.yAxisPen.setColor(ui->yAxisLSWidget->getColor());
            pSettings.yAxisPen.setStyle(ui->yAxisLSWidget->getStyle());
            pSettings.yAxisPen.setWidth(ui->yAxisLSWidget->getWidth());
        }
        plot->yAxis->setBasePen(pSettings.yAxisPen);
        plot->yAxis->setTickPen(pSettings.yAxisPen);
        plot->yAxis->setSubTickPen(pSettings.yAxisPen);
        plotChanged = true;
    }

    if (ui->xAxis2LSWidget->isDifferent(pSettings.xAxis2Pen)) {
        if (update) {
            pSettings.xAxis2Pen.setColor(ui->xAxis2LSWidget->getColor());
            pSettings.xAxis2Pen.setStyle(ui->xAxis2LSWidget->getStyle());
            pSettings.xAxis2Pen.setWidth(ui->xAxis2LSWidget->getWidth());
        }
        plot->xAxis2->setBasePen(pSettings.xAxis2Pen);
        plot->xAxis2->setTickPen(pSettings.xAxis2Pen);
        plot->xAxis2->setSubTickPen(pSettings.xAxis2Pen);
        plotChanged = true;
    }

    if (ui->yAxis2LSWidget->isDifferent(pSettings.yAxis2Pen)) {
        if (update) {
            pSettings.yAxis2Pen.setColor(ui->yAxis2LSWidget->getColor());
            pSettings.yAxis2Pen.setStyle(ui->yAxis2LSWidget->getStyle());
            pSettings.yAxis2Pen.setWidth(ui->yAxis2LSWidget->getWidth());
        }
        plot->yAxis2->setBasePen(pSettings.yAxis2Pen);
        plot->yAxis2->setTickPen(pSettings.yAxis2Pen);
        plot->yAxis2->setSubTickPen(pSettings.yAxis2Pen);
        plotChanged = true;
    }

    if (ui->yGridLSWidget->isDifferent(pSettings.yGridPen)) {
        if (update) {
            pSettings.yGridPen.setColor(ui->yGridLSWidget->getColor());
            pSettings.yGridPen.setStyle(ui->yGridLSWidget->getStyle());
            pSettings.yGridPen.setWidth(ui->yGridLSWidget->getWidth());
        }
        plot->yAxis->grid()->setPen(pSettings.yGridPen);
        plotChanged = true;
    }

    if (ui->xGridLSWidget->isDifferent(pSettings.xGridPen)) {
        if (update) {
            pSettings.xGridPen.setColor(ui->xGridLSWidget->getColor());
            pSettings.xGridPen.setStyle(ui->xGridLSWidget->getStyle());
            pSettings.xGridPen.setWidth(ui->xGridLSWidget->getWidth());
        }
        plot->xAxis->grid()->setPen(pSettings.xGridPen);
        plotChanged = true;
    }

    if (ui->legendCheckBox->isChecked() != pSettings.legendVisible) {
        if (update) pSettings.legendVisible = ui->legendCheckBox->isChecked();
        plot->legend->setVisible(pSettings.legendVisible);
        plotChanged = true;
    }

    if (getAbscissaState() != plot->pSettings().getAbscissa()) {
        if (update) {
            xpdutils::convertAbscissa(plot,plot->plotWave,plot->refl,plot->pSettings().getAbscissa(),getAbscissaState());
            //plot->pSettings().setAbscissa(getAbscissaState());
            pSettings.setAbscissa(getAbscissaState());
        } else {
            xpdutils::convertAbscissa(plot,plot->plotWave,plot->refl,getAbscissaState(),plot->pSettings().getAbscissa());
        }
        //plot->xAxis->setRangeReversed(plot->pSettings().getAbscissa() == DVALUE);
        plot->xAxis->setRangeReversed(pSettings.getAbscissa() == DVALUE);

        plot->updateMinMax();
        plot->xAxis->rescale();
        plotChanged = true;        
    }

    if (mYScaleType != pSettings.yScaleType) {
        if (update) pSettings.yScaleType = mYScaleType;
        plot->yAxis->setScaleType(pSettings.yScaleType);
        plot->yAxis->setLabel(pSettings.getYLabel());
        plotChanged = true;
    }

    if (ui->xAxisTickLabelsCheckBox->isChecked() != pSettings.xTickLabels) {
        if (update) pSettings.xTickLabels = ui->xAxisTickLabelsCheckBox->isChecked();
        plot->xAxis->setTickLabels(pSettings.xTickLabels);
        plotChanged = true;
    }

    if (ui->yAxisTickLabelsCheckBox->isChecked() != pSettings.yTickLabels) {
        if (update) pSettings.yTickLabels = ui->yAxisTickLabelsCheckBox->isChecked();
        plot->yAxis->setTickLabels(pSettings.yTickLabels);
        plotChanged = true;
    }

    if (ui->yOffsetCheckBox->isChecked() != pSettings.hasOffset) {
        if (update) {
            pSettings.hasOffset = ui->yOffsetCheckBox->isChecked();
            pSettings.yOffset = ui->yOffsetLineEdit->text().toDouble();
        }
        emit applyOffsetRequested(pSettings.hasOffset ? pSettings.yOffset : 0.0);
        plotChanged = true;
    } else {
        if (ui->yOffsetCheckBox->isChecked()) {
            double offSet = ui->yOffsetLineEdit->text().toDouble();
            if (offSet != pSettings.yOffset) {
                if (update) {
                    pSettings.yOffset = ui->yOffsetLineEdit->text().toDouble();
                }
                emit applyOffsetRequested(pSettings.yOffset);
                plotChanged = true;
            }
        }
    }

    if (redrawRequired) {
        emit redrawRequested();
        changeApplied = true;
        plotChanged = false; // redrawPlot executes replot
    }

    if (plotChanged) {
        plot->setPlotSettings(pSettings);
        plot->replot();
        changeApplied = true;
    }
}

void PlotStyleDialog::cancel(XpdViewWidget *plot)
                             // , QVector<graphItem> &obs, graphItem &back, graphItem &bpoints, graphItem &calc,
                             // graphItem &diff, graphItem &cdiff, graphItem &peaks, QVector<graphItem> &refl,
                             // const QVector<double> waves)
{
    if (anyChangeApplied()) {
        for (int i = 0; i < plot->obs.count(); i++) {
            plot->obs[i].setPen(obsPen0[i]);
            plot->obs[i].setScatter(obsScatter0[i]);
            plot->obs[i].setVisible(obsVisible0.at(i));
        }
        //psettings = settings0;
        plot->setPlotSettings(settings0);
        plot->back.setPen(backPen0);
        plot->back.setVisible(backVisible0);
        plot->bpoints.setScatter(backScatter0);
        plot->bpoints.setVisible(backpVisible0);
        plot->calc.setPen(calcPen0);
        plot->calc.setVisible(calcVisible0);
        plot->diff.setPen(diffPen0);
        plot->diff.setVisible(diffVisible0);
        plot->cdiff.setPen(cDiffPen0);
        plot->cdiff.setVisible(cDiffVisible0);
        plot->peaks.setPen(peaksPen0);
        plot->peaks.setVisible(peaksVisible0);
        for (int i = 0; i < plot->refl.count(); i++) {
            plot->refl[i].setPen(reflPen0.at(i));
            plot->refl[i].setVisible(reflVisible0.at(i));
        }
        apply(plot,false);
//, obs, back, bpoints, calc, diff, cdiff, peaks, refl, waves, false);
    }
}

bool PlotStyleDialog::anyChangeToApply() const
{    
    return ui->buttonBox->button(QDialogButtonBox::Apply)->isEnabled();
}

bool PlotStyleDialog::anyChangeApplied() const
{    
    return changeApplied;
}

void PlotStyleDialog::on_buttonBox_clicked(QAbstractButton *button)
{
    QDialogButtonBox::StandardButton stdButton = ui->buttonBox->standardButton(button);

    if(stdButton == QDialogButtonBox::Apply) {
        emit dialogClosed(stdButton);
        //setApplied();
        ui->buttonBox->button(QDialogButtonBox::Apply)->setEnabled(false);
    } else if (stdButton == QDialogButtonBox::Ok) {
        emit dialogClosed(stdButton);

    } else if (stdButton == QDialogButtonBox::RestoreDefaults) {
        emit dialogClosed(stdButton);
        enableApplyButton();
    }
}

void PlotStyleDialog::reject()
{
    emit dialogClosed(QDialogButtonBox::Cancel);
    QDialog::reject();
}

void PlotStyleDialog::on_observedComboBox_currentIndexChanged(int index)
{
    if (index < 0) return; // signal emitted for clear combo
    setObserved(index);
}

void PlotStyleDialog::on_reflComboBox_currentIndexChanged(int index)
{
    if (index < 0) return; // signal emitted for clear combo
    setReflections(index);
}

void PlotStyleDialog::enableApplyButton()
{
    ui->buttonBox->button(QDialogButtonBox::Apply)->setEnabled(true);
}

void PlotStyleDialog::connectLineWidget(LineStyleWidget *lsWidget)
{
    connect(lsWidget, &LineStyleWidget::lineColorChanged, this, &PlotStyleDialog::enableApplyButton);

    connect(lsWidget, &LineStyleWidget::lineWidthChanged, this, &PlotStyleDialog::enableApplyButton);

    connect(lsWidget, &LineStyleWidget::lineStyleChanged, this, &PlotStyleDialog::enableApplyButton);
}

void PlotStyleDialog::setObserved(int index)
{
    ui->observedLineWidget->setLineStyle(obsPen.at(index));
    ui->observedMarkerWidget->setMarkerStyle(obsScatter.at(index));
    ui->obsCheckBox->setChecked(obsVisible.at(index));
}

void PlotStyleDialog::setReflections(int index)
{
    ui->reflCheckBox->setChecked(reflVisible.at(index));
    ui->reflectionsLineWidget->setLineStyle(reflPen.at(index));
}

xpdutils::xAbscissaType PlotStyleDialog::getAbscissaState() const
{
    if (ui->tthetaRadioButton->isChecked())
        return xpdutils::TTHETA;
    else if (ui->dRadioButton->isChecked())
        return xpdutils::DVALUE;
    else if (ui->oneOverDRadioButton->isChecked())
        return xpdutils::ONE_OVER_DVALUE;
    else
        return xpdutils::ONE_OVER_DVALUE2;
}
