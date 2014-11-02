package ml;

import javafx.scene.chart.LineChart;
import javafx.scene.chart.NumberAxis;
import javafx.scene.chart.XYChart;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;

/**
 * @author Arkadii Rost
 */
public class PlotterPane extends TabPane {
    public void addChart(double[] points, String tabTitle) {
        getTabs().add(createChartTab(points, tabTitle));
    }

    @SuppressWarnings("unchecked")
    private Tab createChartTab(double[] points, String tabTitle) {
        Tab tab = new Tab();
        tab.setText(tabTitle);
        tab.setClosable(false);
        final NumberAxis xAxis = new NumberAxis();
        final NumberAxis yAxis = new NumberAxis();
        LineChart<Number, Number> chart = new LineChart<>(xAxis, yAxis);
        XYChart.Series series = new XYChart.Series();
        for (int i = 0; i < points.length; i += 2)
            series.getData().add(new XYChart.Data(points[i], points[i + 1]));
        chart.getData().add(series);
        chart.setCreateSymbols(false);
        chart.setLegendVisible(false);
        tab.setContent(chart);
        return tab;
    }
}
