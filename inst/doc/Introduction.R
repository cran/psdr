## ---- eval = FALSE, warning = FALSE-------------------------------------------
#  # Install the package from GitHub
#  # devtools::install_github("yhhc2/psdr")

## ---- warning = FALSE---------------------------------------------------------
# Load package
library("psdr")


## ---- warning = FALSE---------------------------------------------------------

example_data <- GenerateExampleData()

example_data_displayed <- example_data

colnames(example_data_displayed) <- c("Time in seconds", "Signal", "Session", "Category")

head(example_data_displayed)


## ---- warning = FALSE---------------------------------------------------------

#Only works in html, not md. 
rmarkdown::paged_table(example_data_displayed)


## -----------------------------------------------------------------------------

example_data_windows <- GetHomogeneousWindows(example_data, "Session", c("Session"))


## ---- warning = FALSE---------------------------------------------------------

plot_result <- ggplot2::ggplot(subset(example_data, example_data$Category=="A"), ggplot2::aes(x = Time, y = Signal, colour = Session, group = 1)) + ggplot2::geom_line()

plot_result


## ---- warning = FALSE---------------------------------------------------------

plot_result <- ggplot2::ggplot(subset(example_data, example_data$Category=="B"), ggplot2::aes(x = Time, y = Signal, colour = Session, group = 1)) + ggplot2::geom_line()

plot_result



## ---- warning = FALSE, results = 'hide'---------------------------------------

FirstComboToUse <- list( c(1, 2, 3), c("A") )

SecondComboToUse <- list( c(4, 5, 6), c("B") )

timeseries.results <- AutomatedCompositePlotting(list.of.windows = example_data_windows,
                           name.of.col.containing.time.series = "Signal",
                           x_start = 0,
                           x_end = 999,
                           x_increment = 1,
                           level1.column.name = "Session",
                           level2.column.name = "Category",
                           level.combinations = list(FirstComboToUse, SecondComboToUse),
                           level.combinations.labels = c("A", "B"),
                           plot.title = "Comparing category A and B",
                           plot.xlab = "Time in 0.01 second increments",
                           plot.ylab = "Original units of signal",
                           combination.index.for.envelope = NULL,
                           TimeSeries.PSD.LogPSD = "TimeSeries",
                           sampling_frequency = NULL)

ggplot.obj.timeseries <- timeseries.results[[2]]

ggplot.obj.timeseries


## ---- warning = FALSE---------------------------------------------------------

data1 <- example_data_windows[[1]]
psd_results1 <- MakePowerSpectralDensity(100, data1$Signal)

data2 <- example_data_windows[[2]]
psd_results2 <- MakePowerSpectralDensity(100, data2$Signal)

data3 <- example_data_windows[[3]]
psd_results3 <- MakePowerSpectralDensity(100, data3$Signal)

Frequency <- c(psd_results1[[1]], psd_results2[[1]], psd_results3[[1]])
PSD <- c(psd_results1[[2]], psd_results2[[2]], psd_results3[[2]])
Session <- c(rep(1, length(psd_results1[[1]])), rep(2, length(psd_results1[[1]])), 
             rep(3, length(psd_results1[[1]])))

data_to_plot <- data.frame(Frequency, PSD, Session)

plot_results <- ggplot2::ggplot(data=data_to_plot, ggplot2::aes(x=Frequency, y=PSD, color = as.factor(Session), group=1)) +
  ggplot2::geom_point() + ggplot2::geom_path() + ggplot2::xlim(0,3)

plot_results


## ---- warning = FALSE---------------------------------------------------------


data1 <- example_data_windows[[4]]
psd_results1 <- MakePowerSpectralDensity(100, data1$Signal)

data2 <- example_data_windows[[5]]
psd_results2 <- MakePowerSpectralDensity(100, data2$Signal)

data3 <- example_data_windows[[6]]
psd_results3 <- MakePowerSpectralDensity(100, data3$Signal)

Frequency <- c(psd_results1[[1]], psd_results2[[1]], psd_results3[[1]])
PSD <- c(psd_results1[[2]], psd_results2[[2]], psd_results3[[2]])
Session <- c(rep(4, length(psd_results1[[1]])), rep(5, length(psd_results1[[1]])), 
             rep(6, length(psd_results1[[1]])))

data_to_plot <- data.frame(Frequency, PSD, Session)

plot_results <- ggplot2::ggplot(data=data_to_plot, ggplot2::aes(x=Frequency, y=PSD, color = as.factor(Session), group=1)) +
  ggplot2::geom_point() + ggplot2::geom_path() + ggplot2::xlim(0,3)

plot_results


## ---- warning = FALSE, results = 'hide'---------------------------------------

FirstComboToUse <- list( c(1, 2, 3), c("A") )

SecondComboToUse <- list( c(4, 5, 6), c("B") )

PSD.results <- AutomatedCompositePlotting(list.of.windows = example_data_windows,
                           name.of.col.containing.time.series = "Signal",
                           x_start = 0,
                           x_end = 5,
                           x_increment = 0.01,
                           level1.column.name = "Session",
                           level2.column.name = "Category",
                           level.combinations = list(FirstComboToUse, SecondComboToUse),
                           level.combinations.labels = c("A", "B"),
                           plot.title = "Comparing category A and B",
                           plot.xlab = "Hz",
                           plot.ylab = "(Original units)^2/Hz",
                           combination.index.for.envelope = NULL,
                           TimeSeries.PSD.LogPSD = "PSD",
                           sampling_frequency = 100)

ggplot.obj.PSD <- PSD.results[[2]]

ggplot.obj.PSD


## ---- warning = FALSE, results = 'hide'---------------------------------------
PSD.results <- AutomatedCompositePlotting(list.of.windows = example_data_windows,
                           name.of.col.containing.time.series = "Signal",
                           x_start = 0,
                           x_end = 5,
                           x_increment = 0.01,
                           level1.column.name = "Session",
                           level2.column.name = "Category",
                           level.combinations = list(FirstComboToUse, SecondComboToUse),
                           level.combinations.labels = c("A", "B"),
                           plot.title = "Comparing category A and B",
                           plot.xlab = "Hz",
                           plot.ylab = "(Original units)^2/Hz",
                           combination.index.for.envelope = 1,
                           TimeSeries.PSD.LogPSD = "PSD",
                           sampling_frequency = 100
                           )

ggplot.obj.PSD <- PSD.results[[2]]

ggplot.obj.PSD

## ---- warning = FALSE, results = 'hide'---------------------------------------
PSD.results <- AutomatedCompositePlotting(list.of.windows = example_data_windows,
                           name.of.col.containing.time.series = "Signal",
                           x_start = 0,
                           x_end = 5,
                           x_increment = 0.01,
                           level1.column.name = "Session",
                           level2.column.name = "Category",
                           level.combinations = list(FirstComboToUse, SecondComboToUse),
                           level.combinations.labels = c("A", "B"),
                           plot.title = "Comparing category A and B",
                           plot.xlab = "Hz",
                           plot.ylab = "(Original units)^2/Hz",
                           combination.index.for.envelope = 2,
                           TimeSeries.PSD.LogPSD = "PSD",
                           sampling_frequency = 100
                           )

ggplot.obj.PSD <- PSD.results[[2]]

ggplot.obj.PSD

## ---- warning = FALSE, results = 'hide'---------------------------------------
LogPSD.results <- AutomatedCompositePlotting(list.of.windows = example_data_windows,
                           name.of.col.containing.time.series = "Signal",
                           x_start = 0,
                           x_end = 5,
                           x_increment = 0.01,
                           level1.column.name = "Session",
                           level2.column.name = "Category",
                           level.combinations = list(FirstComboToUse, SecondComboToUse),
                           level.combinations.labels = c("A", "B"),
                           plot.title = "Comparing category A and B",
                           plot.xlab = "Hz",
                           plot.ylab = "Log((Original units)^2/Hz)",
                           combination.index.for.envelope = NULL,
                           TimeSeries.PSD.LogPSD = "LogPSD",
                           sampling_frequency = 100
                           )

ggplot.obj.LogPSD <- LogPSD.results[[2]]

ggplot.obj.LogPSD

## ---- warning = FALSE---------------------------------------------------------

comparison_results <- PSD.results[[3]]

dominant_freq_for_comparison <- comparison_results[[1]]

kruskal_wallis_test_results <- comparison_results[[2]]

wilcoxon_rank_sum_test_results <- comparison_results[[3]]


## ---- warning = FALSE---------------------------------------------------------
dominant_freq_for_comparison

## ---- warning = FALSE---------------------------------------------------------
kruskal_wallis_test_results

## ---- warning = FALSE---------------------------------------------------------
wilcoxon_rank_sum_test_results

