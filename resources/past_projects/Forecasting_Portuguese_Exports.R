# Packages
library(fpp3)
library(urca)
library(tidyverse)

############################### 1. LOAD AND INITIAL INSPECTION #################
pt_exports <- readxl::read_excel(
  "C:/Users/paulr/OneDrive/Área de Trabalho/FM_Dataset_PortugueseExports.xlsx"
)

pt_exports |>
  mutate(Month =
           yearmonth(Month)
  ) -> pt_exports

# Initial Inspection - considerations on forecast scope (a crucial step)

str(pt_exports)

# tbl_ts [362 × 4] (S3: tbl_ts/tbl_df/tbl/data.frame)
# $ Region       : chr [1:362] "World" "World" "World" "World" ...
# $ Type_of_goods: chr [1:362] "Total" "Total" "Total" "Total" ...
# $ Month        : mth [1:362] 1993 Jan, 1993 Feb, 1993 Mar, 1993 Apr, ...
# $ Value        : num [1:362] 9.84e+08 9.73e+08 1.18e+09 1.05e+09 1.03e+09 ...
# - attr(*, "key")= tibble [1 × 3] (S3: tbl_df/tbl/data.frame)
# ..$ Region       : chr "World"
# ..$ Type_of_goods: chr "Total"
# ..$ .rows        : list<int> [1:1]
# .. ..$ : int [1:362] 1 2 3 4 5 6 7 8 9 10 ...
# .. ..@ ptype: int(0)
# ..- attr(*, ".drop")= logi TRUE
# - attr(*, "index")= chr "Month"
# ..- attr(*, "ordered")= logi TRUE
# - attr(*, "index2")= chr "Month"
# - attr(*, "interval")= interval [1:1] 1M
# ..@ .regular: logi TRUE

# Structure of our data, shows we see that we have a tsibble composed of several
# sub-series with categories, 'Region' and 'Type of Goods'. We will concern
# ourselves with only one of these series, that is only one combination of those
# two categories.

# First plotting the sub time-series that correspond to each region, with
# respect to goods. Where type of goods will be the Total number of exports made
# from the Portuguese economy.

# Regions of interest are the European Union, PALOPs and the World aggregate,
# (Type of Goods subset, available only pre-2012).

pt_exports |>
  filter(Region == 'World', year(Month) < 2012) |>
  as_tsibble(index=Month, key=c(Type_of_goods, Region)) |>
  autoplot()

pt_exports |>
  filter(Region == 'Intra European Union', year(Month) < 2012) |>
  as_tsibble(index=Month, key=c(Type_of_goods, Region)) |>
  autoplot()

# World and IEU show very similar sub-series vis-a-vis type of goods, while
# type of good sub-series show different trend levels.

pt_exports |>
  filter(Region == 'PALOP', year(Month) < 2012) |>
  as_tsibble(index=Month, key=c(Type_of_goods, Region)) |>
  autoplot()

# PALOP region shows very distinct sub-series from other regions but its
# sub-series are very similar between themselves.

# We now filter for region and type of goods total.

# PALOPs
pt_exports |>
  filter(Region=='PALOP', Type_of_goods=='Total') |>
  as_tsibble(index=Month, key=c(Type_of_goods, Region)) -> pt_palop_exports

# Intra European Union (IEU)
pt_exports |>
  filter(Region=='Intra European Union', Type_of_goods=='Total') |>
  as_tsibble(index=Month, key=c(Type_of_goods, Region)) -> pt_ieu_exports

# World
pt_exports |>
  filter(Region=='World', Type_of_goods=='Total') |>
  as_tsibble(index=Month, key=c(Type_of_goods, Region)) -> pt_exports

# And plot them

pt_exports |>
  autoplot(Value) +
  labs(y = 'Total Exports €')

pt_ieu_exports |>
  autoplot(Value) +
  labs(y = 'Intra European Union Exports €')

pt_palop_exports |>
  autoplot(Value) +
  labs(y = 'PALOP Exports €')

# Obviously, the World aggregate contains the other two being displayed. Now,
# crucially note, the PALOP sub-series behaves very differently from both
# the World aggregate and the IEU ones.

pt_exports |>
  mutate(test = pt_palop_exports$Value/Value) |>
  autoplot(test) + labs(y='PALOP/Total (Exports Ratio)')

# The ratio of PALOP exports vis. World exports ranges from values that seem
# to hover below the 3% level and at times rise above the 10% level. Therefore
# this distinct behavior cannot be ignored as it will likely impact forecasting
# accuracy negatively, if we to attempt to forecast World aggregate exports.

# As can be seen in the plot below
pt_exports |>
  mutate(test = pt_ieu_exports$Value/Value) |>
  autoplot(test) + labs(y='IEN/Total (Exports Ratio)')

# Exports to the European Union are preponderant to the Portuguese economy,
# and this sub-series is much more similar to the aggregate figures. Thus we
# will model this last sub-series (Portuguese Intra European Union Exports),
# and leave the challenge of forecasting the aggregate for another opportunity.

# Concluding, after this initial exploration, we now narrow our scope to
# forecast Portuguese Aggregate Exports to Intra European Union Countries, while
# it must be noted that Total exports will contain trends from multiple distinct
# sub time series.

# We now select only those columns that are of interest.
data <- pt_ieu_exports |>
  select(Month, Value)

# And divide the data into a training sample, and a testing sample.
data_training = data |>
  filter(year(Month) < 2015)

data_test = data |>
  filter(year(Month) >= 2015)

# Plot the training set performing some data exploration steps.
data_training |>
  autoplot(Value)

# Plotting the monthly sub-series
data_training|>
  gg_subseries(Value)

# Plotting lags - 12M
data_training |>
  gg_lag(lags = seq(1,12))

# Green-yellowish months seem to correlate more strongly throughout lags. While
# lag 12 presents the more appealing strings.
# August with the lowest performance yOy, Spring being the more volatile of the
# periods. August through, February more or less similar yOy.

# Meta analysis using an STL decomposition
data_training |>
  model(STL(Value ~ trend())) |>
  components() -> parts

# Plotting the yOy seasonality effect.
parts |>
  gg_season(season_year)

# Note the growing volatility around the month of August and the sudden growth
# of the Autumn months.

# Plotting Density Spectrum

# For original data
parts |>
  gg_tsdisplay(
    plot_type='spectrum',
    lag_max = 24
  )

# For the yOy seasonal component of the STL
parts |>
  gg_tsdisplay(
    season_year,
    plot_type='spectrum',
    lag_max = 24
  )
# Because they are very beautiful.

# Plotting the ACF and PCF
data_training |>
  gg_tsdisplay(
    plot_type='partial',
    lag_max = 24
  )
# Strong indication of auto-correlation

# ADF Test
summary(ur.df(data_training$Value,
              type=c("drift"), lags=18))

# Value of test-statistic is: -1.2858 3.2814
#
# Critical values for test statistics:
#       1pct  5pct 10pct
# tau2 -3.44 -2.87 -2.57
# phi1  6.47  4.61  3.79

# H0: Failed to reject, the time series is non-stationary (for all relevant CI).

############################### 2. TRANSFORMATIONS #############################

# Square-Root
sqrt_ts <- sqrt(data_training$Value)

data_training |>
  autoplot(sqrt_ts)

# Logarithmic
log_ts <- log(data_training$Value)

data_training |>
  autoplot(log_ts)

# Box-Cox
cox_ts <- box_cox(
  data_training$Value,
  data_training |> features(Value, guerrero) |> pull(lambda_guerrero)
)

data_training |>
  autoplot(
    box_cox(
      Value,
      data_training |> features(Value, guerrero))
  )

############################### 3. UNIT ROOT TESTS #############################

# Augmented Dickey-Fuller Test Unit Root Test #

summary(ur.df(na.omit(data_training$Value),
              type=c('drift'), lags=18))

summary(ur.df(na.omit(sqrt_ts),
              type=c('drift'), lags=18))

summary(ur.df(na.omit(log_ts),
              type=c('drift'), lags=18))

summary(ur.df(na.omit(cox_ts),
              type=c('drift'), lags=18))

# Value of test-statistic are:
# Baseline:       -1.2858 3.2814
# Square Root:    -1.8151 4.1547
# Logarithmic:    -2.4168 5.1278
# Box and Cox:    -2.0199 4.4917

# Critical values for test statistics:
#       1pct  5pct 10pct
# tau2 -3.46 -2.88 -2.57
# phi1  6.52  4.63  3.81

# Upon visual inspection all transformations seemed to have performed more or
# less equally well. However, performing an ADF Unit Root Test on each of the
# transformation suggests strongly that the transformation for which the auto
# correlations tend to lower is in the logarithmic transformation.

# Since the differences operation is a linear one, and the logarithm
# transformation is a stronger transformation than the square and of similar
# strength to the box and cox, that is, log(x) < sqrt(x) for all x > 2, this
# implies ceteris paribus that a linear factor multiplied by a logged value
# will skew faster into criticallity than a linear factor multiplied by a
# square rooted or box-coxed value, hence we expect the log differences to
# outperform the other transformations differences.

# We now perform the ADF URT on the logged differences.
# Testing autocorrelation of the differences

#  Differences of the seasonal component

log_diff_ts <- difference(log(data_training$Value), 12)

data_training |>
  gg_tsdisplay(
    log_diff_ts,
    plot_type='partial',
    lag_max = 24
  )

summary(ur.df(na.omit(log_diff_ts),
              type=c('drift'), lags=18))

# Differences of the differences of the seasonal component
log_2diff_ts <- difference(difference(log(data_training$Value), 12))

summary(ur.df(na.omit(log_2diff_ts),
              type=c('drift'), lags=18))

data_training |>
  gg_tsdisplay(
    log_2diff_ts,
    plot_type='partial',
    lag_max = 24
  )

# Augmented Dickey-Fuller Test Unit Root Test #

# Value of test-statistic are:
# Baseline:       -1.2858 3.2814
# Logarithmic:    -2.4168 5.1278
# Seasonal:       -3.4923 6.1287
# Seasonal +:     -4.1408 8.5731

# Critical values for test statistics:
#       1pct  5pct 10pct
# tau2 -3.46 -2.88 -2.57
# phi1  6.52  4.63  3.81

# Both seasonal differences only, and seasonal and trend differences met show
# strong evidence that their respective time series is stationary.

############################### 4. METHODOLOGY #################################

# For the sake of rigor we perform cross validation applying the stretch method
# to our data - starting from 1993 up to 2013. And measure the performance
# of our model by providing forecasts from 2014 to 2019. This is purposeful as
# we wish to find out if our model can forecast during the 2013- 2014 Euro
# crisis; having learned from similar events during the Great Recession of 2008.

# Also, in those models that allow the manual specification of the differences,
# we specify those within the model i.e., ARIMA models, rather than providing
# the transformed variables.

# We will create two distinct types of models: ARIMA and ETS.

# Upon suggestion of the professor, we will benchmark these against naive methods
# such as DRIFT, MEAN, SNAIVE and NAIVE; as well as against X13_SEATS and STL.

# Using the stretch CV framework it would be extremely cumbersome to run
# the auto-arima models, as well as the auto-ETS models, thus we will use them
# now as a guide running them, printing their results and plotting the ACF's
# for the log seasonal differences of our data once more to attempt to find
# a nice starting point for our modelling efforts.

# Then we will run each of the most promising models in both of the model family
# types ARIMA and ETS, then the NNAR and finally the naive methods and finally
# the STL and X13 benchmarks.

# ARIMA Models

# In our approach to modelling we will perform the following steps for each
# candidate model:
#   - Specification - define mean/drift, order, seasonality;
#   - Visual inspection - plot acf and residual histogram;
#   - Shapiro-Wilks Test - test the normality of the residuals;
#   - Ljung-Box Test - test the auto-correlations of the residuals;
#   - Rank - rank the candidate model on (AICc, BIC and sigma^2)

# Models can be disqualified at any of the steps, also.

############################### 5. CANDIDATE ###################################

# We compute the recommended models
auto_model <- data_training |>
  model(
    auto_arima = ARIMA(log(Value), stepwise=FALSE, approximation=FALSE),
    auto_ets = ETS(log(Value))
  )

# The results of the auto ARIMA and ETS models are:
  # <ARIMA(2,1,0)(2,1,2)[12]>
  # <ETS(A,Ad,A)>

# We now test some candidate models.

fit_arima <- data_training |>
  model(
    sarima1_100011 = ARIMA(log(Value) ~ 1 + pdq(1,0,0) + PDQ(0,1,1)),
    sarima1_100111 = ARIMA(log(Value) ~ 1 + pdq(1,0,0) + PDQ(1,1,1)),
    sarima1_100112 = ARIMA(log(Value) ~ 1 + pdq(1,0,0) + PDQ(1,1,2)),
    sarima1_100012 = ARIMA(log(Value) ~ 1 + pdq(1,0,0) + PDQ(0,1,2)),
    sarima1_200012 = ARIMA(log(Value) ~ 1 + pdq(2,0,0) + PDQ(0,1,2)),
    sarima0_210212 = ARIMA(log(Value) ~ 0 + pdq(2,1,0) + PDQ(2,1,2)),
    sarima1_210212 = ARIMA(log(Value) ~ 1 + pdq(2,1,0) + PDQ(2,1,2)),
    sarima0_110014 = ARIMA(log(Value) ~ 0 + pdq(1,1,0) + PDQ(0,1,4)),
    sarima0_210014 = ARIMA(log(Value) ~ 0 + pdq(2,1,0) + PDQ(0,1,4)),
    sarima0_600216 = ARIMA(log(Value) ~ 0 + pdq(6,0,0) + PDQ(2,1,6)),
    sarima0_600116 = ARIMA(log(Value) ~ 0 + pdq(6,0,0) + PDQ(1,1,6)),
    sarima0_700116 = ARIMA(log(Value) ~ 0 + pdq(7,0,0) + PDQ(1,1,6)),
    sarima0_700316 = ARIMA(log(Value) ~ 0 + pdq(7,0,0) + PDQ(3,1,6)),
    sarima0_700216 = ARIMA(log(Value) ~ 0 + pdq(7,0,0) + PDQ(2,1,6)),
    sarima0_700213 = ARIMA(log(Value) ~ 0 + pdq(7,0,0) + PDQ(2,1,3)),
    sarima0_700215 = ARIMA(log(Value) ~ 0 + pdq(7,0,0) + PDQ(2,1,5)),
    sarima1_600016 = ARIMA(log(Value) ~ 1 + pdq(6,0,0) + PDQ(0,1,6)),
    sarima1_700016 = ARIMA(log(Value) ~ 1 + pdq(7,0,0) + PDQ(0,1,6)),
    sarima1_600116 = ARIMA(log(Value) ~ 1 + pdq(6,0,0) + PDQ(1,1,6)),
    sarima1_700116 = ARIMA(log(Value) ~ 1 + pdq(7,0,0) + PDQ(1,1,6)),
    sarima1_700316 = ARIMA(log(Value) ~ 1 + pdq(7,0,0) + PDQ(3,1,6)),
    sarima1_700216 = ARIMA(log(Value) ~ 1 + pdq(7,0,0) + PDQ(2,1,6)),
    sarima1_700213 = ARIMA(log(Value) ~ 1 + pdq(7,0,0) + PDQ(2,1,3)),
    sarima1_700215 = ARIMA(log(Value) ~ 1 + pdq(7,0,0) + PDQ(2,1,5)),
  )
#
############################### 6. CANDIDATE TESTS #############################

# Initializing vectors to store the results
lBox_pvalues <- c()
lBox_results  <- c()
shapiroW_pvalues <- c()
shapiroW_results  <- c()

for (model in colnames(fit_arima)) {

  # Ljung-Box Test

  fit_arima[model] |>
    augment() |>
    features(
      .innov,
      ljung_box,
      lag=24,
      dof=(
        (fit_arima[model]  |>
           coef() |>
           nrow())
      )
    ) |> pull(lb_pvalue) -> lbox_pvalue

  # Shapiro-Wilks Test

  shapiro.test(
      fit_arima[model] |>
        residuals() |>
        pull(.resid)
    )$p.value -> shapiro_pvalue


  append(
    lBox_results,
    ifelse(lbox_pvalue < 0.05, 'FAIL', 'PASS')
  ) -> lBox_results

  append(
    shapiroW_results,
    ifelse(shapiro_pvalue < 0.05, 'FAIL', 'PASS')
  ) -> shapiroW_results

  append(lBox_pvalues, round(lbox_pvalue,4)) -> lBox_pvalues
  append(shapiroW_pvalues, round(shapiro_pvalue, 4)) -> shapiroW_pvalues

  # Residuals

  print(
    fit_arima[model] |>
      gg_tsresiduals() +
      labs(y='Value', title=model)
  )

  # Forecast

  print(
    fit_arima[model] |>
      forecast(h=98) |>
        autoplot(data) +
        labs(y='Value', title=model)
  )

}

stats_tests_df <- data.frame(
  colnames(fit_arima),
  lBox_pvalues,
  lBox_results,
  shapiroW_pvalues,
  shapiroW_results
  )

############################### 7. CANDIDATE ANALYSIS DISCUSSION ###############

stats_tests_df

#              model lBox_pvalues lBox_results shapiroW_pvalues shapiroW_results
# 1   sarima1_100011       0.0000         FAIL           0.2426             PASS
# 2   sarima1_100111       0.0000         FAIL           0.1606             PASS
# 3   sarima1_100112       0.0000         FAIL           0.2362             PASS
# 4   sarima1_100012       0.0000         FAIL           0.1853             PASS
# 5   sarima1_200012       0.0000         FAIL           0.3554             PASS
# 6   sarima0_210212       0.0263         FAIL           0.6803             PASS
# 7   sarima1_210212       0.0180         FAIL           0.6713             PASS
# 8   sarima0_110014       0.0000         FAIL           0.6274             PASS
# 9   sarima0_210014       0.0010         FAIL           0.1273             PASS
# 10  sarima0_600216       0.1301         PASS           0.5534             PASS
# 11  sarima0_600116       0.0006         FAIL           0.2186             PASS
# 12  sarima0_700116       0.0513         PASS           0.2358             PASS
# 13  sarima0_700316       0.2303         PASS           0.2456             PASS
# 14  sarima0_700216       0.3597         PASS           0.2493             PASS
# 15  sarima0_700213       0.5028         PASS           0.1547             PASS
# 16  sarima0_700215       0.0557         PASS           0.2921             PASS
# 17  sarima1_600016       0.0006         FAIL           0.1750             PASS
# 18  sarima1_700016       0.0521         PASS           0.2026             PASS
# 19  sarima1_600116       0.0003         FAIL           0.2108             PASS
# 20  sarima1_700116       0.0422         FAIL           0.3249             PASS
# 21  sarima1_700316       0.1969         PASS           0.2405             PASS
# 22  sarima1_700216       0.3193         PASS           0.2383             PASS
# 23  sarima1_700213       0.4638         PASS           0.1372             PASS
# 24  sarima1_700215       0.0518         PASS           0.2920             PASS

# All models passed the Normality test, only a few models passed the auto-correlation
# test.

fit_arima |>
  glance() |>
  print(n=30)

  #    .model          sigma2 log_lik   AIC  AICc   BIC ar_roots   ma_roots
  #    <chr>            <dbl>   <dbl> <dbl> <dbl> <dbl> <list>     <list>
  # 1  sarima1_100011 0.00454    320. -632. -631. -618. <cpl [1]>  <cpl [12]>
  # 2  sarima1_100111 0.00427    328. -647. -646. -629. <cpl [13]> <cpl [12]>
  # 3  sarima1_100112 0.00404    335. -658. -658. -637. <cpl [13]> <cpl [24]>
  # 4  sarima1_100012 0.00406    334. -658. -658. -641. <cpl [1]>  <cpl [24]>
  # 5  sarima1_200012 0.00318    365. -719. -718. -698. <cpl [2]>  <cpl [24]>
  # 6  sarima0_210212 0.00242    393. -772. -771. -747. <cpl [26]> <cpl [24]>
  # 7  sarima1_210212 0.00243    393. -770. -769. -742. <cpl [26]> <cpl [24]>
  # 8  sarima0_110014 0.00289    373. -733. -733. -712. <cpl [1]>  <cpl [48]>
  # 9  sarima0_210014 0.00263    387. -759. -759. -735. <cpl [2]>  <cpl [48]>
  # 10 sarima0_600216 0.00210    409. -787. -785. -734. <cpl [30]> <cpl [72]>
  # 11 sarima0_600116 0.00258    393. -758. -756. -709. <cpl [18]> <cpl [72]>
  # 12 sarima0_700116 0.00248    398. -767. -765. -714. <cpl [19]> <cpl [72]>
  # 13 sarima0_700316 0.00209    412. -789. -787. -729. <cpl [43]> <cpl [72]>
  # 14 sarima0_700216 0.00210    411. -791. -788. -734. <cpl [31]> <cpl [72]>
  # 15 sarima0_700213 0.00194    408. -791. -789. -745. <cpl [31]> <cpl [36]>
  # 16 sarima0_700215 0.00239    402. -774. -772. -721. <cpl [31]> <cpl [60]>
  # 17 sarima1_600016 0.00252    396. -764. -762. -714. <cpl [6]>  <cpl [72]>
  # 18 sarima1_700016 0.00242    402. -773. -771. -720. <cpl [7]>  <cpl [72]>
  # 19 sarima1_600116 0.00257    394. -759. -757. -706. <cpl [18]> <cpl [72]>
  # 20 sarima1_700116 0.00245    400. -769. -766. -712. <cpl [19]> <cpl [72]>
  # 21 sarima1_700316 0.00208    414. -791. -788. -728. <cpl [43]> <cpl [72]>
  # 22 sarima1_700216 0.00208    413. -793. -790. -733. <cpl [31]> <cpl [72]>
  # 23 sarima1_700213 0.00193    410. -792. -790. -743. <cpl [31]> <cpl [36]>
  # 24 sarima1_700215 0.00236    404. -776. -774. -720. <cpl [31]> <cpl [60]>

# More complex models seem to capture the most information, which  makes
# intuitive sense since the series is actually an aggregate of many
# smaller time series. It is also true however, that these more complex models
# achieve this at the expense of non-linear error.

fit_arima |>
  forecast(h=98) |>
  accuracy(data) |>
  print(n=30)

#   .model         .type          ME       RMSE        MAE     MPE  MAPE  MASE   RMSSE  ACF1
#   <chr>          <chr>       <dbl>      <dbl>      <dbl>    <dbl> <dbl> <dbl>    <dbl> <dbl>
#
# 1  sarima0_110014 Test  -334686736. 594899838. 395581743.  -9.75   11.4   2.31  2.70  0.846
# 2  sarima0_210014 Test  -145233594. 423456287. 269077617.  -4.88   8.23   1.57  1.92  0.790
# 3  sarima0_210212 Test  -712394108. 972175115. 727152392.  -19.7   20.2   4.24  4.41  0.892
# 4  sarima0_600116 Test   -82728764. 389731559. 250110534.  -3.43   7.67   1.46  1.77  0.770
# 5  sarima0_600216 Test  -305401747. 452839607. 320121329.  -9.27   9.71   1.87  2.05  0.745
# 6  sarima0_700116 Test     2456148. 379003248. 261289924.  -1.24   7.74   1.52  1.72  0.765
# 7  sarima0_700213 Test   -52620320. 323795631. 207705934.  -2.58   6.35   1.21  1.47  0.716
# 8  sarima0_700215 Test    19906571. 397272072. 282707054.  -0.91   8.30   1.65  1.80  0.791
# 9  sarima0_700216 Test  -170118474. 358731139. 229016047.  -5.71   7.21   1.34  1.63  0.726
# 10 sarima0_700316 Test  -148322083. 349788942. 219694236.  -5.15   6.93   1.28  1.59  0.726
# 11 sarima1_100011 Test  -231209462. 450466538. 275889857.  -7.53   8.60   1.61  2.04  0.770
# 12 sarima1_100012 Test  -262034472. 469844150. 297570374.  -8.32   9.20   1.74  2.13  0.764
# 13 sarima1_100111 Test  -280223487. 482860156. 309981839.  -8.82   9.55   1.81  2.19  0.762
# 14 sarima1_100112 Test  -247807953. 460060890. 287581756.  -7.95   8.92   1.68  2.08  0.768
# 15 sarima1_200012 Test  -256660239. 483981525. 300709120.  -8.00   9.21   1.75  2.19  0.786
# 16 sarima1_210212 Test  -299643108. 504566227. 331122439.  -9.04   9.97   1.93  2.29  0.803
# 17 sarima1_600016 Test  -376014348. 583664639. 389156908.  -11.2   11.6   2.27  2.65  0.827
# 18 sarima1_600116 Test  -373554840. 589439876. 392129756.  -11.1   11.6   2.29  2.67  0.827
# 19 sarima1_700016 Test  -310412890. 522767367. 331389829.  -9.52   10.1   1.93  2.37  0.814
# 20 sarima1_700116 Test  -330673320. 545435379. 352057477.  -10.0   10.6   2.05  2.47  0.814
# 21 sarima1_700213 Test  -388180200. 542637488. 398001502.  -11.4   11.7   2.32  2.46  0.775
# 22 sarima1_700215 Test  -349853858. 559596596. 376563571.  -10.7   11.4   2.20  2.54  0.823
# 23 sarima1_700216 Test  -481047736. 625921413. 491826840.  -13.9   14.2   2.87  2.84  0.802
# 24 sarima1_700316 Test  -460307185. 607003355. 468893684.  -13.4   13.6   2.73  2.75  0.800

# The accuracy metrics for many of the preferred models using the Information
# Criterion are in fact very poor.

# Crucially, note that, as was briefly mentioned the Portuguese Exports
# time-series is an aggregate of many smaller sub-time series, some of these
# series are rather small in size, yet on a visual inspection seem to have very
# well defined behaviors, with respect to seasonality and trend. We can infer
# from this that, if the coefficients in our models purport to the combination
# of the coefficients of each of those sub-series, and the coefficients of their
# dependence relations where applicable.

# If the former is true, those sub-series with small values might not impact the
# aggregate series level, yet when calculating coefficients to build a model, not
# accounting for them could potentially lead to auto-correlation in the residuals.

# More complex models lead to more degrees of freedom, which increases variance,
# thus we seem to be at a cross-roads between accepting bias or accepting variance.

# Fortunately, Hyndman provides us with a tool to evaluate the quality of our
# models, in the shape of the Continuous Ranked Probability Score, which takes
# into account the fit of all values in the distribution that underlies the
# forecast, and not only its mean.

# We are blessed here with the fact that none of our models failed the normality
# test, meaning that despite there being bias in the residuals, it is distributed
# to either side of the distribution normally.

fit_arima |>
  forecast(h=98) |>
  accuracy(data, list(crps = CRPS)) |>
  print(n=30)

#    .model         .type     crps
#    <chr>          <chr>     <dbl>
# 1  sarima0_110014 Test  375801363.
# 2  sarima0_210014 Test  289559271.
# 3  sarima0_210212 Test  420457761.
# 4  sarima0_600116 Test  275381216.
# 5  sarima0_600216 Test  285844695.
# 6  sarima0_700116 Test  291879986.
# 7  sarima0_700213 Test  276210876.
# 8  sarima0_700215 Test  304256711.
# 9  sarima0_700216 Test  284017113.
# 10 sarima0_700316 Test  282396233.
# 11 sarima1_100011 Test  223570437.
# 12 sarima1_100012 Test  230809593.
# 13 sarima1_100111 Test  236502610.
# 14 sarima1_100112 Test  226864716.
# 15 sarima1_200012 Test  242015657.
# 16 sarima1_210212 Test  334276246.
# 17 sarima1_600016 Test  280913111.
# 18 sarima1_600116 Test  283510239.
# 19 sarima1_700016 Test  259882998.
# 20 sarima1_700116 Test  268810856.
# 21 sarima1_700213 Test  266543087.
# 22 sarima1_700215 Test  276194748.
# 23 sarima1_700216 Test  301980243.
# 24 sarima1_700316 Test  293010771.

# Note that we wish to minimize the value; and sarima1_100011, the simplest
# model, is the one that minimizes.

############################### 8. MODEL SELECTION #############################

# Below we present our chosen models and the rationale for their particular choices.

# 11 sarima1_100011 - Minimizes CRPS.

# 12 sarima1_100012 - Performs almost as well as sarima1_100011 on CRPS.

# 13 sarima1_100111 - Simple model with low CRPS, good accuracy and normally
# distributed residuals.

# 15 sarima1_200012 -  Simple model with low CRPS, good accuracy and normally
# distributed residuals.

# 16 sarima1_210212 - Model selected by the Auto-Arima function.

# 20 sarima0_700116 - Model with very High Information Criterion for which point
# forecast adhered to the actual level of the test series, passed both statistical
# tests.

# 7  sarima0_700213 - Model with the least MAPE, which is the preferred
# metric according to Hyndman.

# 10 sarima0_700316 - Model with the second best MAPE, passing all criteria.

############################### 9. CROSS VALIDATION ############################

# We now proceed to cross-validate our chosen models using stretch_tsibble in
# 12 months increments, starting from a size time-series with 120 months worth
# of observations.

# Create a tsibble object containing the stretches of the training dataset.

data_stretch <- data_training |>
  slice(1:(n()-12)) |>
  stretch_tsibble(.init = 120, .step=1)

# sarima1_100011 = ARIMA(log(Value) ~ 1 + pdq(1,0,0) + PDQ(0,1,1))
# sarima1_100012 = ARIMA(log(Value) ~ 1 + pdq(1,0,0) + PDQ(0,1,2))
# sarima1_100111 = ARIMA(log(Value) ~ 1 + pdq(1,0,0) + PDQ(1,1,1))
# sarima1_200012 = ARIMA(log(Value) ~ 1 + pdq(2,0,0) + PDQ(0,1,2))
# sarima1_210212 = ARIMA(log(Value) ~ 1 + pdq(2,1,0) + PDQ(2,1,2))
# sarima0_700116 = ARIMA(log(Value) ~ 0 + pdq(7,0,0) + PDQ(1,1,6))
# sarima0_700213 = ARIMA(log(Value) ~ 0 + pdq(7,0,0) + PDQ(2,1,3))
# sarima0_700316 = ARIMA(log(Value) ~ 0 + pdq(7,0,0) + PDQ(3,1,6))

columns <- c('models', 'model_rmsse', 'model_mape', 'model_crsp', 'model_mae')

results <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(results) <- columns

###############################     ARIMAS     #################################
############################### sarima1_100011 #################################

data_stretch |>
  model(sarima1_100011 = ARIMA(log(Value) ~ 1 + pdq(1,0,0) + PDQ(0,1,1))) |>
  forecast(h=120) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(
    response = 'Value',
    distribution = Value) |>
  accuracy(
    data_training,
    list(RMSSE=RMSSE, MAPE=MAPE, CRPS=CRPS, MAE=MAE),
    by=c('Month', '.model')) |>
  pivot_longer(
    cols=c('RMSSE','MAPE','MAE','CRPS'),
    names_to='Measures',
    values_to='Values')|>
  filter(year(Month) < 2015) -> measures

measures |>
  filter(Measures %in% c('RMSSE', 'MAPE')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('RMSSE', 'MAPE'),
                     values=c('blue', 'orange')) + labs(title='sarima1_100011')

measures |>
  filter(Measures %in% c('MAE', 'CRPS')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('MAE', 'CRPS'),
                     values=c('red', 'purple')) + labs(title='sarima1_100011')

model_rmsse <-
  sum(
    measures |>
      filter(Measures %in% 'RMSSE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mae <-
  sum(
    measures |>
      filter(Measures %in% 'MAE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mape <-
  sum(
    measures |>
      filter(Measures %in% 'MAPE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_crps <-
  sum(
    measures |>
      filter(Measures %in% 'CRPS',!is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))
results <- rbind(
  results,
  setNames(
    as.list(c('sarima1_100011',
              format(round(model_rmsse,2), nsmall=2),
              format(round(model_mape, 2), nsmall=2),
              format(round(model_crps, 2), nsmall=2),
              format(round(model_mae , 2), nsmall=2))
    ),
    names(results)
  )
)
############################### sarima1_100012 #################################

data_stretch |>
  model(sarima1_100012 = ARIMA(log(Value) ~ 1 + pdq(1,0,0) + PDQ(0,1,2))) |>
  forecast(h=120) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(
    response = 'Value',
    distribution = Value) |>
  accuracy(
    data,
    list(RMSSE=RMSSE, MAPE=MAPE, CRPS=CRPS, MAE=MAE),
    by=c('Month', '.model')) |>
  pivot_longer(
    cols=c('RMSSE','MAPE','MAE','CRPS'),
    names_to='Measures',
    values_to='Values') -> measures

measures |>
  filter(Measures %in% c('RMSSE', 'MAPE')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('RMSSE', 'MAPE'),
                     values=c('blue', 'orange')) + labs(title='sarima1_100012')

measures |>
  filter(Measures %in% c('MAE', 'CRPS')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('MAE', 'CRPS'),
                     values=c('red', 'purple')) + labs(title='sarima1_100012')

model_rmsse <-
  sum(
    measures |>
      filter(Measures %in% 'RMSSE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mae <-
  sum(
    measures |>
      filter(Measures %in% 'MAE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mape <-
  sum(
    measures |>
      filter(Measures %in% 'MAPE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_crps <-
  sum(
    measures |>
      filter(Measures %in% 'CRPS',!is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

results <- rbind(
  results,
  setNames(
    as.list(c('sarima1_100012',
              format(round(model_rmsse, 2), nsmall=2),
              format(round(model_mape, 2), nsmall=2),
              format(round(model_crps, 2), nsmall=2),
              format(round(model_mae , 2), nsmall=2))
    ),
    names(results)
  )
)
############################### sarima1_100111 #################################

data_stretch |>
  model(sarima1_100111 = ARIMA(log(Value) ~ 1 + pdq(1,0,0) + PDQ(1,1,1))) |>
  forecast(h=120) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(
    response = 'Value',
    distribution = Value) |>
  accuracy(
    data,
    list(RMSSE=RMSSE, MAPE=MAPE, CRPS=CRPS, MAE=MAE),
    by=c('Month', '.model')) |>
  pivot_longer(
    cols=c('RMSSE','MAPE','MAE','CRPS'),
    names_to='Measures',
    values_to='Values') -> measures

measures |>
  filter(Measures %in% c('RMSSE', 'MAPE')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('RMSSE', 'MAPE'),
                     values=c('blue', 'orange')) + labs(title='sarima1_100111')

measures |>
  filter(Measures %in% c('MAE', 'CRPS')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('MAE', 'CRPS'),
                     values=c('red', 'purple')) + labs(title='sarima1_100111')

model_rmsse <-
  sum(
    measures |>
      filter(Measures %in% 'RMSSE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mae <-
  sum(
    measures |>
      filter(Measures %in% 'MAE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mape <-
  sum(
    measures |>
      filter(Measures %in% 'MAPE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_crps <-
  sum(
    measures |>
      filter(Measures %in% 'CRPS',!is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

results <- rbind(
  results,
  setNames(
    as.list(c('sarima1_100111',
              format(round(model_rmsse, 2), nsmall=2),
              format(round(model_mape, 2), nsmall=2),
              format(round(model_crps, 2), nsmall=2),
              format(round(model_mae , 2), nsmall=2))
    ),
    names(results)
  )
)
############################### sarima1_200012 #################################

data_stretch |>
  model(sarima1_200012 = ARIMA(log(Value) ~ 1 + pdq(2,0,0) + PDQ(0,1,2))) |>
  forecast(h=120) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(
    response = 'Value',
    distribution = Value) |>
  accuracy(
    data,
    list(RMSSE=RMSSE, MAPE=MAPE, CRPS=CRPS, MAE=MAE),
    by=c('Month', '.model')) |>
  pivot_longer(
    cols=c('RMSSE','MAPE','MAE','CRPS'),
    names_to='Measures',
    values_to='Values') -> measures

measures |>
  filter(Measures %in% c('RMSSE', 'MAPE')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('RMSSE', 'MAPE'),
                     values=c('blue', 'orange')) + labs(title='sarima1_200012')

measures |>
  filter(Measures %in% c('MAE', 'CRPS')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('MAE', 'CRPS'),
                     values=c('red', 'purple')) + labs(title='sarima1_200012')

model_rmsse <-
  sum(
    measures |>
      filter(Measures %in% 'RMSSE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mae <-
  sum(
    measures |>
      filter(Measures %in% 'MAE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mape <-
  sum(
    measures |>
      filter(Measures %in% 'MAPE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_crps <-
  sum(
    measures |>
      filter(Measures %in% 'CRPS',!is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))
results <- rbind(
  results,
  setNames(
    as.list(c('sarima1_200012',
              format(round(model_rmsse, 2), nsmall=2),
              format(round(model_mape, 2), nsmall=2),
              format(round(model_crps, 2), nsmall=2),
              format(round(model_mae , 2), nsmall=2))
    ),
    names(results)
  )
)
############################### sarima1_210212 #################################

data_stretch |>
  model(sarima1_210212 = ARIMA(log(Value) ~ 1 + pdq(2,1,0) + PDQ(2,1,2))) |>
  forecast(h=120) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(
    response = 'Value',
    distribution = Value) |>
  accuracy(
    data,
    list(RMSSE=RMSSE, MAPE=MAPE, CRPS=CRPS, MAE=MAE),
    by=c('Month', '.model')) |>
  pivot_longer(
    cols=c('RMSSE','MAPE','MAE','CRPS'),
    names_to='Measures',
    values_to='Values') -> measures

measures |>
  filter(Measures %in% c('RMSSE', 'MAPE')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('RMSSE', 'MAPE'),
                     values=c('blue', 'orange')) + labs(title='sarima1_210212')

measures |>
  filter(Measures %in% c('MAE', 'CRPS')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('MAE', 'CRPS'),
                     values=c('red', 'purple')) + labs(title='sarima1_210212')

model_rmsse <-
  sum(
    measures |>
      filter(Measures %in% 'RMSSE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mae <-
  sum(
    measures |>
      filter(Measures %in% 'MAE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mape <-
  sum(
    measures |>
      filter(Measures %in% 'MAPE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_crps <-
  sum(
    measures |>
      filter(Measures %in% 'CRPS',!is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

results <- rbind(
  results,
  setNames(
    as.list(c('sarima1_210212',
              format(round(model_rmsse, 2), nsmall=2),
              format(round(model_mape, 2), nsmall=2),
              format(round(model_crps, 2), nsmall=2),
              format(round(model_mae , 2), nsmall=2))
    ),
    names(results)
  )
)
############################### sarima0_700116 #################################

data_stretch |>
  model(sarima0_700116 = ARIMA(log(Value) ~ 1 + pdq(7,0,0) + PDQ(1,1,6))) |>
  forecast(h=120) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(
    response = 'Value',
    distribution = Value) |>
  accuracy(
    data,
    list(RMSSE=RMSSE, MAPE=MAPE, CRPS=CRPS, MAE=MAE),
    by=c('Month', '.model')) |>
  pivot_longer(
    cols=c('RMSSE','MAPE','MAE','CRPS'),
    names_to='Measures',
    values_to='Values') -> measures

measures |>
  filter(Measures %in% c('RMSSE', 'MAPE')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('RMSSE', 'MAPE'),
                     values=c('blue', 'orange')) + labs(title='sarima0_700116')

measures |>
  filter(Measures %in% c('MAE', 'CRPS')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('MAE', 'CRPS'),
                     values=c('red', 'purple')) + labs(title='sarima0_700116')

model_rmsse <-
  sum(
    measures |>
      filter(Measures %in% 'RMSSE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mae <-
  sum(
    measures |>
      filter(Measures %in% 'MAE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mape <-
  sum(
    measures |>
      filter(Measures %in% 'MAPE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_crps <-
  sum(
    measures |>
      filter(Measures %in% 'CRPS',!is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))
results <- rbind(
  results,
  setNames(
    as.list(c('sarima0_700116',
              format(round(model_rmsse, 2), nsmall=2),
              format(round(model_mape, 2), nsmall=2),
              format(round(model_crps, 2), nsmall=2),
              format(round(model_mae , 2), nsmall=2))
    ),
    names(results)
  )
)
############################### sarima0_700213 #################################

data_stretch |>
  model(sarima0_700213 = ARIMA(log(Value) ~ 1 + pdq(7,0,0) + PDQ(2,1,3))) |>
  forecast(h=120) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(
    response = 'Value',
    distribution = Value) |>
  accuracy(
    data,
    list(RMSSE=RMSSE, MAPE=MAPE, CRPS=CRPS, MAE=MAE),
    by=c('Month', '.model')) |>
  pivot_longer(
    cols=c('RMSSE','MAPE','MAE','CRPS'),
    names_to='Measures',
    values_to='Values') -> measures

measures |>
  filter(Measures %in% c('RMSSE', 'MAPE')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('RMSSE', 'MAPE'),
                     values=c('blue', 'orange')) + labs(title='sarima0_700213')

measures |>
  filter(Measures %in% c('MAE', 'CRPS')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('MAE', 'CRPS'),
                     values=c('red', 'purple')) + labs(title='sarima0_700213')

model_rmsse <-
  sum(
    measures |>
      filter(Measures %in% 'RMSSE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mae <-
  sum(
    measures |>
      filter(Measures %in% 'MAE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mape <-
  sum(
    measures |>
      filter(Measures %in% 'MAPE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_crps <-
  sum(
    measures |>
      filter(Measures %in% 'CRPS',!is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))
results <- rbind(
  results,
  setNames(
    as.list(c('sarima0_700213',
              format(round(model_rmsse, 2), nsmall=2),
              format(round(model_mape, 2), nsmall=2),
              format(round(model_crps, 2), nsmall=2),
              format(round(model_mae , 2), nsmall=2))
    ),
    names(results)
  )
)
############################### sarima0_700316 #################################

data_stretch |>
  model(sarima0_700316 = ARIMA(log(Value) ~ 1 + pdq(7,0,0) + PDQ(3,1,6))) |>
  forecast(h=120) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(
    response = 'Value',
    distribution = Value) |>
  accuracy(
    data_training,
    list(RMSSE=RMSSE, MAPE=MAPE, CRPS=CRPS, MAE=MAE),
    by=c('Month', '.model')) |>
  pivot_longer(
    cols=c('RMSSE','MAPE','MAE','CRPS'),
    names_to='Measures',
    values_to='Values') -> measures

measures |>
  filter(Measures %in% c('RMSSE', 'MAPE')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('RMSSE', 'MAPE'),
                     values=c('blue', 'orange')) + labs(title='sarima0_700316')

measures |>
  filter(Measures %in% c('MAE', 'CRPS')) |>
  ggplot(aes(x=Month, y=Values)) +
  geom_line(aes(color=Measures)) +
  scale_color_manual(name='Values', labels=c('MAE', 'CRPS'),
                     values=c('red', 'purple')) + labs(title='sarima0_700316')

model_rmsse <-
  sum(
    measures |>
      filter(Measures %in% 'RMSSE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mae <-
  sum(
    measures |>
      filter(Measures %in% 'MAE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_mape <-
  sum(
    measures |>
      filter(Measures %in% 'MAPE', !is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

model_crps <-
  sum(
    measures |>
      filter(Measures %in% 'CRPS',!is.na(Values)) |>
      select(Values)
  ) / length(!is.na(measures$Values))

results <- rbind(
  results,
  setNames(
    as.list(c('sarima0_700316',
              format(round(model_rmsse, 2), nsmall=2),
              format(round(model_mape, 2), nsmall=2),
              format(round(model_crps, 2), nsmall=2),
              format(round(model_mae , 2), nsmall=2))
    ),
    names(results)
  )
)
############################### 10. STRETCH METHOD ACCURACY RESULTS ############

results

#           models model_rmsse model_mape   model_crsp    model_mae
# 1 sarima1_100011        0.84       6.02 101013415.28 145364340.87
# 2 sarima1_100012        0.92       8.13 172965200.36 223406274.00
# 3 sarima1_100111        0.93       8.24 176689808.87 227148591.61
# 4 sarima1_200012        0.93       8.14 169009882.77 224690484.16
# 5 sarima1_210212        0.82       6.47 158109607.83 192078730.13
# 6 sarima0_700116        0.43       3.08  70588543.01 100688344.59
# 7 sarima0_700213        0.70       5.81 109405837.84 160978126.07
# 8 sarima0_700316         NaN        NaN          NaN          NaN

# Note that: model sarima0_700316 was unable to generate forecasts from approx.
# 75% of the time series, due to the larger number of coefficients making the
# calculation of confidence intervals impossible.

# With the results presented above we now decide to keep:
    # 1 sarima1_100011
    # 5 sarima1_210212
    # 6 sarima0_700116
    # 8 sarima0_700316

################################ 11. FORECASTING ###############################

# We now forecast with the selected models, onto the held out test set.

fc_arima <- data_training |>
  model(
    sarima1_100011 = ARIMA(log(Value) ~ 1 + pdq(1,0,0) + PDQ(0,1,1)),
    sarima1_210212 = ARIMA(log(Value) ~ 1 + pdq(2,1,0) + PDQ(2,1,2)),
    sarima0_700116 = ARIMA(log(Value) ~ 0 + pdq(7,0,0) + PDQ(1,1,6)),
    sarima0_700316 = ARIMA(log(Value) ~ 0 + pdq(7,0,0) + PDQ(3,1,6))
  )

fc_arima |>
  forecast(h=98) |>
  accuracy(data_test, list(RMSE=RMSE, MAPE=MAPE, CRPS=CRPS, MAE=MAE)) -> accuracy

accuracy$skill <- (
  fc_arima |>
    forecast(h=98) |>
    accuracy(data_test, list(skill_score=skill_score(CRPS)))
)$skill_score

for (model in colnames(fc_arima)) {

  print(
    fc_arima[model] |>
      forecast(h=98) |>
      autoplot(data) +
      labs(y='Value', title=model)
  )

}

############################ 12. TEST ACCURACY RESULTS #########################

accuracy

# .model         .type       RMSE  MAPE       CRPS        MAE
# <chr>          <chr>      <dbl> <dbl>      <dbl>      <dbl>
# 1 sarima0_700116 Test  379003248.  7.74 291879986. 261289924.
# 2 sarima0_700316 Test  349788942.  6.93 282396233. 219694236.
# 3 sarima1_100011 Test  450466538.  8.60 223570437. 275889857.
# 4 sarima1_210212 Test  504566227.  9.97 334276246. 331122439.

# According to Hyndman best is to choose the model with the lowest RMSE which is
# 1_700316. Also it must be said that, 700116, shows decent quantitative figures,
# but on visual inspection looks absolutely abhorrent.

# As was discussed  previously, the plots for these metrics show a clear
# trade-off, and whilst the more specified model is able to predict the near
# future with very good precision, it sacrifices forecastability in the long-run
# due to its high variance.

# In that sense, the 1_100011 model was vastly superior, and obviously so, since
# the further one attempts to foresee into the future, the broader the movements
# have to be, and therefore, it is the simple model that generalizes the big
# trends best.

# Stretch cross-validation provided us with great confidence in the
# generalization power of both our models of choice: 1_100011 and 700316.

################################ 12. ENSEMBLE ##################################

# As such, and under ceteris paribus, we ideally like to test a combination of
# these two models. For that we will use the combination_model function, that
# takes two models and finds their combination that maximizes accuracy.

# Unfortunately, it is not supporting the log transformation, directly, we must
# pass the transformed variable, which loses the transform method call, that
# transforms the log-likelihood space of probabilities into a likelihood space
# for the forecast of value.

# Thus, we are forced to forecast the log-transform, meaning that with our
# ensemble we can only refer, to a log-level specification, seeing that these
# are auto-regressive models, this is interpreted as the % change of exports
# per unit time (Month).

#Creating the log variable
data_training |>
  mutate(log_value = log(Value)) -> data_training

data |>
  mutate(log_value = log(Value)) -> data

# Creating the combination

data_training %>%
  model(
    ensemble =
      combination_model(
        ARIMA(log_value ~ 1 + pdq(1,0,0) + PDQ(0,1,1)),
        ARIMA(log_value ~ 0 + pdq(7,0,0) + PDQ(3,1,6)),
        weights = "inv_var")
      ) |>
  forecast(h=98) -> ensemble

ensemble |>
  autoplot(data)

ensemble |>
  accuracy(data, list(RMSE=RMSE, MAPE=MAPE, CRPS=CRPS, MAE=MAE))

ensemble |>
  autoplot(data) + labs(y='% Exports Change mOm')


# And below the results

# .model  .type      ME    RMSE    MAE    MPE      MAPE  MASE RMSSE  ACF1
#   <chr>  <chr>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl> <dbl> <dbl> <dbl>
# 1 cmbn1  Test  -0.0272   0.109 0.0633  -0.126   0.289 0.680 0.925 0.725
# 1 one    Test  -0.0551   0.127 0.0717  -0.253   0.328 0.771 1.08  0.745
# 2 two    Test   0.000687 0.106 0.0649  0.000350 0.296 0.697 0.897 0.723

########################### 13. BENCHMARKS #####################################

# We will now compare our results of our ARIMA forecast with some other methods

# ETS model
fit_ets <- data_training |>
  model(
    ets_ann = ETS(log(Value) ~ error('A') + trend('N') + season('N')),
    ets_aan = ETS(log(Value) ~ error('A') + trend('A') + season('N')),
    ets_aaa = ETS(log(Value) ~ error('A') + trend('A') + season('A')),
    ets_aam = ETS(log(Value) ~ error('A') + trend('A') + season('M')),
    ets_mam = ETS(log(Value) ~ error('M') + trend('A') + season('M')),
    ets_aada = ETS(log(Value) ~ error('A') + trend('Ad') + season('A'))
  )

fit_ets |>
  glance()

fit_naive <- data_training |>
  model(
    mean = MEAN(log(Value)),
    naive = NAIVE(log(Value)),
    snaive = SNAIVE(log(Value)),
    drift = RW(log(Value) ~ drift())
  )

# not possible to get AIC for these methods (no log_lik)

fit_stl <- data_training |>
  model(
    stl = decomposition_model(
      STL(log(Value) ~ season(window=13)),
      NAIVE(season_adjust)
    )
  )

dcmp <- data_training |>
  model(X_13ARIMA_SEATS(Value ~ transform(`function` = "none") + x11(mode = "add"))) |>
  components() |>
  select(-.model)


dcmp |>
  model(
    decomposition_model(
      X_13ARIMA_SEATS(Value ~ transform(`function` = "none") + x11(mode = "add")),
      SNAIVE(season_adjust)
    )
  ) |>
  forecast(h=87) |>
  autoplot(dcmp) +
  labs(y = "Log exports",
       title = "Portuguese Exports")

# These models were found to be very fidgety and not very malleable, opinions
# and documentation available online points generally to the notion that it
# provides very good results when specified correctly.

############################### 13.6. FORECASTING ##############################

fc_naive <- data_training |>
  model(
    ets_aada = ETS(log(Value) ~ error('A') + trend('Ad') + season('A')),
    mean = MEAN(log(Value)),
    naive = NAIVE(log(Value)),
    snaive = SNAIVE(log(Value)),
    RW(log(Value) ~ drift()),
    stl = decomposition_model(
      STL(log(Value) ~ season(window=13)), NAIVE(season_adjust))
  )

fc_naive |>
  forecast(h=98) |>
  accuracy(data_test, list(RMSE=RMSE, MAE=MAE, MAPE=MAPE, CRPS=CRPS)) -> naive_accuracy

naive_accuracy$skill <- (
  fc_naive |>
    forecast(h=98) |>
    accuracy(data, list(skill_score=skill_score(CRPS)))
)$skill_score

for (model in colnames(fc_naive)) {

  print(
    fc_naive[model] |>
      forecast(h=98) |>
      autoplot(data) +
      labs(y='Value', title=model)
  )
}

########################### 13.7. TEST ACCURACY RESULTS ########################

naive_accuracy

#   .model                   .type        RMSE  MAPE        CRPS  skill
# 1 RW(log(Value) ~ drift()) Test  5225552144.  111. 1994144020.  -2.46
# 2 ets_aada                 Test   792237088.  16.7  454967722.  0.210
# 3 mean                     Test  1741031546.  43.4 1294075222.  -1.25
# 4 naive                    Test  1982756521.  45.8 1287970988.  -1.24
# 5 snaive                   Test   853265777.  18.5  498724572.  0.134
# 6 stl                      Test   614361898.  12.6  454464963.  0.211

