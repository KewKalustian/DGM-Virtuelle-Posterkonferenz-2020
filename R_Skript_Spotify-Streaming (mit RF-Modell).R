# ################ #
# Loading packages #
# ################ #

remove(list = ls(all = T)); gc(T,T,T)

# Always reporting three significant numbers to avoid spurious accuracy
options(digits = 6)

# Generating a neat blank function to load all desired packages

load_packages <- function(){
  
# Object of desired packages

desired.packages <- c(# Tidy coding paradigm
                      "tidyverse", "magrittr", 
                      # Data import & Web-Scraping
                      "readr", "rvest",
                      # Data frames 
                      "tibble", "tsibble",
                      # Data wrangling
                      "lubridate", 
                      # Graphics
                      "ggExtra", "ggrepel", "scales", 
                      # Machine Learning 
                      "caret", "randomForest", "ranger",
                      # Forecasting
                      "forecast", "fpp2", "vars",
                      # Time series related tests / accuracy-metrics
                      "urca", "FuzzyR")

# Object which contains the desired packages as long as they are not already 
# installed. 
  
inst.packages <- desired.packages[!desired.packages %in% installed.packages()]

# Generating a for-loop.

for (packages in inst.packages) {install.packages(packages, dependencies = T)}

sapply(desired.packages, require, character = T)}

# Finally, using the just generated function to load and/or install the desired
# packages.

load_packages()

# ############################## # 
# Retrieving Data | Web-Scraping #
# ############################## # 

# For faster access to the data, I have uploaded the dataset to my 
# Github-repo so that this dataset can easily be retrieved:

spotifyR <- read_csv("https://raw.githubusercontent.com/KewKalustian/DGM-Virtuelle-Posterkonferenz-2020/master/spotifyR_charts.csv")

# ############# #
# Data Cleaning #
# ############# #

spotifyRR <- spotifyR %>% 
  
# Group-wise count of distinct songs/tracks (How many times occurs
# a distinct song/track during the whole period? A max of 546 times
# is possible as the period lasts 546 days).

add_count(title) %>% 
mutate(chart_position = as.integer(chart_position),
         
# gsub: Replacing of a matching string pattern by a 
# replacement string (i.e., we simply omit the string "by" 
# and the whitespace before the artist names).

artist = sub("by\\s", "", artist),

# Adding songIDs is useful since there could be songs/tracks 
# with the same title but from different artists.

songID = group_indices(., title, artist),

# Converting/coercing stream_count as integer-class

stream_count = as.integer(stream_count),

date = as.Date(date, "%m/%d/%Y"),

# lubridate::wday: Setting days and specifying the week 
# start on Monday based on ISO conventions (i.e., week 
# starts on Monday = 1)

weekday = wday(date, label = T, week_start = 1)) 

# ##################### #
# Plotting Streamcounts #
# ##################### #


Streams <- spotifyRR %>%
  filter(date <= as.Date("2020-03-10"))

# Calculating the bin width following the formula of Freedman & Diaconis (1981) 
# for the upcoming histogram (see Hyndman 1995)

binw <- 2 * IQR(Streams$stream_count) / nrow(Streams)^(1/3)  

# Plotting the histogram and the densities of the distributions in question.

ggplot() +
  
# Frequency Histogram   

geom_histogram(data = Streams, aes(stream_count), binwidth  = binw, 
               fill = "#4a192c", color = "#280000", alpha = 0.4) +

# Customizing the labels and break points on the x-axis

scale_x_continuous(breaks = c(min(Streams$stream_count), 
                              median(Streams$stream_count),  5e5, 1e6, 15e5,
                              max(Streams$stream_count)),
                   labels = c("37568","90850.5\n(Median)", "500K", "1M", "1.5M",
                              "~1.97M")) +

# Label numbers with SI prefixes 

scale_y_continuous(labels = label_number_si(accurarcy = NULL)) +

# For visual purposes (i.e., avoiding that high frequencies and a heavy tail 
# of high counts dominate the plot), we use here a square root-scaling of 
# the axes (not of the values). That is, it is of ample importance to note
# that we do not transform the scale statistics of our data: They are still
# the same.

coord_trans(x = "sqrt", y = "sqrt") + 

# Customizing the labels on both axes

labs(x = "\nStreaming Counts", y = "Frequency\n") +

# Layout  

theme_bw(base_size = 14) +
theme(axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      plot.margin = unit(c(.66,.33,.66,.66), "cm"))

# ############################################################################ #
# EDA_H: Stream counts during the Pandemic are higher than during Pre-Pandemic #
# ############################################################################ #

# Subsetting the data into a pre-crisis set  

spotifyR_Pre <- spotifyRR %>%
  filter(date <= as.Date("2020-03-10")) %>%
  group_by(weekday) %>% 
  summarize(Mdn_Streams = median(stream_count)) 

# Subsetting the data into a crisis set  

spotifyR_Crisis <- spotifyRR %>%
  filter(date >= as.Date("2020-03-11") & 
           date <= as.Date("2020-06-29")) %>%
  group_by(weekday) %>%
  summarize(Mdn_Streams = median(stream_count))  

# ################## #
# t-Test assumptions #
# ################## #

# Checking, whether the differences of the median stream counts between the two 
# periods in question are approximately normally distributed

# Assigning the differences to a new object. We calculate the difference 
# because if the difference between those two sets is approximately normally 
# distributed, so are the sets.

dif <- spotifyR_Crisis$Mdn_Streams - spotifyR_Pre$Mdn_Streams

# Testing for normality of the differences

normality <- shapiro.test(dif)

print(normality)

# Are outliers detectable?

boxplot.stats(spotifyR_Pre$Mdn_Streams)$out

boxplot.stats(spotifyR_Crisis$Mdn_Streams)$out

# Checking, whether the outlier of the pre-crisis data set is 
# of "concern" (i.e., significant).

# We need to set the seed, because the Q-test interpolates (i.e, approximates) 
# the p-value from simulations (see above)

set.seed(1987, sample.kind = "Rounding")

outl_pre <- outliers::dixon.test(spotifyR_Pre$Mdn_Streams)
print(outl_pre)

outl_crisis <- outliers::dixon.test(spotifyR_Crisis$Mdn_Streams)
print(outl_crisis)

# As the assumption are met, we can move on to compute the t-Test. 

# Performing the t-test

t <- t.test(spotifyR_Crisis$Mdn_Streams, spotifyR_Pre$Mdn_Streams, exact = T, 
            alernative = "greater", paired = T, conf.level = .95)

print(t)

# Calculating the effect size according to the formula of Cohen (1992). Only 
# because we have got a significant value, we do not know whether this 
# particular value is of substantial relevance at all.

eff_size <- sqrt(abs(t$statistic^2 / (t$statistic^2 + t$parameter)))

print(eff_size)

# ##################### #
# EDA_H1: # Boot
# ##################### #

spotifyR_Pre_X <-  spotifyRR %>%
  filter(date <= as.Date("2020-03-10")) %>%
  group_by(date) %>%
  summarize(Mdn_Streams = median(stream_count))

#Subsetting the data into a crisis set  

spotifyR_Crisis_X <-  spotifyRR %>%
  filter(date >= as.Date("2020-03-11") & 
           date <= as.Date("2020-05-24")) %>%
  group_by(date) %>%
  summarize(Mdn_Streams = median(stream_count))

#Note that these sets are almost the same as in the previous test. However, 
#instead of grouping the median stream counts according to weekdays, we group 
#them now per date. This is useful, since we want to draw our bootstrap sample 
#from the whole empirical data.


#Defining the number of repetitions

B <- 1e4

#Sample size of the Weekdays

N <- 7

#Setting the seed

set.seed(2007, sample.kind = "Rounding")

#Bootstrapping samples of the pre-crisis period 

boot_pre <- replicate(B, 
                      
{Pre_Mdn_Streams_star <- sample(spotifyR_Pre_X$Mdn_Streams, N, replace = T)
                                median(Pre_Mdn_Streams_star)
                                })

#Setting the seed   

set.seed(1991, sample.kind = "Rounding")

#Bootstrapping samples of the crisis period

boot_crisis <- replicate(B, 

{Crisis_Mdn_Streams_star <- sample(spotifyR_Crisis_X$Mdn_Streams, N, 
                                   replace = T)
                         median(Crisis_Mdn_Streams_star)
                         })

boot_pre %<>% data.frame()
boot_crisis %<>% data.frame()

# Confidence Interval for Medians according to McGill, Larsen Tuckey (1978) & 
# Chambers, Cleveland, Kleiner, Tuckey (1983): 62

# Pre Crisis: CI for Actual Medians

median(spotifyR_Pre$Mdn_Streams) + 1.57 * IQR(spotifyR_Pre$Mdn_Streams) /
  sqrt(length(spotifyR_Pre$Mdn_Streams))

median(spotifyR_Pre$Mdn_Streams) - 1.57 * IQR(spotifyR_Pre$Mdn_Streams) / 
  sqrt(length(spotifyR_Pre$Mdn_Streams))

# Pre Crisis: CI for Bootstrapped Medians

median(boot_pre$.) + 1.57 * IQR(boot_pre$.) / sqrt(length(boot_pre$.))
median(boot_pre$.) - 1.57 * IQR(boot_pre$.) / sqrt(length(boot_pre$.))

# Pre Crisis: CI for Actual Medians

median(spotifyR_Crisis$Mdn_Streams) + 1.57 * IQR(spotifyR_Crisis$Mdn_Streams) /
       sqrt(length(spotifyR_Crisis$Mdn_Streams))

median(spotifyR_Crisis$Mdn_Streams) - 1.57 * IQR(spotifyR_Crisis$Mdn_Streams) / 
       sqrt(length(spotifyR_Crisis$Mdn_Streams))

# Crisis: CI for Bootstrapped Medians

median(boot_crisis$.) + 1.57 * IQR(boot_crisis$.) / sqrt(length(boot_crisis$.))
median(boot_crisis$.) - 1.57 * IQR(boot_crisis$.) / sqrt(length(boot_crisis$.))

# ########################################## #
# EDA_H1: # Bringing all together | Plotting #
# ########################################## #

# Plotting the hybrid graph  

spotifyR_Pre %>%
  
ggplot() +

geom_point(aes(weekday, Mdn_Streams, 
               color = "Pre-Pandemic (01.01.19—10.03.20)")) +

geom_line(aes(weekday, Mdn_Streams, group = 1,
              color = "Pre-Pandemic (01.01.19—10.03.20)"),
          size = .75, show.legend = F) +

geom_point(data = spotifyR_Crisis, aes(weekday, Mdn_Streams, 
                               color = "Pandemic (11.03.20—29.06.20)")) +

geom_line(data = spotifyR_Crisis, aes(weekday, Mdn_Streams, group = 1,
                               color = "Pandemic (11.03.20—29.06.20)"), 
          size = .75, show.legend = F) +

annotate("rect",xmin = 7.04, xmax = 8.5, ymin = -Inf, ymax = Inf, alpha = .3, 
         fill = "grey90") +  
geom_vline(xintercept = c(7.04, 7.09), color = "#333333", size = 0.09, 
           lty = "dashed") +

geom_label(aes(x = 4, y = 118e3, label = "Actual Samples"), color = "grey50", 
           size = 3) +   

geom_boxplot(data = boot_pre, aes(x = 7.55, y = ., 
                           color = "Pre-Pandemic (01.01.19—10.03.20)"), 
             notch = T, notchwidth = .7, width = .3, fill = "navyblue", 
             alpha = .7, outlier.shape = 8, outlier.size = 0.5, 
             show.legend = F) +

stat_summary(data = boot_pre, aes(x = 7.55, y = .), fun.y = mean, 
             geom = "point", shape = 18, size = 2, color = "yellow") +  

geom_boxplot(data = boot_crisis, aes(x = 7.95, y = ., 
                              color = "Pandemic (11.03.20—29.06.20)"), 
             notch = T, notchwidth = .7, width = .3, fill = "darkred", 
             alpha = .7, outlier.shape = 8,  outlier.size = 0.5, 
             show.legend = F) +

stat_summary(data = boot_crisis, aes(x = 7.95, y = .), fun.y = mean, 
             geom = "point", shape = 18, size = 2, color = "yellow") + 

geom_label(aes(x = 7.8, y = 118e3, label = "Bootstrapped Samples"), 
           color = "grey50", size = 3) +    

annotate("rect", xmin = -0.5, xmax = 0.96, ymin = -Inf, ymax = Inf, 
         alpha = 0.3, fill = "grey90") +  
geom_vline(xintercept = c(0.91,0.96), color = "#333333",
           size = 0.09, lty = "dashed") +

geom_boxplot(data = spotifyR_Pre, aes(x = -0.05, y = Mdn_Streams, 
                               color = "Pre-Pandemic (01.01.19—10.03.20)"), 
             notch = T, notchwidth = .7, width = .3, fill = "navyblue",
             alpha = .7, outlier.shape = 8, outlier.size = 0.5, 
             show.legend = F) +

stat_summary(data = spotifyR_Pre, aes(x = -0.05, y = Mdn_Streams),
             fun.y = mean, geom = "point", shape = 18, size = 2, 
             color = "yellow") +  

geom_boxplot(data = spotifyR_Crisis, aes(x = 0.45, y = Mdn_Streams, 
                                  color = "Pandemic (11.03.20—29.06.20)"), 
             notch = T, notchwidth = .7, width = .3, fill = "darkred", 
             alpha = .7, outlier.shape = 8, outlier.size = 0.5, 
             show.legend = F) +
stat_summary(data = spotifyR_Crisis, aes(x = 0.45, y = Mdn_Streams), 
             fun.y = mean, geom = "point", shape = 18, size = 2, 
             color = "yellow") + 

geom_label(aes(x = 0.2, y = 118e3, label = "Actual Samples"), 
           color = "grey50", size = 3) +    

scale_color_manual(name = "Legend:", values = c("darkred", "navyblue")) +
scale_fill_manual(name = "Legend:", values = c("navyblue", "darkred")) +

ylim(NA, 120e3) +  

labs(x = "\nWeekdays", y = "Streaming Counts per Weekday\n(Median)") +

scale_y_continuous(labels = label_number_si(accuracy = NULL)) +
guides(color = guide_legend(override.aes = list(shape = 15, size = 4))) +

theme_bw(base_size = 14) +
theme(axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.text.x = element_text(vjust = 0.5),
      legend.position = "top",
      legend.key = element_rect(color = "white"),
      legend.background = element_rect(fill = "white",
                                       linetype = "solid",
                                       color = "grey90"),
      plot.margin = unit(c(.66,.33,.66,.33), "cm"))


# ########################### #
# Towards a Predictions Model #
# ########################### #

spotifyRRR <- spotifyRR %>% 
  group_by(date) %>%
  summarize(MdnStreams = median(stream_count)) 

#1. From a data.frame into a tibble
spotifyR_tib <- tibble(Date = as.Date(spotifyRRR$date),
                       Actual_Stream_Counts = spotifyRRR$MdnStreams)

#2. From a tibble into a tsibble
spotifyR_tsib <- as_tsibble(spotifyR_tib)

#3. From a tsibble into a daily time series, starting on the 1. January 2019
spotifyR_TS <- as.ts(spotifyR_tsib, frequency = 365, start = c(2019,1))

# ############## #
# Stationarizing #
# ############## #

#Note the end-argument in the window-function. The 10. March is day 70 of the 
#year 2020

spotifyR_TS_station <- window(spotifyR_TS, end = c(2020, 70)) 

#Are the data stationary? If not: How many times do the data need to be 
#differenced?

ndif <- ndiffs(spotifyR_TS_station)
print(ndif)

#Apparently, we have to difference our data only once. This appears reseasonable
#based on the inspection of connected scatter plot from above.

#For doing so, we log-transform the data, then we difference the data 
#(i.e., stabilizing the variance and the means; also known as de-trending and 
#de-seasonalizing)

spotifyR_TS_trans <- diff(log(spotifyR_TS_station), differences = ndif)

#How many lags (i.e., repeating frequency pattern) does the period entail?

lag_selection <- vars::VARselect(spotifyR_TS_trans)

print(lag_selection$selection)

# Lag of 7 seems reasonable since in the connected scatter plot a weeakly 
# pattern is observable: 4-5 spikes per month (= 4-5 pattern of 7 days)  

#Checking whether "spotifyR_TS_trans" has a unit root  

spotifyR_TS_trans %>%
  ur.df(lags = 7) %>%
  summary() 

spotifyR_TS_trans %>%
  ur.kpss(use.lag = 7) %>%
  summary() 

# We have 7 lags (i.e., 7 time units (here: days) have to pass until the same 
# pattern starts to repeat))

lags <- 7

# We estimate the median stream counts of 111 days (that’s the period or horizon 
# of the COVID-19-Crisis of interest); start and end day are included.

horizon <- 111

spotifyR_TS_trans_embed <- embed(spotifyR_TS_trans,lags + 1) 

# ############## #
# Data splitting #
# ############## #

# Regressand (aka: target, label, dependent variable)

y_train <-  spotifyR_TS_trans_embed[, 1] 

# Regressor (aka: input, independent variable)

X_train <- spotifyR_TS_trans_embed[, -1] 

# The actual test set with those suggested lags as validation set 

X_valid <- spotifyR_TS_trans_embed[nrow(spotifyR_TS_trans_embed), c(1:lags)] 

# Now we assign the actual final data set that we want to estimate with our 
# model. In our case: The time series between 11. March 20 (day 71 of the year 
# 2020) and 29.June 20 (day 181 of the year 2020); this is the examined period 
# of the COVID-19 pandemic.

y_test <- window(spotifyR_TS, start = c(2020, 71), end = c(2020, 181))

# ################# #
# Cross—validations #
# ################# #

# We hold back the last observation

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# RANDOM FOREST: Hyperparameter tuning via cross-validation

#We define the number of predictors in the mtry-object. This is the number 
#of randomly chosen splits at each tree. According to the suggestion of 
#Breiman and Cutler (2018) we should divide the predictors by 3 for 
#regression-approaches.

rf_tune_grid <- expand.grid(mtry = floor(col(X_train) / 3), 
                            splitrule = "extratrees", min.node.size = 5)

set.seed(61, sample.kind = "Rounding")

rf_kfold_cv <- train(data.frame(X_train), y_train, method = "ranger", 
                     trControl = train_control, tuneGrid = rf_tune_grid)

plot(rf_kfold_cv)
print(rf_kfold_cv$bestTune[,1])


# We see that the kNN Model performs just slightly better than the RF-Model. 
# Hence, we should compute both models.

# ########### #
# PREDICTIONS #
# ########### #

# Now, we save each estimate in a blank object (like a container that 
# gets filled) 
rf_estimates <- numeric(horizon)

set.seed(1, sample.kind = "Rounding")

# For-loop | Random Forest-Predictions
for (i in 1:horizon) {

set.seed(1857, sample.kind = "Rounding")
  
rf_mod <- randomForest(X_train, y_train, mtry = rf_kfold_cv$bestTune[,1], 
                       nodesize = 5, replace = T, splitrule = "extratrees")
  
# Here we fill the mentioned container with the predicted values

rf_estimates[i] <- predict(rf_mod, X_valid) 
  
#Here we 
y_train <- y_train[-1] 
  
X_train <- X_train[-nrow(X_train),]}

# Retransforming the estimates by taking the "anti-logs" (i.e., computing the
# exponents of every estimate)

rf_exponents_of_estimates <- exp(cumsum(rf_estimates))

# Extracting the last observation from the time series

rf_last_observation <- as.vector(tail(spotifyR_TS_station, 1))

# Getting the final values by retransforming them

rf_retransformed_estimates <- rf_last_observation * rf_exponents_of_estimates

# Converting them into time series-format

rf_y_pred <- ts(rf_retransformed_estimates, start = c(2020, 71), 
                frequency = 365)

# As we get predictions of the actual stream counts with our trained model, we 
# should evaluate the performance of these estimates by comparing it with a 
# benchmark model to be sure that our model and the invested workload are 
# rewarding

set.seed(7301, sample.kind = "Rounding")

benchmark <- snaive(spotifyR_TS_station, h = horizon)

#Now, we have approached the most thrilling part of our endeavor. We will now 
#see the error metrics. This is a very crucial part with regard to our 
#hypothesis. For if the error values are bad for our model, we have failed to 
#predict the stream counts.


# ####### #
# Results #
# ####### #

fuzzyr.accuracy(rf_y_pred, y_test, benchmark$mean)

R_squared <- 1 - (sum((y_test - rf_y_pred)^2) / sum((y_test - mean(y_test))^2))
print(R_squared)

adj.r.squared = 1 - (1 - R_squared) * ((111 - 1)/(111 - 7 - 1))
print(adj.r.squared)

# Bringing the original data and the estimates together

spotifyR_tib_star <- spotifyR_tib %>% 
  mutate(RF_Estimates = c(rep(NA, length(spotifyR_TS_station)), rf_y_pred),
         SN_Estimates = c(rep(NA, length(spotifyR_TS_station)), benchmark$mean))

# Extracting only the crisis-period

test <-  tail(spotifyR_tib_star, horizon)


# As the assumption are met, we can move on to compute the t-Test. 

# Performing the t-test
resids <- test$Actual_Stream_Counts - test$RF_Estimates
shapiro.test(resids)

t <- t.test(test$Actual_Stream_Counts, test$RF_Estimates, paired = T, 
            conf.level = .95)
print(t)

# Not significant: Thus, the central tendencies of the
# predictions and actual values do not differ.

ggplot(data = test) +

geom_violin(aes(x = "Random Forest-estimates", y = RF_Estimates), 
            fill = "darkgreen", alpha = 0.4) +
  
geom_boxplot(aes(x = "Random Forest-estimates", y = RF_Estimates), 
             notch = T, color = "darkgreen", alpha = 0.4, width = 0.2) +

geom_violin(aes(x = "Actual Stream Counts\nduring COVID-19 crisis", 
                y = Actual_Stream_Counts), fill = "darkgrey", alpha = 0.5) +

geom_boxplot(aes(x = "Actual Stream Counts\nduring COVID-19 crisis", 
                 y = Actual_Stream_Counts), notch = T, color = "darkgrey", 
                 alpha = 0.5, width = 0.2) +

geom_hline(yintercept = median(test$Actual_Stream_Counts), color = "red", 
           size = 0.3, lty = "dashed") +

scale_y_continuous(labels = label_number_si(accuracy = NULL)) +  

labs(x = "", y = "Stream Counts per Day (Median)\n") +

theme_bw(base_size = 14) +
  
theme(axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      plot.margin = unit(c(.66,.33,.66,.33), "cm"))

# Since the residuals rely on the medians and normal distributed, we 
# cannot calculate the prediction intervals for each point prediction error. 
# So we run a Monte Carlo simulation as above and use the apply-function to 
# calculate the standard deviation for each of these bootstrapped residual 
# values for calculating the prediction intervals (PI) of the 
# Random Forest-estimates.

# Repetitions

b <- 10e4

#Sample size
n <- 111

set.seed(9579, sample.kind = "Rounding")

boot_SD_resids <- replicate(b, {boot_resids <- sample(resids, n, replace = T)
                            sd(boot_resids)})

SD_star <- mean(boot_SD_resids)

spotifyR_tib_star %<>% 

mutate(RF_E_lo.95 = RF_Estimates  - 1.96 * SD_star,
       RF_E_hi.95 = RF_Estimates  + 1.96 * SD_star)

# Creating a color-object

cols <- c("Actual Stream Counts (Median)" = "black", 
          "Random Forest-estimates\nwithin 95 % PI" = "darkgreen",
          "Seasonal-naive Model" = "deeppink")

#Plotting code

spotifyR_tib_star %>%
  
filter(Date >= as.Date("2020-03-11")) %>% 

ggplot(aes(x = Date)) +

geom_point(aes(y = Actual_Stream_Counts, 
               color = "Actual Stream Counts (Median)"),
           size = .75) +
geom_line(aes(y = Actual_Stream_Counts, 
              color = "Actual Stream Counts (Median)"), 
          lty = "solid") +

geom_point(aes(y = RF_Estimates, 
               color = "Random Forest-estimates\nwithin 95 % PI"), size = .75) + 
  
geom_line(aes(y = RF_Estimates, 
              color = "Random Forest-estimates\nwithin 95 % PI"), 
          lty = "solid") +
  
geom_point(aes(y = SN_Estimates, 
               color = "Seasonal-naive Model"),
           size = .75) +
geom_line(aes(y = SN_Estimates, 
              color = "Seasonal-naive Model"), 
          lty = "solid") +

geom_ribbon(aes(y = RF_Estimates, ymin = RF_E_lo.95, ymax = RF_E_hi.95), 
            fill = "darkgreen", alpha = 0.2) +

scale_colour_manual(name = "Legend:", values = cols) +
scale_fill_manual(name = "Legend:", values = cols) +

geom_vline(xintercept = as.numeric(as.Date("2020-03-11")), 
           color = "navyblue", size = 0.2, lty = "dashed") +

geom_label(aes(x = as.Date("2020-05-05"), y = 13e4, 
               label = "Test dataset\n(Pandemic)"), color = "darkred", 
           size = 4) +

annotate("rect", xmin = as.Date("2020-03-11"), xmax = as.Date("2020-06-29"), 
         ymin = -Inf, ymax = Inf, alpha = 0.04, fill = "darkred") +

scale_x_date(limits = c(as.Date("2020-03-11"), as.Date("2020-06-29")), 
             date_breaks = "7 day", date_labels = "%d.%b.%y") +

scale_y_continuous(labels = label_number_si(accuracy = NULL)) +

labs(x = "\nStreaming-Date", y = "Streaming Counts per Day\n(Median)\n") +

theme_bw(base_size = 14) +
theme(axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      legend.position = "top",
      legend.key = element_rect(color = "white"),
      legend.background = element_rect(fill = "white",
                                       linetype = "solid",
                                       color = "grey90"),
      plot.margin = unit(c(.66,.33,.66,.33), "cm"))
