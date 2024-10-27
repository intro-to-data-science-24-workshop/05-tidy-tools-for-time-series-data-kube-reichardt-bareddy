################################################################################
### Live session script: Tidy tools for time-series data                      ##
### Intro to Data Science Workshop 2024 @ Hertie School                       ##
### Author: Felix Kube (2024/10/25)                                           ##
################################################################################

### ¯\_(ツ)_/¯ MIND THE QUESTION MAN! When you see him, you should start thinking on your own!

library(tidyverse)
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)
# library(tsibbletalk)
# library(fasster)
# library(fable.binary)
# library(fable.prophet)
library(gridExtra) # only for layout purposes in this script
library(gt) # only for readibility purposes in this script
library(ggplot2) # only needed to style plots further

# Load data and prepare basics
data <- tsibbledata::aus_livestock

# Check the data frame for which are the index and key(s)
str(data)
interval(data)

### ¯\_(ツ)_/¯ How many rows does the data set have? What does this mean for the amount of observations?
"YOUR CODE"

### ¯\_(ツ)_/¯ How many different time-series does the data set consist of?
### Can you easily describe them in a table using dplyr::tally()?
data # %>%
  # group_by_key(), group_by(), or another way

# To start analyzing the data, we first explore the data and identify potential gaps
has_gaps(data) %>%
  .$.gaps # all good, let's move on

# Since the data set is huge, we subset only Lambs and get an overview
autoplot(data %>% filter(Animal == "Lambs"))

### ¯\_(ツ)_/¯ What does this first exploratory plot imply? Try describing using your own words
print("I conclude that... Therefore, I suspect the...")

# Using STL feature extraction, we can try to figure out the seasonality of our data
data %>%
  filter(Animal == "Lambs") %>%
  feasts::gg_season(Count, period = "year")

data %>%
  filter(Animal == "Lambs") %>%
  feasts::gg_season(Count, period = "6 months")

data %>%
  filter(Animal == "Lambs") %>%
  feasts::gg_season(Count, period = "3 months")

data %>%
  filter(Animal == "Lambs") %>%
  feasts::ACF(Count) %>%
  autoplot()

data %>%
  filter(Animal == "Lambs") %>%
  model(STL = feasts::STL(Count))  %>%
  components() %>%
  autoplot()

# Let us compare the original data against the 'trend' plot of our STL decomposition
plot1 <- autoplot(data %>%
                    filter(Animal == "Lambs",
                           Month > make_yearmonth(2010, 1))
                  ) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1250000))

plot2 <- data %>%
  filter(Animal == "Lambs", Month > make_yearmonth(2010, 1)) %>%
  model(STL(Count)) %>%
  components() %>%
  select(trend) %>%    # Select only the Trend component
  autoplot() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1250000))

gridExtra::grid.arrange(plot1, plot2, ncol = 2)

# Let's forecast into the future for Victoria using different models
models <- data %>%
  filter(Animal == "Lambs") %>%
  fable::model(ets = ETS(Count ~ trend()),
               arima = ARIMA(Count ~ trend()))

forecasts <- models %>%
  fabletools::forecast(h = "5 years")

### ¯\_(ツ)_/¯ What are the classes of the created objects? What are their special properties?
?tsibble
"YOUR INVESTIGATION"

forecasts %>%
  filter(.model == "ets", State %in% c("Victoria", "Tasmania", "New South Wales")) %>%
  autoplot(data %>% filter(Animal == "Lambs", State %in% c("Victoria", "Tasmania", "New South Wales"))) +
  ggtitle("ETS model")

forecasts %>%
  filter(.model == "arima", , State %in% c("Victoria", "Tasmania", "New South Wales")) %>%
  autoplot(data %>% filter(Animal == "Lambs", , State %in% c("Victoria", "Tasmania", "New South Wales"))) +
  ggtitle("ARIMA model")

### ¯\_(ツ)_/¯ Which of the models seems more realistic?
# What are each models problems in the short and long term?
print("I think the model using the...")

# Let's confirm this by looking at model statistics
glance(models) %>%
  filter(State %in% c("Victoria", "Tasmania", "New South Wales")) %>%
  select(-ar_roots, -ma_roots) %>% # only relevant for ARIMA models. Happy if you're interested, but you can read up that on your own as we do not have time. Good luck!
  gt(groupname_col = "State") %>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = log_lik,
      rows = log_lik == max(log_lik[State == "Victoria"])
    )
  ) %>%
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(
      columns = AIC,
      rows = AIC == min(AIC[State == "Victoria"])
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightgrey"),
      cell_text(align = "center")
    ),
    locations = cells_row_groups()
  )

### With the remainder of your time, check out our learnr tutorial
### in the GitHub Workshop Classroom or read up on the models at
### https://medium.com/analytics-vidhya/time-series-forecasting-models-726f7968a2c1

### ¯\_(^^)_/¯
# Look! You made the QUESTION MAN happy!