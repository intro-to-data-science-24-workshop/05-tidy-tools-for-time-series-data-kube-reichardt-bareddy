################################################################################
### Live session script: Tidy tools for time-series data                      ##
### Intro to Data Science Workshop 2024 @ Hertie School                       ##
### Author: Felix Kube (2024/10/25)                                           ##
################################################################################

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

# Load data and prepare basics
data <- tsibbledata::aus_livestock

# Check the data frame for which are the index and key(s)
str(data)

### ¯\_(ツ)_/¯ How many rows does the dataset have?

"YOUR CODE"

### ¯\_(ツ)_/¯ How many different time-series does the dataset consist of?
### Can you easily descibe them in a table using dplyr::tally()?

# To start analizing the data, we first explore the data and identify potential gaps
has_gaps(data) %>%
  .$.gaps # all good, let's move on

# Since the dataset is huge, we subset only Lambs and get an overview
autoplot(data %>% filter(Animal == "Lambs"))

### ¯\_(ツ)_/¯ What does this first exploratory plot imply? Try describing using your own words
print("I conclude that... Therefore, I suspect the...")

# Using STL feature extraction, we can try to figure out the Seasonality of our data
data %>%
  filter(Animal == "Lambs") %>%
  gg_season(Count, period = "year") # seems this depends on a yearly pattern

data %>%
  filter(Animal == "Lambs") %>%
  model(feasts::STL(Count ~ season(window = "periodic")))  %>%
  components() %>%
  autoplot()

# Let us compare the original data against the 'trend' plot of our STL decomposition
plot1 <- autoplot(data %>%
                    filter(Animal == "Lambs")) +
  theme(legend.position = "none")

plot2 <- data %>%
  filter(Animal == "Lambs") %>%
  model(STL(Count ~ season(window = "periodic"))) %>%
  components() %>%
  select(trend) %>%    # Select only the Trend component
  autoplot() +
  theme(legend.position = "none")

gridExtra::grid.arrange(plot1, plot2, ncol = 2)

# Let's forecast into the future for Victoria using different models
models <- data %>%
  filter(Animal == "Lambs", State == "Victoria") %>%
  model(ets = ETS(Count ~ trend()),
        arima = ARIMA(Count ~ trend())) %>%
  forecast(h = "5 years")


models %>%
  filter(.model == "ets") %>%
  autoplot(data %>% filter(Animal == "Lambs", State == "Victoria")) +
  ggtitle("ETS model")

models %>%
  filter(.model == "arima") %>%
  autoplot(data %>% filter(Animal == "Lambs", State == "Victoria")) +
  ggtitle("ARIMA model")

### ¯\_(ツ)_/¯ Which of the models seems more realistic?
print("I think the model using the...")

### With the remainder of your time, check out our learnr tutorial
### in the GitHub Workshop Classroom or read up on the models at
### https://medium.com/analytics-vidhya/time-series-forecasting-models-726f7968a2c1