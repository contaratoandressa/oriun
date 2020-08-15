#### SVM functions ####


# memory and packges
rm(list=ls())
install.load::install_load("PortfolioAnalytics", "quantmod", "nnet", "caret", "zoo", "PerformanceAnalytics", "plotly", "dplyr","readxl","writexl","tseries", "moments", "forecast", "polynom", "rugarch", "xts","SkewHyperbolic", "rgl")

# source
path = "/home/andressa/dev/workspace/oriun/functions_aux/"
setwd(path)

source("HGbib_atz2.R")
source("ajud2.R")
source('functions.R')

# Dataset
prices.data = import_data()
var <- names(prices.data)

# param
percent_train = 0.8

# result
total_data(data = prices.data, var = var, percent_train = percent_train)







