# Apply make_classes function 

source("make_classes.R")

library(tibble)
library(tidyverse)
library(lubridate)

df = read.delim("example_weather_data.csv",
                sep = ",",
                header = TRUE)

df[, "class"] = make_classes(df$temperature, scale_ = 5)[[1]][, 2]


head(df)
