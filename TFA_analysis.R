#TFA Script



library(tidyverse)
library(lubridate)



universal_function <-
  function(filelist, pivot = TRUE, startdate, enddate) {
    sensor_1 <- list.files(filelist, full.names = TRUE)
    
    read_df <- function(datasurce) {
      inone <- read.csv2(datasurce)
      readi <- data.frame(Datum = inone$Messzeitpunkt,
                          Temperatur = inone$Temperatur.2)
      return(readi)
    }
    
    df <- NULL
    for (sensor in sensor_1) {
      set_one <-  read_df(sensor)
      df <- rbind(df, data.frame(set_one))
    }
    
    df$date_new <- as.Date(df$Datum, format = "%d.%m.%Y")
    
    final <- df %>% subset(date_new < enddate) %>%
      subset(date_new > startdate) %>%
      group_by(Tag = day(date_new)) %>%
      summarise(
        Mittel =  round(mean(Temperatur, na.rm = TRUE), digits = 1),
        Tmax = max(Temperatur),
        Tmin = min(Temperatur)
      )
    if (pivot) {
      plt <- pivot_longer(final,-"Tag",
                          names_to = "Treatment",
                          values_to = "Temperature")
      return(list(final, plt))
    }
    else{
      return(final)
    }
  }

universal_function("JULI_1",
                   pivot = TRUE,
                   startdate = "2022-06-30",
                   enddate = "2022-08-01")
