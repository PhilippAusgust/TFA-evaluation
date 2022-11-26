library(tibble)
library(tidyverse)

# Here is a classification function 


make_classes <- function(data, scale_) {
  data <- as_tibble(data)
  min = min(data) - 1
  max = max(data)
  sacle = scale_
  end = round(((max - min) / sacle))
  
  data[, "class"] <- NA
  
  for (i in seq(1, nrow(data[, 1]))) {
    for (j in seq(0, (end - 1))) {
      if (data[, 1][i,] > (min + end * sacle)) {
        data[, "class"][i,] =  paste((min + end * sacle), " >")
        break
      }
      
      if (data[, 1][i,] >= (min + (j * sacle))  &&
          data[, 1][i,] <= ((min + sacle) + (j * sacle))) {
        data[, "class"][i,] = paste((min + (j * sacle)), "-",
                                    ((min + sacle) + (j * sacle)))
        break
      }
    }
  }
  return(list(data, 
              unique(data[,"class"])))
}
