
library(tidyverse)
library(lubridate)
require(reshape2)
source("TFA_analysis.R")


# prepare data 

Kalenborn = universal_function("Oktober_2022_Kalenborn",
                   pivot = TRUE,
                   startdate = "2022-09-30",
                   enddate = "2022-11-01")[[2]]

Langenfeld  = universal_function("Oktober_2022_Langenfeld",
                                            pivot = TRUE,
                                            startdate = "2022-09-30",
                                            enddate = "2022-11-01")[[2]]


# preparation for min_max

Kalenborn_val = universal_function("Oktober_2022_Kalenborn",
                               pivot = TRUE,
                               startdate = "2022-09-30",
                               enddate = "2022-11-01")[[1]]

Langenfeld_val  = universal_function("Oktober_2022_Langenfeld",
                                 pivot = TRUE,
                                 startdate = "2022-09-30",
                                 enddate = "2022-11-01")[[1]]


# pick mean(min),mean(max) and mean Kalenborn

K_max = round(mean(Kalenborn_val$Tmax), digits =2)
Kmin = round(mean(Kalenborn_val$Tmin), digits = 2)
Kmittel = round(mean(Kalenborn_val$Mittel), digits = 2)

# pick mean(min),mean(max) and mean Langenfeld
L_max = round(mean(Langenfeld_val$Tmax), digits =2)
Lmin = round(mean(Langenfeld_val$Tmin), digits = 2)
Lmittel = round(mean(Langenfeld_val$Mittel), digits = 2)



# prepare df 
Kalenborn$Ort = rep(paste0("Kalenborn-Scheuern"," ","Tmin ="," ",Kmin," ", "Tmax ="," ",K_max," ", "Tmean =", " ",Kmittel), nrow(Kalenborn[, 1]))
Langenfeld$Ort = rep(paste0("Langenfeld"," ","Tmin ="," ",Lmin," ", "Tmax ="," ",L_max," ", "Tmean =", " ",Lmittel), nrow(Langenfeld[, 1]))
df = rbind(Langenfeld,Kalenborn)

# visualization 



#dir.create("October_2022_result")
 
ggplot(data = df, aes(x = df$Tag, y = df$Temperature, groupe=Ort))+
  geom_point(aes(color=Treatment))+
  geom_line(aes(colour=Treatment))+
  geom_text(aes(label=Temperature), vjust=-0.3, size=3.5)+
  #facet_grid(cols=vars(Ort))+
  facet_wrap(~Ort, ncol = 1)+
  theme_gray(base_size = 15)+
  labs(x="Tag", y = "Temperatur")+
  ggtitle("Oktober 2022")+
  scale_x_continuous(breaks = seq(1,31,1))
  #ggsave(file="October_2022_result/Oktober_2022.pdf", units ="cm",width=50, height=30, dpi=2000)
  



