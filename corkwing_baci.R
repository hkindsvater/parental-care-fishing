install.packages("tidyverse")
library(tidyverse)
wrasse_data <- read.csv("~/Documents/slot_limits/corkwingAbund.csv")

head(wrasse_data)

  sub1 <- wrasse_data %>%
      filter(Stratum.SampleUnit=="area1a")  
          
  
  
wrasse2<-wrasse_data %>%
  mutate(fishedarea = ifelse(Stratum.SampleUnit=="area1a" | Stratum.SampleUnit=="area1b", TRUE, FALSE) )  %>%

      mutate(binary_F = ifelse(Year < 2010, FALSE, TRUE) )
      
  
  
  m1 <- lm(Stratum.MeanDensity~Area_name+binary_F, data=wrasse2)
  wrasse.av <- aov(m1)
  dotplot(Stratum.MeanDensity~Area_name, data=wrasse2, xlab="Area")
  tukey.test<-TukeyHSD(wrasse.av)