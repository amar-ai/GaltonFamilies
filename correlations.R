library(ggplot2)
library(tidyverse)
library(dplyr)
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

##Calculate mean and standard deviation of heights of mothers and daughters
mean_mother <- mean(female_heights$mother)
mean_daughter <- mean(female_heights$daughter)
sd_mother <- sd(female_heights$mother)
sd_daughter <- sd(female_heights$daughter)

#Calculate correlation between mother and daughter heights
corr <- female_heights %>% summarise(cor(mother,daughter))

#Scatter plot of daughter's vs mother's height
female_heights %>% ggplot(aes(mother,daughter)) +
  geom_point(alpha=0.5)

#Compute regression line for daughter vs mother
m_1 <- corr*sd_daughter/sd_mother #Slope of regression line
b_1 <- mean_daughter - m_1*mean_mother
