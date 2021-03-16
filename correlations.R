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


#Least square method assessment
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

#Monte Carlo simulation
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
lse %>% ggplot(aes(beta_0)) + geom_histogram()
lse %>% ggplot(aes(beta_1)) + geom_histogram()

##Assessment 2.3
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

galton %>% group_by(pair) %>% summarise(n=n())