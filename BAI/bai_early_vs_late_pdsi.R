# Load packages -----------------------------------------------------------

library(tidyverse)
library(lmtest)
library(car)
library(effects)

# Load data ---------------------------------------------------------------

duct_metrics <- read.csv('Data/Processed/complete_data_12.1.25.csv')

duct_metrics_1990to2019 <- duct_metrics %>% #these are trees just exposed to ambient conditions
  filter(year < 2020) %>% 
  filter(group != 'legacy')

#Average across all trees

yearly_avg_all <- duct_metrics_1990to2019 %>% 
  group_by(year) %>% 
  summarise(
    mean_bai = mean(bai_cm2, na.rm = TRUE),
    pdsi_early = mean(pdsi_early_growing_season),
    pdsi_late = mean(pdsi_late_growing_season)
  )

# Test effects of late vs early season PDSI -------------------------------

bai_pdsi_lm <- lm(mean_bai ~ pdsi_early + pdsi_late, data = yearly_avg_all) 
#model testing the effect of early or late conditions while holding the other constant

summary(bai_pdsi_lm)

linearHypothesis(bai_pdsi_lm, "pdsi_early = pdsi_late") #significant

# Test assumptions --------------------------------------------------------

plot(bai_pdsi_lm, which = 1, add.smooth = FALSE)
qqPlot(bai_pdsi_lm, which = 2)
hist(resid(bai_pdsi_lm))
acf(resid(bai_pdsi_lm), lag.max = 10)
vif(bai_pdsi_lm)

#All look okay

avPlots(bai_pdsi_lm)
plot(allEffects(bai_pdsi_lm))

