# Load packages -----------------------------------------------------------

library(tidyverse)
library(car)

# Load data ---------------------------------------------------------------

pdsi <- read.csv('socorro_co_pdsi_1989to2025.csv')
duct_metrics <- read.csv('complete_data_12.1.25.csv')
duct_metrics_1990to2019 <- duct_metrics %>% #these are trees just exposed to ambient conditions
  filter(year < 2020) %>% 
  filter(group != 'legacy')

#Average across all trees

yearly_avg_all <- duct_metrics_1990to2019 %>% 
  group_by(year) %>% 
  summarise(
    mean_rel_duct_area = mean(rel_duct_area, na.rm = TRUE),
    pdsi_full = mean(pdsi_full_season)
  )
#New column for previous year's PDSI

yearly_avg_pdsi_1yr_lag <- yearly_avg_all %>% 
  mutate(full_pdsi_lag = lag(pdsi_full, n = 1)) 

# Add 1989 pdsi -----------------------------------------------------------

yearly_avg_pdsi_1yr_lag[1, 4] <- -2.107

# Look at data ------------------------------------------------------------

#Look at autocorrelation 
acf(yearly_avg_pdsi_1yr_lag$full_pdsi_lag, lag.max = 10) #mildly autocorrelated but not exceeding bands
acf(yearly_avg_pdsi_1yr_lag$mean_rel_duct_area, lag.max = 10) #autocorrelated

#Check normality
qqnorm(yearly_avg_all$mean_rel_duct_area); qqline(yearly_avg_all$mean_rel_duct_area) #fairly normal, some deviation at tail
hist(yearly_avg_all$mean_rel_duct_area) #looks good
qqnorm(yearly_avg_all$pdsi_full); qqline(yearly_avg_all$pdsi_full) #fairly normal, outlier

#Scatterplot
plot(yearly_avg_pdsi_1yr_lag$full_pdsi_lag, yearly_avg_pdsi_1yr_lag$mean_rel_duct_area)
#appears linear

#Relative duct area and last year PDSI linear model ------------------------------------------

pdsi_rda_lm <- lm(mean_rel_duct_area ~ full_pdsi_lag, data = yearly_avg_pdsi_1yr_lag)

summary(pdsi_rda_lm) #significant

# Check assumptions -------------------------------------------------------

plot(fitted(pdsi_rda_lm), resid(pdsi_rda_lm)) #no clear pattern
plot(pdsi_rda_lm, which = 4) #4
plot(pdsi_rda_lm, which = 5)
qqPlot(pdsi_rda_lm) #residuals fairly normal
hist(resid(pdsi_rda_lm)) #not too bad
acf(resid(pdsi_rda_lm), lag.max = 10) #no significant autocorrelation, even though raw data was

model_no4 <- lm(mean_rel_duct_area ~ full_pdsi_lag, data = yearly_avg_pdsi_1yr_lag[-4, ])
coef(pdsi_rda_lm)
coef(model_no4) #changed slope by ~ 1 std error

# Compute DFBETAs
dfb <- dfbeta(pdsi_rda_lm)
# Calculate cutoff
cutoff <- 2 / sqrt(nrow(yearly_avg_all))
# Check if any exceed cutoff
apply(abs(dfb), 2, function(x) any(x > cutoff)) #none exceeding cutoff

# Plot --------------------------------------------------------------------

ggplot(yearly_avg_pdsi_1yr_lag, aes(full_pdsi_lag, mean_rel_duct_area)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', alpha = 0.3) +
  theme_bw(base_size = 15) +
  labs(x = 'Mean PDSI (March-Oct of Last Year)',
       y = 'Mean Relative Duct Area (% of Ring Area)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  theme(panel.grid.minor = element_blank()) 

#ggsave('Output/pdsi_figures/full_pdsi_lag_rda.png', width = 7, height = 6, bg = 'white', dpi = 400)

# Important values --------------------------------------------------------

coef(pdsi_rda_lm)
confint(pdsi_rda_lm)
summary(pdsi_rda_lm) #significant
