# Load packages -----------------------------------------------------------

library(tidyverse)
library(car)

# Load data ---------------------------------------------------------------

pdsi <- read.csv('Data/Processed/socorro_co_pdsi_1989to2025.csv')
duct_metrics <- read.csv('Data/Processed/complete_data_12.1.25.csv')
duct_metrics_1990to2019 <- duct_metrics %>% #these are trees just exposed to ambient conditions
  filter(year < 2020) %>% 
  filter(group != 'legacy')

#Average across all trees

yearly_avg_all <- duct_metrics_1990to2019 %>% 
  group_by(year) %>% 
  summarise(
    mean_total_duct_area = mean(total_duct_area_mm2, na.rm = TRUE),
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
acf(yearly_avg_pdsi_1yr_lag$mean_total_duct_area, lag.max = 10) #not autocorrelated

#Check normality
qqnorm(yearly_avg_all$mean_total_duct_area); qqline(yearly_avg_all$mean_total_duct_area) #fairly normal, some deviation at tail
hist(yearly_avg_all$mean_total_duct_area) #left skew but not super far from normal
qqnorm(yearly_avg_all$pdsi_full); qqline(yearly_avg_all$pdsi_full) #fairly normal, some deviation at tail

#Scatterplot
plot(yearly_avg_pdsi_1yr_lag$full_pdsi_lag, yearly_avg_pdsi_1yr_lag$mean_total_duct_area)
#no apparent relationship

#Total duct area and last year PDSI linear model ------------------------------------------

pdsi_tda_lm <- lm(mean_total_duct_area ~ full_pdsi_lag, data = yearly_avg_pdsi_1yr_lag)

summary(pdsi_tda_lm) #no evidence

# Check assumptions -------------------------------------------------------

plot(fitted(pdsi_tda_lm), resid(pdsi_tda_lm)) #no clear pattern
plot(pdsi_tda_lm, which = 4) #4
plot(pdsi_tda_lm, which = 5)
qqPlot(pdsi_tda_lm) #residuals fairly normal
hist(resid(pdsi_tda_lm)) #not too bad
acf(resid(pdsi_tda_lm), lag.max = 10) #no significant autocorrelation 

model_no4 <- lm(mean_total_duct_area ~ full_pdsi_lag, data = yearly_avg_pdsi_1yr_lag[-4, ])
coef(pdsi_tda_lm)
coef(model_no4) #didn't change coef much, less than 1 std error

# Plot --------------------------------------------------------------------

#Full PDSI
ggplot(yearly_avg_pdsi_1yr_lag, aes(full_pdsi_lag, mean_total_duct_area)) +
  geom_point(alpha = 0.8) +
  theme_bw(base_size = 15) +
  labs(x = 'Mean PDSI (March-Oct of Last Year)',
       y = 'Mean Total Duct Area (mm²)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  theme(panel.grid.minor = element_blank()) 

ggsave('Output/pdsi_figures/full_pdsi_lag_tda.png', width = 7, height = 6, bg = 'white', dpi = 400)

# Important values --------------------------------------------------------

confint(pdsi_tda_lm) #contains zero since no evidence to reject null

#There is not a statistically significant relationship between last year's full season PDSI and mean total duct area.
