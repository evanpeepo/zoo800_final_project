# Load packages -----------------------------------------------------------

library(tidyverse)
library(car)
library(lmtest)

# Load data ---------------------------------------------------------------

duct_metrics <- read.csv('complete_data_12.1.25.csv')

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

#Look at autocorrelation 
acf(yearly_avg_all$pdsi_full, lag.max = 10) #not autocorrelated
acf(yearly_avg_all$mean_total_duct_area, lag.max = 10) #not autocorrelated

#Check normality
qqnorm(yearly_avg_all$mean_total_duct_area); qqline(yearly_avg_all$mean_total_duct_area) #fairly normal, some deviation at tail
hist(yearly_avg_all$mean_total_duct_area) #left skew but not super far from normal
qqnorm(yearly_avg_all$pdsi_full); qqline(yearly_avg_all$pdsi_full) #fairly normal, outlier

#Scatterplot
plot(yearly_avg_all$pdsi_full, yearly_avg_all$mean_total_duct_area)
#looks like there could be a linear relationship

#Based on these checks, it looks like a simple linear regression could be appropriate

# Total duct area and early PDSI linear model ------------------------------------------

pdsi_tda_lm <- lm(mean_total_duct_area ~ pdsi_full, data = yearly_avg_all)

summary(pdsi_tda_lm) #strong evidence

# Check assumptions -------------------------------------------------------

plot(fitted(pdsi_tda_lm), resid(pdsi_tda_lm)) #no clear pattern, one outlier
plot(pdsi_tda_lm, which = 4) #some influential points; 23
plot(pdsi_tda_lm, which = 5) 
qqPlot(pdsi_tda_lm) #residuals fairly normal
acf(resid(pdsi_tda_lm), lag.max = 10) #no significant autocorrelation

#Check influence of specific points on coefficients
model_no23  <- lm(mean_total_duct_area ~ pdsi_full, data = yearly_avg_all[-23, ])

coef(pdsi_tda_lm)
coef(model_no23) #not much change 

# Compute DFBETAs
dfb <- dfbeta(pdsi_tda_lm)
# Calculate cutoff
cutoff <- 2 / sqrt(nrow(yearly_avg_all))
# Check if any exceed cutoff
apply(abs(dfb), 2, function(x) any(x > cutoff)) #none exceeding cutoff

# Plot --------------------------------------------------------------------

ggplot(yearly_avg_all, aes(pdsi_full, y = mean_total_duct_area)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = 'lm', alpha = 0.3) +
  theme_bw(base_size = 15) +
  labs(x = 'Mean PDSI (March-October)',
       y = 'Mean Total Duct Area (mm²)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  theme(panel.grid.minor = element_blank()) 

ggsave('Output/pdsi_figures/full_pdsi_tda.png', width = 6, height = 5, bg = 'white', dpi = 400)

# Important values --------------------------------------------------------

coef(pdsi_tda_lm)
confint(pdsi_tda_lm)

