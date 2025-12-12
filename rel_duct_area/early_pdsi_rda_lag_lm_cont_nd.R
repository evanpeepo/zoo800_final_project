# Load packages -----------------------------------------------------------

library(tidyverse)
library(car)
library(nlme)
library(lmtest)

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
    pdsi_early = mean(pdsi_early_growing_season),
  )

#New column for previous year's PDSI

yearly_avg_pdsi_1yr_lag <- yearly_avg_all %>% 
  mutate(early_pdsi_lag = lag(pdsi_early, n = 1)
  ) 

# Add 1989 pdsi -----------------------------------------------------------

yearly_avg_pdsi_1yr_lag[1, 4] <- -2.435

# Plot data ---------------------------------------------------------------

#Look at autocorrelation 
acf(yearly_avg_pdsi_1yr_lag$early_pdsi_lag, lag.max = 10) #not autocorrelated

acf(yearly_avg_pdsi_1yr_lag$mean_rel_duct_area, lag.max = 10) #autocorrelated, slightly beyond blue at -0.4

#Check normality
qqnorm(yearly_avg_pdsi_1yr_lag$mean_rel_duct_area); qqline(yearly_avg_pdsi_1yr_lag$mean_rel_duct_area) #fairly normal, some deviation at tail
hist(yearly_avg_pdsi_1yr_lag$mean_rel_duct_area) #normal
qqnorm(yearly_avg_pdsi_1yr_lag$early_pdsi_lag); qqline(yearly_avg_pdsi_1yr_lag$early_pdsi_lag) #near line but s shaped
hist(yearly_avg_pdsi_1yr_lag$early_pdsi_lag) #not far off from normal

#Scatterplot
plot(yearly_avg_pdsi_1yr_lag$early_pdsi_lag, yearly_avg_pdsi_1yr_lag$mean_rel_duct_area)
#appears linear

#Relative Duct Area and last year early PDSI linear model ------------------------------------------

early_pdsi_rda_lm <- lm(mean_rel_duct_area ~ early_pdsi_lag, data = yearly_avg_pdsi_1yr_lag)

summary(early_pdsi_rda_lm) #very strong 


# Check assumptions -------------------------------------------------------

plot(fitted(early_pdsi_rda_lm), resid(early_pdsi_rda_lm)) #looks fine
plot(early_pdsi_rda_lm, which = 4) #4, 5
plot(early_pdsi_rda_lm, which = 5) #4
qqPlot(early_pdsi_rda_lm) #residuals normal enough, one slightly beyond blue band
acf(resid(early_pdsi_rda_lm), lag.max = 10) #no significant autocorrelation 
dwtest(early_pdsi_rda_lm) #not significant

#test outlier influence
model_no4  <- lm(mean_rel_duct_area ~ early_pdsi_lag, data = yearly_avg_pdsi_1yr_lag[-4, ])
model_no5  <- lm(mean_rel_duct_area ~ early_pdsi_lag, data = yearly_avg_pdsi_1yr_lag[-5, ])

coef(early_pdsi_rda_lm)
coef(model_no4) #decreased slope by 17.5%, or 1 standard error
coef(model_no5) #less change than removing 4
confint(early_pdsi_rda_lm)
confint(model_no4) #lots of overlap, model is fairly robust, doesn't change conclusions

# Compute DFBETAs
dfb_early <- dfbeta(early_pdsi_rda_lm)
# Calculate cutoff
cutoff <- 2 / sqrt(nrow(yearly_avg_pdsi_1yr_lag))
# Check if any exceed cutoff
apply(abs(dfb_early), 2, function(x) any(x > cutoff)) #none exceeding cutoff

# Plot --------------------------------------------------------------------

ggplot(yearly_avg_pdsi_1yr_lag, aes(early_pdsi_lag, mean_rel_duct_area)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', alpha = 0.3) +
  theme_bw(base_size = 15) +
  labs(x = 'Mean PDSI (March-June of Prev. Year)',
       y = 'Mean Relative Duct Area (% of Ring Area)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  theme(panel.grid.minor = element_blank()) 

#ggsave('Output/pdsi_figures/early_pdsi_lag_rda.png', width = 6, height = 5, bg = 'white', dpi = 400)


# Important values --------------------------------------------------------
coef(early_pdsi_rda_lm)
confint(early_pdsi_rda_lm)
summary(early_pdsi_rda_lm)

source('Scripts/pdsi/relative_duct_area/full_pdsi_rda_lag_lm_cont_nd.R') #get model with full season PDSI
model_null  <- lm(mean_rel_duct_area ~ 1, data = yearly_avg_pdsi_1yr_lag)

AIC(early_pdsi_rda_lm, pdsi_rda_lm, model_null)
