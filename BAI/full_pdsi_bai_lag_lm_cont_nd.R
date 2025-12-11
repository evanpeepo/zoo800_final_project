# Load packages -----------------------------------------------------------

library(tidyverse)
library(lmtest)
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
    mean_bai = mean(bai_cm2, na.rm = TRUE),
    pdsi_full = mean(pdsi_full_season)
  )

#New column for previous year's PDSI

yearly_avg_pdsi_1yr_lag <- yearly_avg_all %>% 
  mutate(full_pdsi_lag = lag(pdsi_full, n = 1)
  ) 

# Add 1989 pdsi -----------------------------------------------------------

yearly_avg_pdsi_1yr_lag[1, 4] <- -2.107

#Look at autocorrelation 
acf(yearly_avg_pdsi_1yr_lag$full_pdsi_lag, lag.max = 10) #not autocorrelated
acf(yearly_avg_pdsi_1yr_lag$mean_bai, lag.max = 10) #not autocorrelated

#Check normality
qqnorm(yearly_avg_pdsi_1yr_lag$mean_bai); qqline(yearly_avg_pdsi_1yr_lag$mean_bai) #fairly normal, some deviation at tail
hist(yearly_avg_pdsi_1yr_lag$mean_bai) #not too bad
qqnorm(yearly_avg_pdsi_1yr_lag$full_pdsi_lag); qqline(yearly_avg_pdsi_1yr_lag$full_pdsi_lag) #near line, one large outlier
hist(yearly_avg_pdsi_1yr_lag$full_pdsi_lag) #not far off from normal

#Scatterplot
plot(yearly_avg_pdsi_1yr_lag$full_pdsi_lag, yearly_avg_pdsi_1yr_lag$mean_bai)
#no clear pattern

# BAI and previous year march-oct PDSI linear model ------------------------

pdsi_bai_lm <- lm(mean_bai ~ full_pdsi_lag, data = yearly_avg_pdsi_1yr_lag)

summary(pdsi_bai_lm) #no evidence

# Check assumptions -------------------------------------------------------

plot(fitted(pdsi_bai_lm), resid(pdsi_bai_lm)) #no clear pattern
plot(pdsi_bai_lm, which = 4) #4, 22
plot(pdsi_bai_lm, which = 5)
qqPlot(pdsi_bai_lm) #fine
hist(resid(pdsi_bai_lm)) #fine
acf(resid(pdsi_bai_lm), lag.max = 10) #no significant autocorrelation 

#Check influence of specific points on coefficients
model_no4  <- lm(mean_bai ~ full_pdsi_lag, data = yearly_avg_pdsi_1yr_lag[-4, ])
model_no22  <- lm(mean_bai ~ full_pdsi_lag, data = yearly_avg_pdsi_1yr_lag[-22, ])

coef(pdsi_bai_lm)
coef(model_no22)
coef(model_no4)

#fairly large % changes, model not robust, relationship fragile

# Plot --------------------------------------------------------------------

ggplot(yearly_avg_pdsi_1yr_lag, aes(full_pdsi_lag, y = mean_bai)) +
  geom_point(alpha = 0.9) +
  theme_bw(base_size = 15) +
  labs(x = 'Mean PDSI (March-October of Prev. Year)',
       y = 'Mean BAI (cm²)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6) +
  theme(panel.grid.minor = element_blank()) 

#ggsave('Output/pdsi_figures/full_pdsi_lag_bai.png', width = 7, height = 6, bg = 'white')



