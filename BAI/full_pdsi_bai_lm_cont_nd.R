# Load packages -----------------------------------------------------------

library(tidyverse)
library(lmtest)
library(car)

# Load data ---------------------------------------------------------------

duct_metrics <- read.csv('complete_data_12.1.25.csv')

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

#Look at autocorrelation 
acf(yearly_avg_all$pdsi_full, lag.max = 10) #not autocorrelated
acf(yearly_avg_all$mean_bai, lag.max = 10) #not autocorrelated

#Check normality
qqnorm(yearly_avg_all$mean_bai); qqline(yearly_avg_all$mean_bai) #fairly normal, some deviation at tail
hist(yearly_avg_all$mean_bai) #not too bad
qqnorm(yearly_avg_all$pdsi_full); qqline(yearly_avg_all$pdsi_full) #near line, one large outlier
hist(yearly_avg_all$pdsi_full) #not far off from normal

#Scatterplot
plot(yearly_avg_all$pdsi_full, yearly_avg_all$mean_bai)
#looks linear

# BAI and march-oct PDSI linear model ------------------------------------------

pdsi_bai_lm <- lm(mean_bai ~ pdsi_full, data = yearly_avg_all)

summary(pdsi_bai_lm) #strong evidence

# Check assumptions -------------------------------------------------------

plot(fitted(pdsi_bai_lm), resid(pdsi_bai_lm)) #no clear pattern
plot(pdsi_bai_lm, which = 4) #3, 22
plot(pdsi_bai_lm, which = 5)
qqPlot(pdsi_bai_lm) #mostly ok
hist(resid(pdsi_bai_lm)) #not terrible
acf(resid(pdsi_bai_lm), lag.max = 10) #no significant autocorrelation 

#Check influence of specific points on coefficients
model_no3  <- lm(mean_bai ~ pdsi_full, data = yearly_avg_all[-3, ])
model_no22  <- lm(mean_bai ~ pdsi_full, data = yearly_avg_all[-22, ])

coef(pdsi_bai_lm)
coef(model_no22)
coef(model_no3)

#no differences in slope beyond 1 std. error

# Compute DFBETAs
dfb <- dfbeta(pdsi_bai_lm)
# Calculate cutoff
cutoff <- 2 / sqrt(nrow(yearly_avg_all))
# Check if any exceed cutoff
apply(abs(dfb), 2, function(x) any(x > cutoff)) #none exceeding cutoff

# Plot --------------------------------------------------------------------

ggplot(yearly_avg_all, aes(pdsi_full, y = mean_bai)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = 'lm', alpha = 0.3) +
  theme_bw(base_size = 15) +
  labs(x = 'Mean PDSI (March-October)',
       y = 'Mean BAI (cm²)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6) +
  theme(panel.grid.minor = element_blank()) 

ggsave('Output/pdsi_figures/full_pdsi_bai.png', width = 7, height = 6, bg = 'white')


# Important values --------------------------------------------------------

coef(pdsi_bai_lm)
confint(pdsi_bai_lm)
summary(pdsi_bai_lm) #strong evidence

