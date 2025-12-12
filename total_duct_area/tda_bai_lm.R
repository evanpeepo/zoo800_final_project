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
    mean_bai = mean(bai_cm2, na.rm = TRUE)
  )

#Look at autocorrelation 
acf(yearly_avg_all$mean_total_duct_area, lag.max = 10) #not autocorrelated
acf(yearly_avg_all$mean_bai, lag.max = 10) #not autocorrelated

#Check normality
qqnorm(yearly_avg_all$mean_total_duct_area); qqline(yearly_avg_all$mean_total_duct_area) #normal
hist(yearly_avg_all$mean_total_duct_area) #left skew
qqnorm(yearly_avg_all$mean_bai); qqline(yearly_avg_all$mean_bai) #fairly normal, some deviation at tail
hist(yearly_avg_all$mean_bai) #a bit off from normal

#Scatterplot
plot(yearly_avg_all$mean_bai, yearly_avg_all$mean_total_duct_area)
#linear

#Relative duct area and BAI linear model ------------------------------------------

bai_tda_lm <- lm(mean_total_duct_area ~ mean_bai, data = yearly_avg_all)

summary(bai_tda_lm) #significant

#Check assumptions
plot(fitted(bai_tda_lm), resid(bai_tda_lm)) #decent, no clear pattern
plot(bai_tda_lm, which = 4) #some influential points; 11, 28
plot(bai_tda_lm, which = 5)
qqPlot(bai_tda_lm) #decent
acf(resid(bai_tda_lm), lag.max = 10) #no significant autocorrelation
dwtest(bai_tda_lm, alternative = 'less') #not significant

#Check influence of specific points on coefficients
model_no28 <- lm(mean_total_duct_area ~ mean_bai, data = yearly_avg_all[-28, ])
model_no11 <- lm(mean_total_duct_area ~ mean_bai, data = yearly_avg_all[-11, ])

coef(bai_tda_lm)
coef(model_no28) 
coef(model_no11)
#very small changes

# Plot --------------------------------------------------------------------

ggplot(yearly_avg_all, aes(mean_bai, y = mean_total_duct_area)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = 'lm') +
  theme_bw(base_size = 15) +
  labs(x = 'Mean BAI (cm²)',
       y = 'Mean Total Duct Area (mm²)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  theme(panel.grid.minor = element_blank()) 

#ggsave('Output/bai_figures/bai_tda.png', width = 6, height = 5, bg = 'white')


