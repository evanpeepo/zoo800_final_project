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
    mean_rel_duct_area = mean(rel_duct_area, na.rm = TRUE),
    mean_bai = mean(bai_cm2, na.rm = TRUE)
  )

#Look at autocorrelation 
acf(yearly_avg_all$mean_rel_duct_area, lag.max = 10) #autocorrelated, slightly beyond blue band at lag 1
acf(yearly_avg_all$mean_bai, lag.max = 10) #not autocorrelated

#Check normality
qqnorm(yearly_avg_all$mean_rel_duct_area); qqline(yearly_avg_all$mean_rel_duct_area) #normal
hist(yearly_avg_all$mean_rel_duct_area) #normal
qqnorm(yearly_avg_all$mean_bai); qqline(yearly_avg_all$mean_bai) #fairly normal, some deviation at tail
hist(yearly_avg_all$mean_bai) #a bit off from normal

#Scatterplot
plot(yearly_avg_all$mean_bai, yearly_avg_all$mean_rel_duct_area)
#vaguely linear

#Relative duct area and BAI linear model ------------------------------------------

bai_rda_lm <- lm(mean_rel_duct_area ~ mean_bai, data = yearly_avg_all)

summary(bai_rda_lm) #significant

#Check assumptions
plot(fitted(bai_rda_lm), resid(bai_rda_lm)) #decent, no clear pattern
plot(bai_rda_lm, which = 4) #some influential points; 11, 17
plot(bai_rda_lm, which = 5)
qqPlot(bai_rda_lm) #decent
acf(resid(bai_rda_lm), lag.max = 10) #no significant autocorrelation, even though raw data was slightly autocorrelated, will proceed with LM
dwtest(bai_rda_lm, alternative = 'less') #not significant

#Check influence of specific points on coefficients
model_no17 <- lm(mean_rel_duct_area ~ mean_bai, data = yearly_avg_all[-17, ])
model_no11 <- lm(mean_rel_duct_area ~ mean_bai, data = yearly_avg_all[-11, ])

coef(bai_rda_lm)
coef(model_no17) 
coef(model_no11)
#within 1 std error

# Plot --------------------------------------------------------------------

ggplot(yearly_avg_all, aes(mean_bai, y = mean_rel_duct_area)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = 'lm') +
  theme_bw(base_size = 15) +
  labs(x = 'Mean BAI (cm²)',
       y = 'Mean Relative Duct Area (% of Ring Area)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  theme(panel.grid.minor = element_blank()) 

#ggsave('Output/bai_figures/bai_rda.png', width = 7, height = 6, bg = 'white')


