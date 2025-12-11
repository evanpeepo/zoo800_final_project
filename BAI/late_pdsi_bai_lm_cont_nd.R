# Load packages -----------------------------------------------------------

library(tidyverse)
library(lmtest)
library(car)

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

#Look at autocorrelation 
acf(yearly_avg_all$pdsi_late, lag.max = 3) #not autocorrelated, but close
acf(yearly_avg_all$mean_bai, lag.max = 3) #not autocorrelated

#Check normality
qqnorm(yearly_avg_all$mean_bai); qqline(yearly_avg_all$mean_bai) #fairly normal, some deviation at tail
hist(yearly_avg_all$mean_bai) #not too bad

qqnorm(yearly_avg_all$pdsi_late); qqline(yearly_avg_all$pdsi_late) #fairly normal, some deviation at tail
hist(yearly_avg_all$pdsi_late) #looks normal

#Scatterplot

plot(yearly_avg_all$pdsi_late, yearly_avg_all$mean_bai)
#vaguely linear

#BAI and late PDSI linear model ------------------------------------------

late_pdsi_bai_lm <- lm(mean_bai ~ pdsi_late, data = yearly_avg_all)

summary(late_pdsi_bai_lm) #slightly significant

#Check assumptions
plot(fitted(late_pdsi_bai_lm), resid(late_pdsi_bai_lm)) #no clear pattern
plot(late_pdsi_bai_lm, which = 4) #some large values: 17, 22, 30
plot(late_pdsi_bai_lm, which = 5)
qqPlot(late_pdsi_bai_lm) #residuals normal enough
acf(resid(late_pdsi_bai_lm), lag.max = 3) #no significant autocorrelation 

#Check influence of specific points on coefficients
model_no17_late  <- lm(mean_bai ~ pdsi_late, data = yearly_avg_all[-17, ])
model_no22_late  <- lm(mean_bai ~ pdsi_late, data = yearly_avg_all[-22, ])
model_no30_late  <- lm(mean_bai ~ pdsi_late, data = yearly_avg_all[-30, ])

# Compute DFBETAs
dfb_late <- dfbeta(late_pdsi_bai_lm)
# Calculate cutoff
cutoff <- 2 / sqrt(nrow(yearly_avg_all))
# Check if any exceed cutoff
apply(abs(dfb_late), 2, function(x) any(x > cutoff)) #none exceeding cutoff

coef(late_pdsi_bai_lm)
coef(model_no17_late)
coef(model_no22_late)
coef(model_no30_late)

#No changes larger than 1 std. error, but some pretty big % changes. SE is wide. 

summary(model_no17_late)
summary(model_no22_late) #not significant
summary(model_no30_late)
#Some become not significant, model not very stable, relationship is fragile

#technically, since no variables had significant autocorrelation, I can calculate correlation
#weak, not significant
pearson_cor <- cor.test(yearly_avg_all$pdsi_late, yearly_avg_all$mean_bai)
cor.test(yearly_avg_all$pdsi_late, yearly_avg_all$mean_bai, method = 'spearman') 

# Plot --------------------------------------------------------------------

ggplot(yearly_avg_all, aes(pdsi_late, y = mean_bai)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = 'lm', alpha = 0.3) + #barely significant slope test
  theme_bw(base_size = 15) +
  labs(x = 'Mean PDSI (July-October)',
       y = 'Mean BAI (cm²)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6) +
  theme(panel.grid.minor = element_blank()) 

ggsave('Output/pdsi_figures/late_pdsi_bai.png', width = 7, height = 6, bg = 'white')

# Important values --------------------------------------------------------
coef(late_pdsi_bai_lm)
confint(late_pdsi_bai_lm) #nearly contains 0
summary(late_pdsi_bai_lm) #slightly significant

#There is slightly statistically significant (p = 0.0438) relationship between late summer/fall PDSI and mean basal area increment.
#95% confidence interval: 0.006, 0.425
#Excluding certain outliers has fairly large impact on slope and p-values; relationship is not very robust.