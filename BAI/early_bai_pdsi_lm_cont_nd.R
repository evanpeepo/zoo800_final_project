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
    pdsi_early = mean(pdsi_early_growing_season)
  )

#Look at autocorrelation 
acf(yearly_avg_all$pdsi_early, lag.max = 3) #not autocorrelated
acf(yearly_avg_all$mean_bai, lag.max = 3) #not autocorrelated

#Check normality
qqnorm(yearly_avg_all$mean_bai); qqline(yearly_avg_all$mean_bai) #fairly normal, some deviation at tail
hist(yearly_avg_all$mean_bai) #not too bad
qqnorm(yearly_avg_all$pdsi_early); qqline(yearly_avg_all$pdsi_early) #near line but s shaped
hist(yearly_avg_all$pdsi_early) #not far off from normal

#Scatterplot
plot(yearly_avg_all$pdsi_early, yearly_avg_all$mean_bai)
#looks linear

# BAI and early PDSI linear model ------------------------------------------

early_pdsi_bai_lm <- lm(mean_bai ~ pdsi_early, data = yearly_avg_all)

summary(early_pdsi_bai_lm) #strong evidence

#Check assumptions
plot(fitted(early_pdsi_bai_lm), resid(early_pdsi_bai_lm)) #no clear pattern
plot(early_pdsi_bai_lm, which = 4) #22, 24
plot(early_pdsi_bai_lm, which = 5)
qqPlot(early_pdsi_bai_lm) #mostly ok
hist(resid(early_pdsi_bai_lm)) #low freq at 0
acf(resid(early_pdsi_bai_lm), lag.max = 10) #no significant autocorrelation 

#Check influence of specific points on coefficients
model_no22  <- lm(mean_bai ~ pdsi_early, data = yearly_avg_all[-22, ])
model_no24  <- lm(mean_bai ~ pdsi_early, data = yearly_avg_all[-24, ])

coef(early_pdsi_bai_lm)
coef(model_no22)
coef(model_no24)
#Not much different, model appears robust

# Compute DFBETAs
dfb_early <- dfbeta(early_pdsi_bai_lm)
# Calculate cutoff
cutoff <- 2 / sqrt(nrow(yearly_avg_all))
# Check if any exceed cutoff
apply(abs(dfb_early), 2, function(x) any(x > cutoff)) #none exceeding cutoff

#technically, since no variables had significant autocorrelation, I can calculate correlation
pearson_cor <- cor.test(yearly_avg_all$pdsi_early, yearly_avg_all$mean_bai)
cor.test(yearly_avg_all$pdsi_early, yearly_avg_all$mean_bai, method = 'spearman') #actually stronger than pearson

# BAI and early PDSI with quadratic relationship --------------

early_pdsi_bai_quad <- lm(mean_bai ~ poly(pdsi_early, 2, raw = TRUE), data = yearly_avg_all) #2 is degree of polynomial

quad_summary <- summary(early_pdsi_bai_quad) #quadratic term is almost significant 0.6

#Check assumptions
plot(fitted(early_pdsi_bai_quad), resid(early_pdsi_bai_quad)) #no clear pattern
plot(early_pdsi_bai_quad, which = 4) #24 has high cooks distance
plot(early_pdsi_bai_quad, which = 5)
qqnorm(resid(early_pdsi_bai_quad)); qqline(resid(early_pdsi_bai_quad)) #looks decent
acf(resid(early_pdsi_bai_quad), lag.max = 3) #no significant autocorrelation 

#Check influence of specific points on coefficients
model_no3_quad  <- lm(mean_bai ~ poly(pdsi_early, 2, raw = TRUE), data = yearly_avg_all[-3, ])
model_no24_quad <- lm(mean_bai ~ poly(pdsi_early, 2, raw = TRUE), data = yearly_avg_all[-24, ])
model_no30_quad <- lm(mean_bai ~ poly(pdsi_early, 2, raw = TRUE), data = yearly_avg_all[-30, ])

coef(early_pdsi_bai_quad)
coef(model_no3_quad) 
coef(model_no24_quad)
coef(model_no30_quad)

#Coefficients appear to be fairly stable

# Compute DFBETAs
dfb_early <- dfbeta(early_pdsi_bai_quad)
# Calculate cutoff
cutoff <- 2 / sqrt(nrow(yearly_avg_all))
# Check if any exceed cutoff
apply(abs(dfb_early), 2, function(x) any(x > cutoff)) #none exceeding cutoff

AIC(early_pdsi_bai_lm, early_pdsi_bai_quad) #quadratic slightly lowers AIC, deltaAIC of 2

# Plot --------------------------------------------------------------------
#Linear
ggplot(yearly_avg_all, aes(pdsi_early, y = mean_bai)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = 'lm', alpha = 0.3) +
  theme_bw(base_size = 15) +
  labs(x = 'Mean PDSI (March-June)',
       y = 'Mean BAI (cm²)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6) +
  theme(panel.grid.minor = element_blank()) 

#ggsave('Output/pdsi_figures/early_pdsi_bai.png', width = 6, height = 5, bg = 'white')


#Quadratic
ggplot(yearly_avg_all, aes(pdsi_early, y = mean_bai)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2), alpha = 0.3) +
  theme_bw(base_size = 15) +
  labs(x = 'Mean PDSI (March-June)',
       y = 'Mean BAI (cm²)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6) +
  theme(panel.grid.minor = element_blank()) 


# Important values --------------------------------------------------------

coef(early_pdsi_bai_lm)
confint(early_pdsi_bai_lm)
summary(early_pdsi_bai_lm)
#For every 1 unit increase in early season PDSI, there is a 0.383 cm^2 increase in mean basal area increment. 
#95% confidence interval: 0.231, 0.535

