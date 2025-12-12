# Load packages -----------------------------------------------------------

library(tidyverse)
library(car)
library(lmtest)
library(nlme)
library(DescTools)

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
    pdsi_full = mean(pdsi_full_season)
  )

#Look at autocorrelation 
acf(yearly_avg_all$pdsi_full, lag.max = 10) #not autocorrelated
acf(yearly_avg_all$mean_rel_duct_area, lag.max = 10) #autocorrelated, slightly beyond blue band at lag 1

#Check normality
qqnorm(yearly_avg_all$mean_rel_duct_area); qqline(yearly_avg_all$mean_rel_duct_area) #normal
hist(yearly_avg_all$mean_rel_duct_area) #normal
qqnorm(yearly_avg_all$pdsi_full); qqline(yearly_avg_all$pdsi_full) #fairly normal, outlier

#Scatterplot
plot(yearly_avg_all$pdsi_full, yearly_avg_all$mean_rel_duct_area)
#no clear relationship

#Going to try linear model first, but might not be appropriate due to autocorrelation

#Relative duct area and early PDSI linear model ------------------------------------------

pdsi_rda_lm <- lm(mean_rel_duct_area ~ pdsi_full, data = yearly_avg_all)

summary(pdsi_rda_lm) #not significant

# Check assumptions -------------------------------------------------------

plot(fitted(pdsi_rda_lm), resid(pdsi_rda_lm)) #no clear pattern
plot(pdsi_rda_lm, which = 4) #some influential points; 23
plot(pdsi_rda_lm, which = 5)
qqPlot(pdsi_rda_lm) #good
acf(resid(pdsi_rda_lm), lag.max = 10) #significant autocorrelation
dwtest(pdsi_rda_lm, alternative = 'less') #significant

# Model accounting for autocorrelation ------------------------------------

pdsi_rda_gls <- gls(
  mean_rel_duct_area ~ pdsi_full,
  data = yearly_avg_all,
  correlation = corAR1(form = ~ year)
)

summary(pdsi_rda_gls) #not significant

pdsi_rda_gls_null <- update(pdsi_rda_gls, . ~ 1)

R2 <- 1 - (var(resid(pdsi_rda_gls)) / var(resid(pdsi_rda_gls_null))) #pseudo R^2 comparing null to fitted

AIC(pdsi_rda_gls, pdsi_rda_gls_null) #not sure why this doesn't work

# Check assumptions -------------------------------------------------------

plot(pdsi_rda_gls, which = 1) #looks fine
qqnorm(resid(pdsi_rda_gls)); qqline(resid(pdsi_rda_gls)) #residuals look decent
acf(resid(pdsi_rda_gls, type = 'normalized'), lag.max = 10) #successfully accounted for autocorrelation
hist(resid(pdsi_rda_gls), breaks = 10) #a bit off but not drastic

# Plot --------------------------------------------------------------------

ggplot(yearly_avg_all, aes(pdsi_full, y = mean_rel_duct_area)) +
  geom_point(alpha = 0.9) +
  theme_bw(base_size = 15) +
  labs(x = 'Mean PDSI (March-October)',
       y = 'Mean Relative Duct Area (% of Ring Area)') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  theme(panel.grid.minor = element_blank()) 

#ggsave('Output/pdsi_figures/full_pdsi_rel_area.png', width = 7, height = 6, bg = 'white', dpi = 400)



