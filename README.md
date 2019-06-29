## Title: Regression Module Assignment 1
## Exlore variables to improve fit of proposed model for test growth
## Author: Eben Dowell
## Date: 6/29/2019
## Last Updated: 6/29/2019

###--------------------
# Load the data (from your own pathway to the file):
load("C:/SDP/R_work/regression_1_r/data/full_sch_data.rda")

# Load any necessary packages
library(ggplot2) # for plotting
library(dplyr) # for data manipulation
library(broom) # for working with statistical models in a tidy way
library(coefplot) # for making quick plots of regression coefficients
library(gvlma) # global tests of fit

###--------------------
# Work with copy of original file
sch_scr <- sch_score

# Recreate the proposed model ("Model 1") and generate key fields for analysis
m1 <- lm(ss2 ~ ss1, data = sch_scr)

sch_scr$yhat <- fitted(m1)
sch_scr$bin_yhat <- ntile(sch_scr$yhat, 5) #quintile of predicted
sch_scr$bin_n1 <- ntile(sch_scr$n1, 5) #quintile of testtaker count
sch_scr$resid <- residuals(m1)
sch_scr$std_resid <- rstandard(m1)

sch_scr$schid_uniq <- paste0(sch_scr$distid, "-", sch_scr$schid)

###--------------------
# Before exploring additional variables, let's apply two filters to the data set...
# Minimum n size of 20: Prelim work demonstrated high variance for smaller n sizes.  Since the purpose is NOT comprehensive identification of schools, we have the flexibility to set an n threshold that lessens the risk of misidentification and also aligns with the agency's suppression rules.  
# Likewise, it is preferable to use only the latest three years of resuls in order to hone in on recent performance and to allow for system stabilization to the testing regime which began in SY 2006 (go with it).

sch_scr2 <- subset(sch_scr, n1>19 & n2>19 & test_year>2008, ) 

###--------------------
# Rerun model (as Model 2) and recalculate the key fields for analysis
# Model observations reduced from 15,087 to 8,094
m2 <- lm(ss2 ~ ss1, data = sch_scr2)

sch_scr2$yhat <- fitted(m2)
sch_scr2$bin_yhat <- ntile(sch_scr2$yhat, 5)
sch_scr2$bin_n1 <- ntile(sch_scr2$n1, 5)
sch_scr2$resid <- residuals(m2)
sch_scr2$std_resid <- rstandard(m2)

###--------------------
# See if these filters alone improve the model fit
gv1 <- gvlma(m1) 
summary(gv1)

gv2 <- gvlma(m2) 
summary(gv2)

# Run residual distributions by fitted value and testtaker quintiles, as well as by grade and subject
# Residual variance remains uneven by score, size, and grade level
# For Model 1
sch_scr %>% group_by(bin_yhat) %>%
  summarize(error_var = var(resid),
            error_sd = sd(resid))

sch_scr %>% group_by(bin_n1) %>%
  summarize(error_var = var(resid),
            error_sd = sd(resid))

sch_scr %>% group_by(grade) %>%
  summarize(error_var = var(resid),
            error_sd = sd(resid))

sch_scr %>% group_by(subject) %>%
  summarize(error_var = var(resid),
            error_sd = sd(resid))
# For Model 2
sch_scr2 %>% group_by(bin_yhat) %>%
  summarize(error_var = var(resid),
            error_sd = sd(resid))

sch_scr2 %>% group_by(bin_n1) %>%
  summarize(error_var = var(resid),
            error_sd = sd(resid))

sch_scr2 %>% group_by(grade) %>%
  summarize(error_var = var(resid),
            error_sd = sd(resid))

sch_scr2 %>% group_by(subject) %>%
  summarize(error_var = var(resid),
            error_sd = sd(resid))

###--------------------
# Explore influence of other individual variables on ss2, absent ss1
# Attendance rate
chk1 <- lm(ss2 ~ att_rate, data = sch_scr2)
summary(chk1) #AdjR^2 = 0.09419
# Suspension rate
chk2 <- lm(ss2 ~ suspension_rate, data = sch_scr2)
summary(chk2) #AdjR^2 = 0.1126
# Locale description
chk3 <- lm(ss2 ~ locale_desc, data = sch_scr2)
summary(chk3) #AdjR^2 = 0.1799
# School size grouping
chk4 <- lm(ss2 ~ school_size, data = sch_scr2)
summary(chk4) #AdjR^2 = 0.04068
# Ratio of students to licensed educators
chk5 <- lm(ss2 ~ ratio_stu_to_licensed, data = sch_scr2)
summary(chk5) #AdjR^2 = -0.000106

# There are weak (and sometimes counterintuitive) relationships with ss2 for the variables above

# If we turn attention back to variables identified as influencial in prelim work...
# Test taker count
chk6 <- lm(ss2 ~ n1, data = sch_scr2)
summary(chk6) #AdjR^2 = 0.3036
# Grade level
chk7 <- lm(ss2 ~ grade, data = sch_scr2)
summary(chk7) #AdjR^2 = 0.5428
# FRL percent
chk8 <- lm(ss2 ~ frl_per, data = sch_scr2)
summary(chk8) #AdjR^2 = 0.3347

###--------------------
# Let's see how much these 3 influential variables, along with subject, can improve the orginal model
# Create a data set where input fields are complete
sch_scr3 <- sch_scr2[(!is.na(sch_scr2$ss1) & !is.na(sch_scr2$subject) & !is.na(sch_scr2$grade) & !is.na(sch_scr2$n1) & !is.na(sch_scr2$frl_per)), ]

# Run Model 3 and create key fields for analysis
m3 <- lm(ss2 ~ ss1 + subject + grade + n1 + frl_per, data = sch_scr3)
gv3 <- gvlma(m3) 
summary(gv3)

nrow(m2$model) - nrow(m3$model) # 29 rows lost due to missingness
sch_scr3$yhat_m3 <- fitted(m3)
sch_scr3$bin_yhat_m3 <- ntile(sch_scr3$yhat_m3, 5)
sch_scr3$bin_n1_m3 <- ntile(sch_scr3$n1, 5)
sch_scr3$resid_m3 <- residuals(m3)
sch_scr3$std_resid_m3 <- rstandard(m3)

# Inspect Model 3 residual plots
source("R/plotting_funs.R")
resid_plot(m3, bins = 100)

ggplot(sch_scr3) + aes(x = yhat_m3, y = std_resid_m3) +
  geom_point(alpha = I(0.1)) +
  geom_smooth() + geom_smooth(method = "lm") + 
  theme_bw() + labs(title = "M3 Std Residual by Predicted")

ggplot(sch_scr3) + aes(x = n1, y = std_resid_m3) +
  geom_point(alpha = I(0.1)) +
  geom_smooth(method = "lm", se=FALSE) + 
  theme_bw() + labs(title = "M3 Std Residual by Number of Test Takers")

# Inspect Model 3 residual distributions by bins and categories
sch_scr3 %>% group_by(bin_yhat_m3) %>%
  summarize(error_var = var(resid_m3),
            error_sd = sd(resid_m3))

sch_scr3 %>% group_by(bin_n1) %>%
  summarize(error_var = var(resid_m3),
            error_sd = sd(resid_m3))

sch_scr3 %>% group_by(grade) %>%
  summarize(error_var = var(resid_m3),
            error_sd = sd(resid_m3))

sch_scr3 %>% group_by(subject) %>%
  summarize(error_var = var(resid_m3),
            error_sd = sd(resid_m3))

# Check for identification of over- and under-performers by Model 3
table(sch_scr3$std_resid_m3 >=2, sch_scr3$test_year)
table(sch_scr3$std_resid_m3 >=2, sch_scr3$grade)
table(sch_scr3$std_resid_m3 >=2, sch_scr3$subject)
table(sch_scr3$std_resid_m3 >=2, sch_scr3$bin_yhat_m3)
table(sch_scr3$std_resid_m3 >=2, sch_scr3$bin_n1)

table(sch_scr3$std_resid_m3 <=-2, sch_scr3$test_year)
table(sch_scr3$std_resid_m3 <=-2, sch_scr3$grade)
table(sch_scr3$std_resid_m3 <=-2, sch_scr3$subject)
table(sch_scr3$std_resid_m3 <=-2, sch_scr3$bin_yhat_m3)
table(sch_scr3$std_resid_m3 <=-2, sch_scr3$bin_n1)

###--------------------
# Let's see the impact of breaking Model 3 into two models by grade group
# Model for grades 4 and 5
sch_scr4a <- sch_scr3[(sch_scr3$grade < 6) , ]
m4a <- lm(ss2 ~ ss1 + subject + subject + frl_per, data = sch_scr4a) # note that n1 removed as no longer significant 
gv4a <- gvlma(m4a) 
summary(gv4a)

sch_scr4a$yhat_m4a <- fitted(m4a)
sch_scr4a$bin_yhat_m4a <- ntile(sch_scr4a$yhat_m4a, 5)
sch_scr4abin_n1_m4a <- ntile(sch_scr4a$n1, 5)
sch_scr4a$resid_m4a <- residuals(m4a)
sch_scr4a$std_resid_m4a <- rstandard(m4a)

# Model for grades 6, 7 and 8
sch_scr4b <- sch_scr3[(sch_scr3$grade > 5) , ]
m4b <- lm(ss2 ~ ss1 + subject + grade + frl_per, data = sch_scr4b)
gv4b <- gvlma(m4b) 
summary(gv4b)

sch_scr4b$yhat_m4b <- fitted(m4b)
sch_scr4b$bin_yhat_m4b <- ntile(sch_scr4b$yhat_m4b, 5)
sch_scr4bbin_n1_m4b <- ntile(sch_scr4b$n1, 5)
sch_scr4b$resid_m4b <- residuals(m4b)
sch_scr4b$std_resid_m4b <- rstandard(m4b)

# Inspect residual plots for Models 4a and 4b
source("R/plotting_funs.R")
resid_plot(m4a, bins = 100)
resid_plot(m4b, bins = 100)

ggplot(sch_scr4a) + aes(x = yhat_m4a, y = std_resid_m4a) +
  geom_point(alpha = I(0.1)) +
  geom_smooth() + geom_smooth(method = "lm") + 
  theme_bw() + labs(title = "M4a Std Residual by Predicted")

ggplot(sch_scr4b) + aes(x = yhat_m4b, y = std_resid_m4b) +
  geom_point(alpha = I(0.1)) +
  geom_smooth() + geom_smooth(method = "lm") + 
  theme_bw() + labs(title = "M4b Std Residual by Predicted")

ggplot(sch_scr4a) + aes(x = n1, y = std_resid_m4a) +
  geom_point(alpha = I(0.1)) +
  geom_smooth(method = "lm", se=FALSE) + 
  theme_bw() + labs(title = "M4a Std Residual by Number of Test Takers")

ggplot(sch_scr4b) + aes(x = n1, y = std_resid_m4b) +
  geom_point(alpha = I(0.1)) +
  geom_smooth(method = "lm", se=FALSE) + 
  theme_bw() + labs(title = "M4b Std Residual by Number of Test Takers")

# Inspect residual distributions by bins and categories for Models 4a and 4b
table(sch_scr4a$std_resid_m4a >=2, sch_scr4a$subject)
table(sch_scr4a$std_resid_m4a >=2, sch_scr4a$bin_yhat_m4a)
table(sch_scr4a$std_resid_m4a >=2, sch_scr4a$bin_n1)

table(sch_scr4b$std_resid_m4b >=2, sch_scr4b$subject)
table(sch_scr4b$std_resid_m4b >=2, sch_scr4b$bin_yhat_m4b)
table(sch_scr4b$std_resid_m4b >=2, sch_scr4b$bin_n1)

###--------------------
# For later exploration: some variables (such as attendence rate and suspension rate) become significant after the model is broken into two grade groups
