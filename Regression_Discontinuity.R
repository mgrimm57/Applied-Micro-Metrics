### Regression Discontinuity
### Goal: Find the effect of the policy on test score using RD


## load in dataset & Packages
pacman::p_load(tidyverse, haven, stargazer, rdd, ivreg, ggplot2, magrittr, ggpubr, stargazer)
p1 <- read_dta("DATA/Ozier_JHR_Econ587.dta")


## create dummy variable for test
p1$treatment <- ifelse(p1$test > 0, 1, 0)

## standard OLS regression
reg_1a <- lm(secondary ~ test + treatment + test*treatment, data=p1)
summary(reg_1a)

## Assess how OLS slope changes as we approach the cutoff of 0

## limit to <0.8
p1_0.8 = p1[p1$test < 0.8 & p1$test > -0.8, ]
reg_1b_0.8 <- lm(secondary ~ test + treatment + test*treatment, data=p1_0.8)
## limit to <0.4
p1_0.4 = p1[p1$test < 0.4 & p1$test > -0.4, ]
reg_1b_0.4 <- lm(secondary ~ test + treatment + test*treatment, data=p1_0.4)
## limit to <0.2
p1_0.2 = p1[p1$test < 0.2 & p1$test > -0.2, ]
reg_1b_0.2 <- lm(secondary ~ test + treatment + test*treatment, data=p1_0.2)
## limit to <0.1
p1_0.1 = p1[p1$test < 0.1 & p1$test > -0.1, ]
reg_1b_0.1 <- lm(secondary ~ test + treatment + test*treatment, data=p1_0.1)


## regression table of the results
stargazer( reg_1b_0.8, reg_1b_0.4, reg_1b_0.2, reg_1b_0.1, type = "text")


## regression discontinuity (RD) regression, cutoff at 0
rd1 <- RDestimate(secondary ~ test, p1, cutpoint = 0) 

## plot of the data along with a cutoff and two OLS lines at either end
ggplot(data=p1, aes(test, secondary)) + geom_jitter(aes(colour = treatment), show.legend = FALSE, height = 0.1) + 
  geom_smooth(data = p1 |> filter(test > 0), method = 'lm', col = 'red', se=FALSE) + 
  geom_smooth(data = p1 |> filter(test < 0), method = 'lm', col = 'darkblue', se=FALSE) + 
  xlab("Test Score: Normalized to 0") + ylab("Probability of Completing Secondary School") + theme_linedraw()


## RD with a bandwidth of 0.8
rd_rect = RDestimate(secondary ~ test, p1, cutpoint = 0, kernel = 'rectangular', bw = 0.8)


## IV regression for comparison
iv <- ivreg(rv ~ secondary + test + female + test:treatment | treatment + test + female + test:treatment, data = p1)


## Adding more covariates to the RD (bandwidth = default)
rd_covariates <- RDestimate(rv ~ test + secondary, p1, cutpoint = 0)
rd_like_iv <- RDestimate(rv ~ test + secondary | female , data =p1, cutpoint = 0, kernel = 'rectangular', bw = 0.6)