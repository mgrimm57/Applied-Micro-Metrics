### Test for Manipulation in RD setting
### Goal: determine proper testing for the RD done in 'Regression_Discontinuity.R'



## load in dataset & Packages
pacman::p_load(tidyverse, haven, stargazer, rdd, ivreg, ggplot2, magrittr, ggpubr)
p2 <- read_dta("DATA/RD_Manip_Econ587.dta")


##### Part A #####
plot_0.1 <- ggplot(aes(reportwealth1), data = p2) + geom_density(bw=0.1, col = "green") + 
  geom_density(aes(reportwealth2), bw=0.1, col="blue") + 
  geom_density(aes(reportwealth3), bw=0.1, col="purple2") +
  geom_density(aes(reportwealth4), bw=0.1, col="red") + theme_linedraw() + 
  xlab("Bandwidth 0.1") + ylab("")

plot_0.05 <- ggplot(aes(reportwealth1), data = p2) + geom_density(bw=0.1, col = "green") + 
  geom_density(aes(reportwealth2), bw=0.05, col="blue") + 
  geom_density(aes(reportwealth3), bw=0.05, col="purple2") +
  geom_density(aes(reportwealth4), bw=0.05, col="red") + theme_linedraw() + 
  xlab("Bandwidth 0.05") + ylab("")

plot_0.01 <- ggplot(aes(reportwealth1), data = p2) + geom_density(bw=0.01, col = "green") + 
  geom_density(aes(reportwealth2), bw=0.01, col="blue") + 
  geom_density(aes(reportwealth3), bw=0.01, col="purple2") +
  geom_density(aes(reportwealth4), bw=0.01, col="red") + theme_linedraw() + 
  xlab("Bandwidth 0.01") + ylab("")

ggarrange(plot_0.1, plot_0.05, plot_0.01, ncol=3)


##### PART B #####
## rough analysis for wealth1
w1_1 = p2 |> filter(abs(reportwealth1) < 0.1) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "lightgreen", color = "darkgreen") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.1") + ylab("") + theme_cleveland()

w1_05 = p2 |> filter(abs(reportwealth1) < 0.05) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "lightgreen", color = "darkgreen") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.05") + ylab("") + theme_cleveland()

w1_01 = p2 |> filter(abs(reportwealth1) < 0.01) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "lightgreen", color = "darkgreen") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.01") + ylab("") + theme_cleveland()

ggarrange(w1_1, w1_05, w1_01, ncol = 3)

## rough analysis for wealth2
w2_1 = p2 |> filter(abs(reportwealth2) < 0.1) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "lightblue", color = "darkblue") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.1") + ylab("") + theme_cleveland()

w2_05 = p2 |> filter(abs(reportwealth2) < 0.05) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "lightblue", color = "darkblue") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.05") + ylab("") + theme_cleveland()

w2_01 = p2 |> filter(abs(reportwealth2) < 0.01) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "lightblue", color = "darkblue") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.01") + ylab("") + theme_cleveland()

ggarrange(w2_1, w2_05, w2_01, ncol = 3)

## rough analysis for wealth3
w3_1 = p2 |> filter(abs(reportwealth3) < 0.1) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "lavender", color = "purple") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.1") + ylab("") + theme_cleveland()

w3_05 = p2 |> filter(abs(reportwealth3) < 0.05) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "lavender", color = "purple") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.05") + ylab("") + theme_cleveland()

w3_01 = p2 |> filter(abs(reportwealth3) < 0.01) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "lavender", color = "purple") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.01") + ylab("") + theme_cleveland()

ggarrange(w3_1, w3_05, w3_01, ncol = 3)

## rough analysis for wealth4
w4_1 = p2 |> filter(abs(reportwealth4) < 0.1) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "pink", color = "red") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.1") + ylab("") + theme_cleveland()

w4_05 = p2 |> filter(abs(reportwealth4) < 0.05) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "pink", color = "red") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.05") + ylab("") + theme_cleveland()

w4_01 = p2 |> filter(abs(reportwealth4) < 0.01) |> select(reportwealth1)|>
  ggplot(aes(reportwealth1)) + geom_histogram(bins = 15, fill = "pink", color = "red") + 
  geom_vline(xintercept = 0, color = 'black', lwd = 2) +
  xlab("Report 1: v=0.01") + ylab("") + theme_cleveland()

ggarrange(w4_1, w4_05, w4_01, ncol = 3)


##### PART C #####
vars = paste0("reportwealth", 1:4)
sapply(vars, function(x){DCdensity(p2[[x]], cutpoint = 0)})


##### PART D #####
## create dummy variables
p2 %<>% mutate(t1 = if_else(reportwealth1 < 0, 1, 0),
               t2 = if_else(reportwealth2 < 0, 1, 0),
               t3 = if_else(reportwealth3 < 0, 1, 0),
               t4 = if_else(reportwealth4 < 0, 1, 0))

## making newwealth variable
p2 %<>% mutate(new_wealth1 = truewealth + 0.2 * t1 + rnorm(nrow(p2), 0, sqrt(0.01)),
               new_wealth2 = truewealth + 0.2 * t2 + rnorm(nrow(p2), 0, sqrt(0.01)),
               new_wealth3 = truewealth + 0.2 * t3 + rnorm(nrow(p2), 0, sqrt(0.01)),
               new_wealth4 = truewealth + 0.2 * t4 + rnorm(nrow(p2), 0, sqrt(0.01)))

# Plotting density of new wealth variable to summarize it
m_nw1 = mean(p2$new_wealth1)
m_nw2 = mean(p2$new_wealth2)
m_nw3 = mean(p2$new_wealth3)
m_nw4 = mean(p2$new_wealth4)
summary_density <- ggplot(p2) + geom_density(aes(new_wealth1, colour= "New Wealth 1"), colour = "green") + geom_vline(xintercept = m_nw1, colour = "green") + 
  geom_density(aes(new_wealth2, colour= "New Wealth 2"), colour = "blue") + geom_vline(xintercept = m_nw2, colour = "blue") + 
  geom_density(aes(new_wealth3, colour= "New Wealth 3"), colour = "purple") + geom_vline(xintercept = m_nw3, colour = "purple") +
  geom_density(aes(new_wealth4, colour= "New Wealth 4"), colour = "red") + geom_vline(xintercept = m_nw4, colour = "red") + 
  xlab("New Wealth") + ylab("") + theme_linedraw()
summary_density


##### PART E #####
normal_reg = lapply(1:4, function(i){ 
  # Generate formula
  y = paste0("new_wealth", i)
  treat = paste0("t", i)
  report = paste0("reportwealth", i)
  x = paste(treat, report, sep = "*")
  formula = paste(y, x, sep = "~")
  
  # Run regression
  lm(formula, p2) |> summary()
})


##### PART F #####
robust_reg = lapply(1:4, function(i){
  # Generate formula
  y = paste0("new_wealth", i)
  x = paste0("reportwealth", i)
  formula = paste(y, x, sep = "~")
  
  # Run rdd 
  RDestimate(formula, p2)
})