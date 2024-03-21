### Difference and Difference applied to Seattle voting data
### Goal: find the effect of the new policy (ranking voting system) using diff in diff

# Import Data & Packages
pacman::p_load(tidyverse, magrittr, estimatr, fixest, plm, systemfit, tidysynth, haven)
df <- read_dta("Coursework/metrics/Year 2/587/CODE/DATA/GriffithNoonen2022_Econ587.dta")

# create dummy variable =1 if in seattle post 2017
df %<>% mutate(post = if_else(cycle >= 2017, 1, 0), 
               treatment = if_else(city == "Seattle", 1, 0),
               city_cycle = as.factor(city):as.factor(cycle),
               seattle = if_else(city == "Seattle", 1, 0))

# naive OLS regression to serve as a baseline
naive = lm_robust(candidates_ballot ~ treatment + At_Large*Special, clusters = city_cycle, data=df)
summary(naive)


# before and after treatment in Seattle
seattle = df %>% filter(city == "Seattle")

sea_reg = lm_robust(candidates_ballot ~ post + At_Large*Special, data=seattle, 
                    clusters = city_cycle)

# Before and after treatment in control cities
non_seattle = df %>% filter(city != "Seattle")

control_reg = lm_robust(candidates_ballot ~ post + At_Large*Special, data = non_seattle, 
                        clusters = city_cycle)

# difference between two results (naive average treatment effect)
dif = sea_reg[["coefficients"]][["post"]] - control_reg[["coefficients"]][["post"]]


# Pre-Treatment Regression
pre = df %>% filter(post == 0)

pre_reg = lm_robust(candidates_ballot ~ seattle + At_Large*Special, data=pre, 
                    clusters = city_cycle)

# Post Treatment Regression
post = df %>% filter(post == 1)

post_reg = lm_robust(candidates_ballot ~ seattle + At_Large*Special, data=post, 
                     clusters = city_cycle)

# difference (another ATE)
dif_pre_post = post_reg[["coefficients"]][["seattle"]] - pre_reg[["coefficients"]][["seattle"]]

# simple diff in diff regression to find ATE
diff_in_diff = lm_robust(candidates_ballot ~ post*treatment + 
                           At_Large*Special,data=df, clusters = city_cycle)
summary(diff_in_diff)

# two way fixed effects regression
two_way = df %>% feols(candidates_ballot ~ post*treatment | as.factor(city) + 
                         as.factor(cycle) + At_Large*Special,., cluster = "city_cycle")
summary(two_way)


# test for parallel pre trends (no fixed effect)
df |> filter(post == 0 ) |> 
  mutate(`cycle*seattle` = cycle*seattle) %>%
  lm_robust(candidates_ballot ~ `cycle*seattle` + At_Large*Special + as.factor(city),., clusters = city_cycle)

# test for parallel pre trends (fixed effect)
df |> filter(post == 0 ) |> 
  mutate(`cycle*seattle` = cycle*seattle) %>%
  lm_robust(candidates_ballot ~ `cycle*seattle` + At_Large*Special,., clusters = city_cycle)

# treatment effect allowing for non-parallel trends
df |> mutate(`seattle*cycle` = seattle*cycle, `seattle*post` = seattle*post) %>%
  feols(candidates_ballot ~ cycle +  `seattle*cycle` + `seattle*post` + 
          At_Large*Special + as.factor(city),., cluster = "city_cycle")

