### Fixed/Random Effects applied to Seattle voting data
### Goal: find the effect of the new policy (ranking voting system) using Fixed & Random Effects

# Import Data & Packages
pacman::p_load(tidyverse, magrittr, estimatr, fixest, plm, systemfit, tidysynth)
df <- read_dta("DATA/GriffithNoonen2022_Econ587.dta")

# Diff in diff reg for baseline
diff_in_diff_plm =  plm(candidates_ballot ~ post*treatment + At_Large*Special,data=df)

# two way fixed effects regression
two_way_plm =  feols(candidates_ballot ~ post*treatment + as.factor(city) + as.factor(cycle) + At_Large*Special,data=df)

# Run a Hausman Test
plm::phtest(diff_in_diff_plm, two_way_plm)



# prep everything to go into SUR (seemingly unrelated regression)
no_cluster= candidates_ballot ~ post*treatment + At_Large*Special
cluster = candidates_ballot ~ post*treatment + as.factor(cycle)
equations = list(no = no_cluster, yes = cluster)

# Run SUR with no cluster SE
sur_reg = systemfit(equations, method = 'SUR', data = df)
linearHypothesis(sur_reg, c("no_post:treatment - yes_post:treatment = 0"))