### Synthetic Control applied to Seattle voting data
### Goal: find the effect of the new policy (ranking voting system) using Synthetic Control

# Import Data & Packages
pacman::p_load(tidyverse, magrittr, estimatr, fixest, plm, systemfit, tidysynth, haven)
df <- read_dta("DATA/GriffithNoonen2022_Econ587.dta")

# Cleaning up data
balanced_df = df |> 
  mutate(cycle = factor(df$cycle, labels = 1:10), 
         city_cycle = as.factor(city):as.factor(cycle)) |> # Remake this variable using new numbering
  group_by(city_cycle) |> 
  summarise(candidates_ballot = mean(candidates_ballot, na.rm = T),
            post = mean(post, na.rm = T),
            treatment = mean(treatment, na.rm = T),
            At_Large = mean(At_Large, na.rm = T),
            Special = mean(Special, na.rm = T),
            seattle = mean(seattle, na.rm = T),
            Pct_general = mean(Pct_general, na.rm = T),
            inc_run = mean(inc_run, na.rm = T),
            inc_win = mean(inc_win, na.rm = T),
            inc_pct_general = mean(inc_pct_general, na.rm = T),
            Votes_total_general = mean(Votes_total_general, na.rm = T),
            donors = mean(donors, na.rm = T),
            total_Less200 = mean(total_Less200, na.rm = T),
            donors_Less200  = mean(donors_Less200, na.rm = T),
            pop = mean(pop, na.rm = T),
            pop100k = mean(pop100k, na.rm = T),
            state = unique(state)) |> 
  mutate(city = stringr::word(city_cycle, sep = ":"),
         cycle = as.numeric(stringr::word(city_cycle, start = -1, sep = ":")))

## Standard diff in diff regression with various controls
balanced_dd = lm_robust(candidates_ballot ~ post*treatment + At_Large*Special, data = balanced_df, clusters = city_cycle)
summary(balanced_dd)

## Balanced two way fixed effects regression
balanced_twfe =  feols(candidates_ballot ~ post*treatment | as.factor(city) + 
                         as.factor(cycle) + At_Large*Special,data = balanced_df, cluster = "city_cycle")
summary(balanced_twfe)


## Generate synthetic object (all possible cities)
all_synth = balanced_df |> synthetic_control(outcome = candidates_ballot,
                                             unit = city,
                                             time = cycle,
                                             i_unit = 'Seattle',
                                             i_time = 8) |> 
  
  generate_predictor(At_Large = At_Large, 
                     Special = Special,
                     Pct_general = Pct_general,
                     inc_run = inc_run,
                     inc_win = inc_win,
                     inc_pct_general = inc_pct_general,
                     pop = pop,
                     pop100k = pop100k) |> generate_weights() |> generate_control() 

# plotting the weights to see how the synthetic control is looking
plot_weights(all_synth)


# Generate synthetic weights (Just Washington)
only_wash =balanced_df |> filter(state == 'Wash') |> 
  synthetic_control(outcome = candidates_ballot, unit = city, time = cycle,
                    i_unit = 'Seattle', i_time = 8) |> 
  generate_predictor(At_Large = At_Large, 
                     Pct_general = Pct_general,
                     inc_run = inc_run,
                     inc_win = inc_win,
                     inc_pct_general = inc_pct_general,
                     pop = pop,
                     pop100k = pop100k) |> generate_weights() |> generate_control() 

# plotting the weights
plot_weights(only_wash)

## Part E ##

# Generate synthetic weights (Just California)
only_cali = balanced_df |> filter(state == 'Calif' | city == 'Seattle') |> 
  synthetic_control(outcome = candidates_ballot,
                    unit = city,
                    time = cycle,
                    i_unit = 'Seattle',
                    i_time = 8) |> 
  
  generate_predictor(Pct_general = Pct_general,
                     inc_run = inc_run,
                     inc_win = inc_win,
                     inc_pct_general = inc_pct_general,
                     pop = pop,
                     pop100k = pop100k) |> generate_weights() |> generate_control() 

# plotting the weights
plot_weights(only_cali)


# Drop Seattle from data
noseattle_df = balanced_df |> filter(city != 'Seattle')

# weights for placebo treatment
placebo_weights = lapply(unique(noseattle_df$city),
                         function(x) {
                           synthetic_control(
                             noseattle_df,
                             outcome = candidates_ballot,
                             unit = city,
                             time = cycle,
                             i_unit = x,
                             i_time = 8
                           ) |>
                             generate_predictor(
                               At_Large = At_Large,
                               Pct_general = Pct_general,
                               inc_run = inc_run,
                               inc_win = inc_win,
                               inc_pct_general = inc_pct_general,
                               pop = pop,
                               pop100k = pop100k
                             ) |>
                             generate_weights() |>
                             generate_control()
                         }
)
# find the coefficients for each placebo group to see how well the synthetic control works
lapply(1:length(placebo_weights), function(i){
  placebo_weights = placebo_weights[[i]]
  plot_weights(placebo_weights)
  grab_unit_weights(placebo_weights)
})
