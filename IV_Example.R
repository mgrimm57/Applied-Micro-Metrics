### IV example using dinkelman 2011 paper
### goal: find effect of having electricity on proportion of women employed


# load in data
library(haven)
df <- read_dta("DATA/dinkelman_aer2011_Econ587.dta")

# set up controls that will be used throughout the analysis
control = ~ kms_to_subs0+ baseline_hhdens0+ base_hhpovrate0+ prop_head_f_a0+
  sexratio0_a+ prop_indianwhite0+ kms_to_road0+ kms_to_town0+ prop_matric_m0+ 
  prop_matric_f0+ d_prop_waterclose+ d_prop_flush+ idcc1+idcc2+ idcc3+
  idcc4+ idcc5+ idcc6+ idcc7+ idcc8+ idcc9
part_a = update(control, d_prop_emp_f ~ . + T)


# regression with clustered standard errors as baseline
library(estimatr)
reg_a<- lm_robust(part_a, data = df, se_type = "stata", cluster = placecode0)
summary(reg_a)

# Treatment regression
part_b = update(control, T ~ . + mean_grad_new)
reg_b<- lm_robust(part_b, data = df, se_type = "stata", cluster = placecode0)
summary(reg_b)

# F-test robust to heteroskedasticity
car::linearHypothesis(reg_b, "mean_grad_new=0")

# F-test done by hand just because why not
F_by_hand = (reg_b[["statistic"]][["mean_grad_new"]])^2


# get predicitions for T based off of regression part_b
get_T_hat = lm(part_b, data = df)
df$T_hat = predict(get_T_hat)

# using T hat instead of T now...
part_e = update(control, d_prop_emp_f ~ . + T_hat)
reg_e = lm_robust(part_e, data = df, se_type = "stata", cluster = placecode0)

summary(reg_e)


# big long IV regression
library(ivreg)
ivmodel=ivreg(d_prop_emp_f ~  kms_to_subs0+ baseline_hhdens0+ base_hhpovrate0+ prop_head_f_a0+
                sexratio0_a+ prop_indianwhite0+ kms_to_road0+ kms_to_town0+ prop_matric_m0+ 
                prop_matric_f0+ d_prop_waterclose+ d_prop_flush+ idcc1+idcc2+ idcc3+
                idcc4+ idcc5+ idcc6+ idcc7+ idcc8+ idcc9+ T 
              | kms_to_subs0+ baseline_hhdens0+ base_hhpovrate0+ prop_head_f_a0+ 
                sexratio0_a+ prop_indianwhite0+ kms_to_road0+ kms_to_town0+ prop_matric_m0+  
                prop_matric_f0+ d_prop_waterclose+ d_prop_flush+ idcc1+idcc2+ idcc3+idcc4+ 
                idcc5+ idcc6+ idcc7+ idcc8+ idcc9 + mean_grad_new, 
              data=df)

# reduced form regression for use in Wald test
part_h = update(control, d_prop_emp_f ~ . + mean_grad_new)
reg_h<- lm_robust(part_h, data = df, se_type = "stata", cluster = placecode0)
summary(reg_h)


# Wald test 
Wald = reg_h[["coefficients"]][["mean_grad_new"]] / reg_b[["coefficients"]][["mean_grad_new"]]


summary(second_stage)