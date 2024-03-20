### IV example using dinkelman 2011 paper
### goal: find effect of electrifcation on proportion of women employed


# load in data
library(haven)
df <- read_dta("Coursework/metrics/Year 2/587/CODE/DATA/dinkelman_aer2011_Econ587.dta")
# set up controls
control = ~ kms_to_subs0+ baseline_hhdens0+ base_hhpovrate0+ prop_head_f_a0+
  sexratio0_a+ prop_indianwhite0+ kms_to_road0+ kms_to_town0+ prop_matric_m0+ 
  prop_matric_f0+ d_prop_waterclose+ d_prop_flush+ idcc1+idcc2+ idcc3+
  idcc4+ idcc5+ idcc6+ idcc7+ idcc8+ idcc9
part_a = update(control, d_prop_emp_f ~ . + T)

### part a

# regression with clustered standard errors
library(estimatr)
reg_a<- lm_robust(part_a, data = df, se_type = "stata", cluster = placecode0)
summary(reg_a)

### part b
# T regression
part_b = update(control, T ~ . + mean_grad_new)
reg_b<- lm_robust(part_b, data = df, se_type = "stata", cluster = placecode0)
summary(reg_b)

### part c

# F-test robust to heteroskedasticity
car::linearHypothesis(reg_b, "mean_grad_new=0")

### part d

F_by_hand = (reg_b[["statistic"]][["mean_grad_new"]])^2

### part e

get_T_hat = lm(part_b, data = df)
df$T_hat = predict(get_T_hat)
part_e = update(control, d_prop_emp_f ~ . + T_hat)
reg_e = lm_robust(part_e, data = df, se_type = "stata", cluster = placecode0)

### part f

summary(reg_e)

### part g
# estimate model
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

### part h

part_h = update(control, d_prop_emp_f ~ . + mean_grad_new)
reg_h<- lm_robust(part_h, data = df, se_type = "stata", cluster = placecode0)
summary(reg_h)

### part i

Wald = reg_h[["coefficients"]][["mean_grad_new"]] / reg_b[["coefficients"]][["mean_grad_new"]]

### part j

## i dont know how to do this in r :(

### part k

first_stage = lm(part_b, data=df)

df$resid = first_stage[["residuals"]]
part_k = update(control, d_prop_emp_f ~ . +T+ resid)
second_stage = lm_robust(part_k,data=df, se_type="stata", cluster=placecode0)
summary(second_stage)