### Measurement Error in regressors fixerd by using Instrumental Variables

# generate variables
x = rnorm(10000,10,2)
epsilon = rnorm(10000, 0, 10^{1/2})
x_1 = c()
x_2 = c()
y = c()
for (i in 1:10000) {
  x_1[i] = x[i] + rnorm(1,0,1)
  x_2[i] = x[i] + rnorm(1,0,2)
  y[i] = 3 + 1*x[i] + epsilon[i]
}

# function for regressions
regression <- function(y,x){
  beta_0 = c()
  beta_1 = c()
  se_beta_0 = c()
  se_beta_1 = c()
  for (j in 1:1000) {
    normal = lm(y~x)
    normal_s = summary(normal)
    beta_0[j] = normal_s$coefficients[1, 1]
    beta_1[j] = normal_s$coefficients[2, 1]
    se_beta_0[j] = normal_s$coefficients[1, 2]
    se_beta_1[j] = normal_s$coefficients[2, 2]
  }
  b_0 = round(mean(beta_0), 4)
  b_1 = round(mean(beta_1), 4)
  se_0 = round(mean(se_beta_0), 4)
  se_1 = round(mean(se_beta_1), 4)
  results = c(b_0,b_1,se_0, se_1)
  return(results)
}

# regression results (a-d)
regression(y,x)
regression(y, x_1)
regression(y, x_2)

# 2SLS regression
SLS <- function(y,iv,iv2){
  beta_0 = c()
  beta_1 = c()
  se_beta_0 = c()
  se_beta_1 = c()
  gamma_0 = c()
  gamma_1 = c()
  se_gamma_0 = c()
  se_gamma_1 = c()
  for (j in 1:1000) {
    sls = lm(iv~iv2)
    sls_s = summary(sls)
    gamma_0[j] = sls_s$coefficients[1, 1]
    gamma_1[j] = sls_s$coefficients[2, 1]
    se_gamma_0[j] = sls_s$coefficients[1, 2]
    se_gamma_1[j] = sls_s$coefficients[2, 2]
    normal = lm(y~sls[["fitted.values"]])
    normal_s = summary(normal)
    beta_0[j] = normal_s$coefficients[1, 1]
    beta_1[j] = normal_s$coefficients[2, 1]
    se_beta_0[j] = normal_s$coefficients[1, 2]
    se_beta_1[j] = normal_s$coefficients[2, 2]
  }
  b_0 = round(mean(beta_0), 4)
  b_1 = round(mean(beta_1), 4)
  se_0 = round(mean(se_beta_0), 4)
  se_1 = round(mean(se_beta_1), 4)
  g_0 = round(mean(gamma_0), 4)
  g_1 = round(mean(gamma_1), 4)
  se_g_0 = round(mean(se_gamma_0),4)
  se_g_1 = round(mean(se_gamma_1),4)
  results = c(b_0,b_1,se_0, se_1, g_0,g_1,se_g_0, se_g_1)
  return(results)
}

# 2SLS for h
# 2SLS regression
SLSH <- function(){
  beta_0 = c()
  beta_1 = c()
  se_beta_0 = c()
  se_beta_1 = c()
  gamma_0 = c()
  gamma_1 = c()
  se_gamma_0 = c()
  se_gamma_1 = c()
  for (j in 1:1000) {
    x = rnorm(10000,10,2)
    epsilon = rnorm(10000, 0, 10^{1/2})
    x_1 = c()
    x_2 = c()
    y = c()
    for (i in 1:10000) {
      x_1[i] = x[i] + rnorm(1,0,1)
      x_2[i] = x[i] + rnorm(1,0,2)
      y[i] = 3 + 1*x[i] + epsilon[i]
    }
    sls = lm(x_2~x_1)
    sls_s = summary(sls)
    gamma_0[j] = sls_s$coefficients[1, 1]
    gamma_1[j] = sls_s$coefficients[2, 1]
    se_gamma_0[j] = sls_s$coefficients[1, 2]
    se_gamma_1[j] = sls_s$coefficients[2, 2]
    normal = lm(y~sls[["fitted.values"]])
    normal_s = summary(normal)
    beta_0[j] = normal_s$coefficients[1, 1]
    beta_1[j] = normal_s$coefficients[2, 1]
    se_beta_0[j] = normal_s$coefficients[1, 2]
    se_beta_1[j] = normal_s$coefficients[2, 2]
  }
  b_0 = round(mean(beta_0), 4)
  b_1 = round(mean(beta_1), 4)
  se_0 = round(mean(se_beta_0), 4)
  se_1 = round(mean(se_beta_1), 4)
  g_0 = round(mean(gamma_0), 4)
  g_1 = round(mean(gamma_1), 4)
  se_g_0 = round(mean(se_gamma_0),4)
  se_g_1 = round(mean(se_gamma_1),4)
  e_se_b0 = round(sd(beta_0),4)
  e_se_b1 = round(sd(beta_1),4)
  # for part g
  empirical_se_b0 = sd(beta_0)
  empirical_se_b1 = sd(beta_1)
  results = c(b_0,b_1,se_0, se_1, g_0,g_1,se_g_0, se_g_1, e_se_b0, e_se_b1)
  return(results)
}

SLSH
SLS(y, x_2,x_1)
# part i
library("ivreg")
beta_0 = c()
beta_1 = c()
se_beta_0 = c()
se_beta_0 = c()
for (k in 1:1000){
  x = rnorm(10000,10,2)
  epsilon = rnorm(10000, 0, 10^{1/2})
  x_1 = c()
  x_2 = c()
  y = c()
  for (i in 1:10000) {
    x_1[i] = x[i] + rnorm(1,0,1)
    x_2[i] = x[i] + rnorm(1,0,2)
    y[i] = 3 + 1*x[i] + epsilon[i]
  }
  iv = ivreg(y~x|x+ x_1+x_2)
  ivs = summary(iv)
  beta_0[k] = iv[["coefficients"]][["(Intercept)"]]
  beta_1[k] = iv[["coefficients"]][["x"]]
  se_beta_0[k] = ivs[["coefficients"]][1,2]
  se_beta_1[k] = ivs[["coefficients"]][2,2]
}
avg_se_beta_0 = mean(se_beta_0)
avg_se_beta_1 = mean(se_beta_1)
e_se_beta_0 = sd(beta_0)
e_se_beta_1 = sd(beta_1)