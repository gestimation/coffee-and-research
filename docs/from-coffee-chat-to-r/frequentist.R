#---------------------------------------------------------------------------------------------------------
#- Frequentist Thinking II. P-Value Explanations That Seem Plausible at First Glance ---------------------
# 
#  Original article (En): https://gestimation.github.io/coffee-and-research/en/frequentist-2.html
#  Original article (JP): https://gestimation.github.io/coffee-and-research/jp/frequentist-2.html
#  Required packages: survival ---------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

#- Generation of data frame "dat" --------------------------
set.seed(46)
dat <- generate_data(hr1 = 2, hr2 = 1, hr3 = 1.5)  # true HR for death = 1.5

#- Analysis using coxph() ----------------------------------
# install.packages("survival") # if needed
library(survival)

fit <- coxph(Surv(time_os, status_os) ~ stoma, data = dat)
summary(fit)


#---------------------------------------------------------------------------------------------------------
#- Frequentist Experiments I. Understanding Confidence Intervals via Hypothetical Replications in R ------
# 
#  Original article (En): https://gestimation.github.io/coffee-and-research/en/frequentist-4.html
#  Original article (JP): https://gestimation.github.io/coffee-and-research/jp/frequentist-4.html
#  Required packages: survival ---------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

#- Definition of function ----------------------------------
calculate_coverage <- function(model = c("coxph", "finegray"),
                               n, hr1, hr2, hr3, hr_true) {
  model <- match.arg(model)
  replications <- 1000
  covered <- logical(replications)
  
  for (r in seq_len(replications)) {
    dat <- generate_data(n, hr1, hr2, hr3)
    
    if (identical(model, "coxph")) {
      fit <- coxph(Surv(time_os, status_os) ~ stoma, data = dat)
    } else if (identical(model, "finegray")) {
      # Fine-Gray model for competing risks (subdistribution hazards)
      dat$fstatus_cir <- factor(dat$status_cir,
                                levels = 0:2,
                                labels = c("censor", "relapse", "death"))
      fgdat <- finegray(Surv(time_cir, fstatus_cir) ~ ., data = dat)
      fit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ stoma,
                   weight = fgwt, cluster = id, data = fgdat)
    }
    
    ci_log <- confint(fit)  # CI for log(HR)
    covered[r] <- (ci_log[1] <= log(hr_true) && log(hr_true) <= ci_log[2])
  }
  
  mean(covered)  # estimated coverage
}

#- Simulation with n=200------------------------------------
# install.packages("survival") # if needed
library(survival)

coverage_200 <- calculate_coverage(model = "coxph", n = 200,
                                   hr1 = 2, hr2 = 1, hr3 = 1.5, hr_true = 1.5)

print(coverage_200)

#- Simulation with n=100, 400 and 800-----------------------
coverage_100 <- calculate_coverage(model = "coxph", n = 100,
                                   hr1 = 2, hr2 = 1, hr3 = 1.5, hr_true = 1.5)
coverage_400 <- calculate_coverage(model = "coxph", n = 400,
                                   hr1 = 2, hr2 = 1, hr3 = 1.5, hr_true = 1.5)
coverage_800 <- calculate_coverage(model = "coxph", n = 800,
                                   hr1 = 2, hr2 = 1, hr3 = 1.5, hr_true = 1.5)

print(coverage_100)
print(coverage_200)
print(coverage_400)
print(coverage_800)


#---------------------------------------------------------------------------------------------------------
#- Frequentist Experiments II. Alpha, Beta, and Power: The Fundamental Probabilities Behind Sample Size --
# 
#  Original article (En): https://gestimation.github.io/coffee-and-research/en/frequentist-5.html
#  Original article (JP): https://gestimation.github.io/coffee-and-research/jp/frequentist-5.html
#  Required packages: powerSurvEpi -----------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

#- Calculation with allocation ratio 1:1 -------------------
# install.packages("powerSurvEpi") # if needed
library(powerSurvEpi)

ssizeCT.default(
  power = 0.9,
  k     = 1,                      # allocation ratio (nE : nC)
  pE    = 0.6,                    # return-to-work rate in stoma group
  pC    = 0.8,                    # return-to-work rate in non-stoma group
  RR    = log(1 - 0.8) / log(1 - 0.6),  # approximate HR from return rates
  alpha = 0.05
)

#- Calculation with allocation ratio 1:2 -------------------
ssizeCT.default(
  power = 0.9,
  k     = 0.5,                  # allocation ratio 1:2
  pE    = 0.6,
  pC    = 0.8,
  RR    = log(1 - 0.8) / log(1 - 0.6),
  alpha = 0.05
)

#- Calculation with allocation ratio 1:4 -------------------
ssizeCT.default(
  power = 0.9,
  k     = 0.25,                 # allocation ratio 1:4
  pE    = 0.6,
  pC    = 0.8,
  RR    = log(1 - 0.8) / log(1 - 0.6),
  alpha = 0.05
)
