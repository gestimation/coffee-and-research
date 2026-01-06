#--------------------------------------------------------------------------------------------------
#- Study design II. Data Have Types: A Coffee-Chat Guide to R Functions for Common Outcomes--------
# 
#  Original article (En): https://gestimation.github.io/coffee-and-research/en/study-design-2.html
#  Original article (JP): https://gestimation.github.io/coffee-and-research/jp/study-design-2.html
#  Required packages: ggplot2, cifmodeling --------------------------------------------------------
#--------------------------------------------------------------------------------------------------

#- Generation of data frame "dat" --------------------------
set.seed(46)

# Stoma: 1 = with stoma, 0 = without stoma
stoma <- rbinom(200, size = 1, prob = 0.4)

# Sex: 0 = WOMAN, 1 = MAN
sex <- rbinom(200, size = 1, prob = 0.5)

# Age: normal distribution (stoma group slightly older)
age <- rnorm(200, mean = 65 + 3 * stoma, sd = 8)

# Survival time: exponential distribution
# with expected survival 10 years (with stoma) vs 15 years (without)
hazard <- ifelse(stoma == 1, 1 / 10, 1 / 15)
time   <- rexp(200, rate = hazard)

# Random censoring: 0 = censored, 1 = event
status <- rbinom(200, size = 1, prob = 0.9)

dat <- data.frame(
  age    = age,
  sex    = factor(sex, levels = c(0, 1), labels = c("WOMAN", "MAN")),
  stoma  = factor(stoma, levels = c(0, 1),
                  labels = c("WITHOUT STOMA", "WITH STOMA")),
  time   = time,
  status = status
)

head(dat)

#- Analysis of age -----------------------------------------

# install.packages("ggplot2") # if needed
library(ggplot2)

ggplot(dat, aes(x = age, fill = stoma)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 10) +
  labs(x = "AGE", y = "FREQUENCY", fill = "STOMA") +
  theme_minimal()

#- Analysis of sex -----------------------------------------
table(STOMA = dat$stoma, SEX = dat$sex)

#- Analysis of time and status using cifplot() -------------
# install.packages("cifmodeling") # if needed
library(cifmodeling)

cifplot(Event(time, status) ~ stoma,
        data         = dat,
        outcome.type = "survival"
)

#--------------------------------------------------------------------------------------------------
#- Study design IV. A First Step into Survival and Competing Risks Analysis with R ----------------
# 
#  Original article (En): https://gestimation.github.io/coffee-and-research/en/study-design-4.html
#  Original article (JP): https://gestimation.github.io/coffee-and-research/jp/study-design-4.html
#  Required packages: cifmodeling -----------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

#- Definition of function ----------------------------------
generate_data <- function(n = 200, hr1, hr2, hr3) {
  # Stoma: 1 = with stoma, 0 = without stoma
  stoma <- rbinom(n, size = 1, prob = 0.4)
  # Sex: 0 = WOMAN, 1 = MAN
  sex <- rbinom(n, size = 1, prob = 0.5)
  # Age: normal distribution (stoma group slightly older)
  age <- rnorm(n, mean = 65 + 3 * stoma, sd = 8)
  
  # Hazards for relapse and death (larger hazard implies earlier event)
  hazard_relapse   <- 0.10*exp(stoma*log(hr1)+age*log(hr2))
  hazard_death     <- ifelse(stoma == 1, hr3 * 0.10, 0.10)
  hazard_censoring <- 0.05

  # Latent times to relapse, death, and censoring
  t_relapse   <- rexp(n, rate = hazard_relapse)
  t_death     <- rexp(n, rate = hazard_death)
  t_censoring <- rexp(n, rate = hazard_censoring)
  
  # Overall survival (OS)
  # status_os = 1 → death (event of interest)
  # status_os = 0 → censored
  time_os   <- pmin(t_death, t_censoring)
  status_os <- as.integer(t_death <= t_censoring)  # 1 = death, 0 = censored
  
  # Relapse-free survival (RFS)
  # status_rfs = 1 → relapse or death whichever comes first (event of interest)
  # status_rfs = 0 → censored
  time_rfs   <- pmin(t_relapse, t_death, t_censoring)
  status_rfs <- integer(n)
  status_rfs[time_rfs == t_relapse & time_rfs < t_censoring] <- 1  # relapse
  status_rfs[time_rfs == t_death   & time_rfs < t_censoring] <- 1  # death
  
  # Cumulative incidence of relapse (CIR)
  # status_cir = 1 → relapse (event of interest)
  # status_cir = 2 → death as competing risk
  # status_cir = 0 → censored
  
  time_cir <- pmin(t_relapse, t_death, t_censoring)
  status_cir <- integer(n)
  status_cir[time_cir == t_relapse & time_cir < t_censoring] <- 1
  status_cir[time_cir == t_death   & time_cir < t_censoring] <- 2
  
  data.frame(
    id         = 1:n,
    sex        = factor(sex, levels = c(0, 1), labels = c("WOMAN", "MAN")),
    age        = age,
    stoma      = factor(stoma, levels = c(0, 1),
                        labels = c("WITHOUT STOMA", "WITH STOMA")),
    time_os    = time_os,
    status_os  = status_os,
    time_rfs   = time_rfs,
    status_rfs = status_rfs,
    time_cir   = time_cir,
    status_cir = status_cir
  )
}

#- Generation of data frame "dat" --------------------------
set.seed(46)
dat <- generate_data(hr1 = 2, hr2 = 1, hr3 = 1.5)

#- Analysis of OS using cifplot() --------------------------
# install.packages("cifmodeling") # if needed
library(cifmodeling)

cifplot(
  Event(time_os, status_os) ~ stoma,
  data         = dat,
  outcome.type = "survival",
  label.y      = "Overall survival"
)

#- Analysis of RFS using cifplot() -------------------------
cifplot(
  Event(time_rfs, status_rfs) ~ stoma,
  data         = dat,
  outcome.type = "survival",
  label.y      = "Relapse-free survival"
)

#- Analysis of CIR using cifplot() -------------------------
cifplot(
  Event(time_cir, status_cir) ~ stoma,
  data         = dat,
  outcome.type = "competing-risk",
  label.y      = "Cumulative incidence of relapse"
)

