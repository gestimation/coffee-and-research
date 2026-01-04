#---------------------------------------------------------------------------------------------------------
#- AI & R workflow I − Getting Started: AI-Assisted, Independently Validated Clinical Research Analyses --
# 
#  Original article (En): https://gestimation.github.io/coffee-and-research/en/ai-r-1.html
#  Original article (JP): https://gestimation.github.io/coffee-and-research/jp/ai-r-1.html
#  Required packages: dplyr, gtsummary, tableone ---------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

#- Loading data frame "diabetes.complications" as "dat" ----
# install.packages("cifmodeling") # if needed
library(cifmodeling)
data(diabetes.complications)
dat <- diabetes.complications


#- Mean using aggregate() ------------------------------
out1 <- aggregate(age ~ fruitq1, data = dat, FUN = mean)
print(out1)


#- Data check using head() and sapply() --------------------
head(dat)
sapply(dat, class)


#- Missing check using sum() and sapply() ------------------
dat$fruit_group <- factor(
  dat$fruitq1,
  levels = c(0, 1),
  labels = c("High intake", "Low intake")
)
exposure_var <- "fruit_group"
outcome_var <- c("t", "epsilon")
continuous_var <- c(
  "age","bmi","hba1c","diabetes_duration",
  "sbp","ldl","hdl","tg","ltpa"
)
binary_var <- c(
  "sex","drug_oha","drug_insulin",
  "current_smoker","alcohol_drinker"
)
dat[binary_var] <- lapply(dat[binary_var], function(x) factor(x, levels = c(0,1)))

colSums(is.na(dat[, c(exposure_var, outcome_var)]))
colSums(is.na(dat[, continuous_var]))
colSums(is.na(dat[, binary_var]))


#- Making Table 1 using tbl_summary() ----------------------
# install.packages("gtsummary") # if needed
library(gtsummary)
# install.packages("dplyr") # if needed
library(dplyr)

table1 <- dat[, c(exposure_var, continuous_var, binary_var)] %>%
  tbl_summary(
    by = exposure_var,
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    )
  )
print(table1)


#- Improving Table 1 using tbl_summary() -------------------
table1 <- dat[, c(exposure_var, continuous_var, binary_var)] %>%
  tbl_summary(
    by = exposure_var,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    label = list(
      age ~ "Age",
      sex ~ "Women",
      bmi ~ "BMI",
      hba1c ~ "HbA1c",
      diabetes_duration ~ "Diabetes duration",
      drug_oha ~ "Oral Hypoglycemic agents",
      drug_insulin ~ "Insulin use",
      sbp ~ "Systolic Blood pressure",
      ldl ~ "LDL cholesterol",
      hdl ~ "HDL cholesterol",
      tg ~ "Triglycerides",
      current_smoker ~ "Current smoker",
      alcohol_drinker ~ "Alcohol drinker",
      ltpa ~ "Leisure-time physical activity"
    )
  ) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  add_overall()
print(table1)


#- Validating Table 1 using CreateTableOne() ---------------
# install.packages("tableone") # if needed
library(tableone)
# install.packages("dplyr") # if needed
library(dplyr)

# Prepare data with same labeling
dat_check <- diabetes.complications %>%
  select(all_of(continuous_var), all_of(binary_var), fruitq1) %>%
  mutate(fruitq1 = factor(fruitq1, levels = c(0, 1), labels = c("High intake", "Low intake")))

# Create summary table using tableone
table2 <- CreateTableOne(vars = c(continuous_var, binary_var), strata = "fruitq1", data = dat_check, test = TRUE)
print(table2, showAllLevels = TRUE)


#---------------------------------------------------------------------------------------------------------
#- AI & R workflow II − R Demonstration of Bias in Kaplan-Meier Under Competing Risks --------------------
# 
#  Original article (En): https://gestimation.github.io/coffee-and-research/en/ai-r-2.html
#  Original article (JP): https://gestimation.github.io/coffee-and-research/jp/ai-r-2.html
#  Required packages: survival ---------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

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


#- Aalen-Johansen curves of CIR using cifplot() ------------
# install.packages("cifmodeling") # if needed
library(cifmodeling)
aj_event1 <- cifplot(Event(time_cir, status_cir) ~ stoma,
                     data         = dat,
                     outcome.type = "competing-risk", 
                     type.y       = "surv",
                     label.y      = "1-Aalen-Johansen",
                     code.event1  = 1, 
                     code.event2  = 2
)
aj_event2 <- cifplot(Event(time_cir, status_cir) ~ stoma,
                     data         = dat,
                     outcome.type = "competing-risk", 
                     type.y       = "risk",
                     label.y      = "Aalen-Johansen",
                     code.event1  = 2, 
                     code.event2  = 1
)
aj_list <- list(aj_event1$plot, aj_event2$plot)
aj_panel <- cifpanel(rows.columns.panel = c(1,2), plots=aj_list)
print(aj_panel)


#- Kaplan-Meier curves of CIR using cifplot() --------------
dat$status_cir1 <- as.numeric(dat$status_cir==1)
km_event1 <- cifplot(Event(time_cir, status_cir1) ~ stoma,
                     data         = dat,
                     outcome.type = "survival", 
                     type.y       = "surv",
                     label.y      = "Kaplan-Meier"
)
dat$status_cir2 <- as.numeric(dat$status_cir==2)
km_event2 <- cifplot(Event(time_cir, status_cir2) ~ stoma,
                     data         = dat,
                     outcome.type = "survival", 
                     type.y       = "risk",
                     label.y      = "1-Kaplan-Meier"
)
km_list <- list(km_event1$plot, km_event2$plot)
km_panel <- cifpanel(rows.columns.panel = c(1,2), plots=km_list)
print(km_panel)


#---------------------------------------------------------------------------------------------------------
#- AI & R workflow III − Unadjusted vs Adjusted Cumulative Incidence Curves with AI & R ------------------
# 
#  Original article (En): https://gestimation.github.io/coffee-and-research/en/ai-r-3.html
#  Original article (JP): https://gestimation.github.io/coffee-and-research/jp/ai-r-3.html
#  Required packages: cobalt, cifmodeling, WeightIt, gtsummary, survival ---------------------------------
#---------------------------------------------------------------------------------------------------------

#- Analysis using dagitty() --------------------------------
# install.packages("dagitty") # if needed
library(dagitty)
out1 <- dagitty("dag {
age -> fruitq1
age -> t
sex -> fruitq1
sex -> t
bmi -> fruitq1
bmi -> t
hba1c -> fruitq1
hba1c -> t
diabetes_duration -> fruitq1
diabetes_duration -> t
drug_oha -> fruitq1
drug_oha -> t
drug_insulin -> fruitq1
drug_insulin -> t
sbp -> fruitq1
sbp -> t
ldl -> fruitq1
ldl -> t
hdl -> fruitq1
hdl -> t
tg -> fruitq1
tg -> t
current_smoker -> fruitq1
current_smoker -> t
alcohol_drinker -> fruitq1
alcohol_drinker -> t
ltpa -> fruitq1
ltpa -> t
fruitq1 -> t
}")
plot(out1)
out2 <- adjustmentSets(out1, exposure = "fruitq1", outcome = "t")
print(out2)


#- Loading data frame "diabetes.complications" as "dat" ----
# install.packages("cifmodeling") # if needed
library(cifmodeling)
data(diabetes.complications)
dat <- diabetes.complications
stopifnot(is.data.frame(dat))
stopifnot(all(c("t","epsilon","fruitq1") %in% names(dat)))


#- Description of epsilon and t in data frame "dat" --------
table(dat$epsilon, useNA = "ifany")
summary(dat$t)
tapply(dat$t, dat$epsilon, summary)


#- Specification of a model for propensity score -----------
continuous_var <- c("age","bmi","hba1c","diabetes_duration","sbp","ldl","hdl","tg","ltpa")
binary_var <- c("sex","drug_oha","drug_insulin","current_smoker","alcohol_drinker")
exposure.model <- as.formula(paste("fruitq1 ~", paste(continuous_var, collapse = " + "), "+", paste(binary_var, collapse = " + ")))


#- Calculation of inverse probability weights --------------
# install.packages("WeightIt") # if needed
library(WeightIt)

out2 <- weightit(exposure.model, data = dat, method = "cbps")
dat$ip.weight <- out2$weights
dat$one <- 1
quantile(dat$ip.weight, probs = c(0, .01, .05, .5, .95, .99, 1))


#- Diagnosis of calculated inverse probability weights -----
# install.packages("cobalt") # if needed
library(cobalt)

out2$covs$ps <- out2$ps
bal.plot(out2, var.name = "ps", which = "both")  # unadjusted vs adjusted

out3 <- bal.tab(
  exposure.model,
  data    = dat,
  weights = dat$ip.weight,
  method  = "weighting",
  un      = TRUE,
  m.threshold = 0.1
)
out3
love.plot(
  out3,
  stats      = "mean.diffs",
  stars      = "raw", 
  abs        = TRUE,
  thresholds = c(m = 0.1),
  var.order  = "unadjusted"
)


#- Cumulative incidence curves using cifplot() -------------
cifplot(Event(t,epsilon)~fruitq1, data=dat, outcome.type="competing-risk")


#- Improving cumulative incidence curves -------------------
cifplot(Event(t,epsilon)~fruitq1, data=dat, outcome.type="competing-risk", 
        add.risktable=TRUE, add.censor.mark=FALSE, add.competing.risk.mark=TRUE,
        label.y="CIF of diabetic retinopathy", label.x="Years from registration",
        label.strata=c("High intake","Low intake"), level.strata=c(0, 1), order.strata=c(1, 0))


#- Cumulative incidence curves with weights="ip.weight" ----
cifplot(Event(t,epsilon)~fruitq1, data=dat, outcome.type="competing-risk", weights = "ip.weight")


#- Improving cumulative incidence curves -------------------
cifplot(Event(t,epsilon)~fruitq1, data=dat, outcome.type="competing-risk", 
        weights = "ip.weight", add.censor.mark=FALSE,
        label.y="CIF of diabetic retinopathy", label.x="Years from registration",
        label.strata=c("High intake","Low intake"), level.strata=c(1, 0), order.strata=c(0, 1), n.risk.type = "ess")

unadjusted <- cifplot(Event(t,epsilon)~fruitq1, data=dat, outcome.type="competing-risk", 
        add.censor.mark=FALSE, add.risktable=FALSE, label.y="Unadjusted CIF")
adjusted <- cifplot(Event(t,epsilon)~fruitq1, data=dat, outcome.type="competing-risk", 
        weights = "ip.weight", add.censor.mark=FALSE, add.risktable=FALSE, label.y="Adjusted CIF")
aj_list <- list(unadjusted$plot, adjusted$plot)
aj_panel <- cifpanel(rows.columns.panel = c(1,2), plots=aj_list)
print(aj_panel)


#- Validating cumulative incidence curves using survfit() --
# install.packages("survival") # if needed
library(survival)

compare <- function(data, weights, limit.x) {
  out1 <- cifcurve(Event(t, epsilon)~fruitq1, data=data, weights=weights, outcome.type="c", error="if")
  out2 <- survfit(Surv(t, factor(epsilon))~fruitq1, data=data, weights=data[[weights]])
  return1 <- cbind((1-out1$surv[1:limit.x]),out1$time[1:limit.x],out1$std.err[1:limit.x])
  return2 <- cbind(out2$pstate[1:limit.x,2],out2$time[1:limit.x],out2$std.err[1:limit.x])
  cbind(return1, return2)
}
compare_unweighted <- compare(data=dat, weights="one", limit.x=20)
print(compare_unweighted)
compare_weighted <- compare(data=dat, weights="ip.weight", limit.x=20)
print(compare_weighted)


#- Explanation on handling of event variable in survfit() --
out3 <- survfit(Surv(t, factor(epsilon))~fruitq1, data=dat)
names(out3)
out4 <- survfit(Surv(t, epsilon)~fruitq1, data=dat)
names(out4)
