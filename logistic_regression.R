## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("~/Downloads/logistic_regression/dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

# Create data subset and examine it
ev.age.mar <- subset(NH11[c("everwrk", "age_p", "r_maritl")])
summary(ev.age.mar)
str(ev.age.mar)
levels(ev.age.mar$everwrk)

# Fill in missing values with multiple imputation
library(mice)
imputed <- complete(mice(ev.age.mar))
summary(imputed)

# Copy back into original data frame

NH11$everwrk <- imputed$everwrk
summary(NH11)

# Create dummy variable "work" derived from everwrk to simplify to 1 = Yes and 2 = No

library(tidyr)
library(dplyr)
NH11 <- NH11 %>% 
  mutate(work = case_when(everwrk == "1 Yes" ~ "1",
                                      everwrk != "1 Yes" ~ "0"))

# Make sure work is a factor variable

NH11$work <- as.factor(NH11$work)

# Create logistic regression

ev.log <- glm(work~age_p+r_maritl, data=NH11, family="binomial")
summary(ev.log)

## Coefficients are significantly negative for those widowed and never married
## Coefficients are significantly positive for those divorced and living with their partner

# Convert to log odds to odds

ev.log.tab <- coef(summary(ev.log))
ev.log.tab[, "Estimate"] <- exp(coef(ev.log))
ev.log.tab

# Convert odds to probability

ev.log.prob <- ev.log.tab / (1 + ev.log.tab)
ev.log.prob

## Predicted probability of working for each level of marital status:
# Married (spouse not in household) - 48%
# Widowed - 32%
# Divorced - 67%
# Separated - 53%
# Never married - 42%
# Living with partner - 59%
# Unkown marital status - 51%

## Probability of working is highest for those who have been divorced and are living with their partner.
## It is lowest for those who are widowed and have never been married.
## Perhaps a factor to be considered (which is not in the data) is the interplay of an individual's level of choice, autonomy, and also experiencing (or having experienced) the added responsibility that comes with partnership.