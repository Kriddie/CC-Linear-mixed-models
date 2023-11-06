 #from Vinnett
#https://cran.r-project.org/web/packages/multilevelTools/vignettes/lmer-vignette.html
## load lme4, JWileymisc, and multilevelTools packages
## (i.e., "open the 'apps' ") 
library(lme4)
library(lmerTest)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)

## load some sample data for examples
data(aces_daily, package = "JWileymisc")

#view data structure
str(aces_daily, nchar.max = 30)

## how many unique IDs (people) are there?
length(unique(aces_daily$UserID))

## how many not missing observations of negative affect are there?
sum(!is.na(aces_daily$NegAff))

## how many not missing observations of stress are there?
sum(!is.na(aces_daily$STRESS))

summary(aces_daily$NegAff)

summary(aces_daily$STRESS)

# intraclass correlation coefficient or ICC
#The ICC is a measure of the proportion of variance that is between people versus the total variance
#The relevant output is the ICC for the row named UserID. 
#An ICC of 1 indicates that 100% of all variance exists between people, which would mean that 0% of variance exists within person, indicating that people have identical scores every time they are assessed. 
#Conversely an ICC of 0 would indicate that everyone’s average was identical and 100%
iccMixed(
  dv = "NegAff",
  id = "UserID",
  data = aces_daily)

iccMixed("STRESS", "UserID", aces_daily)

#examine the distribution of the variables visually. 
#Visual exploration is a great way to identify the distribution of variables, extreme values, and other potential issues that can be difficult to identify numerically, such as bimodal distributions. 
#For multilevel data, it is helpful to examine between and within person aspects of a variable separately. 
#multilevelTools makes this easy using the meanDecompose() function. 
#or example, if on 11 of 12 days, someone has a negative affect score of 5, and then one day a score of 1, the score of 1 may be an extreme value, for that person even though it is common for the rest of the participants in the study. 
  #meanDecompose() returns a list with X values at different levels, here by ID and the residuals, which in this case are within person effects.
tmp <- meanDecompose(NegAff ~ UserID, data = aces_daily)
str(tmp, nchar.max = 30)

plot(testDistribution(tmp[["NegAff by UserID"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Between Person Negative Affect")

plot(testDistribution(tmp[["NegAff by residual"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Within Person Negative Affect")


tmp <- meanDecompose(STRESS ~ UserID, data = aces_daily)

plot(testDistribution(tmp[["STRESS by UserID"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Between Person STRESS")

plot(testDistribution(tmp[["STRESS by residual"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Within Person STRESS")

#mixed effects, multilevel

m <- lmer(NegAff ~ STRESS + (1 + STRESS | UserID),
          data = aces_daily)


strictControl <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12))

m <- lmer(NegAff ~ STRESS + (1 + STRESS | UserID),
          data = aces_daily, control = strictControl)


md <- modelDiagnostics(m, ev.perc = .001)

plot(md, ask = FALSE, ncol = 2, nrow = 3)

mvextreme <- subset(md$extremeValues,
                    EffectType == "Multivariate Random Effect UserID")

head(mvextreme)
unique(mvextreme$UserID)

m2 <- update(m, data = subset(aces_daily,
                              UserID %!in% unique(mvextreme$UserID)))
md2 <- modelDiagnostics(m2, ev.perc = .001)

plot(md2, ask = FALSE, ncol = 2, nrow = 3)

mvextreme2 <- subset(md2$extremeValues,
                     EffectType == "Multivariate Random Effect UserID")

unique(mvextreme2$UserID)

m3 <- update(m, data = subset(aces_daily,
                              UserID %!in% c(unique(mvextreme$UserID), unique(mvextreme2$UserID))))

md3 <- modelDiagnostics(m3, ev.perc = .001)

plot(md3, ask = FALSE, ncol = 2, nrow = 3)

#fit indices for the overall model
#variance accounted for by the fixed effects (marginal R2) 
#variance accounted for by the fixed and random effects combined (conditional R2). 
#We also get information criterion (AIC, BIC), although note that with a REML estimator, the log likelihood and thus information criterion are not comparable to if the ML estimator was used.
modelPerformance(m3)

#To see the results of individual variables, we can use the summary() function to get the default model summary. 
#Note that this summary differs slightly from that produced by lme4 as it is overridden by lmerTest which adds degrees of freedom and p-values.

summary(m3)

#Confidence intervals are commonly reported 
#t values often are not reported in preference for p-values. 
#In addition, it is increasingly common to ask for effect sizes. 
#The modelTest() function in multilevelTools provides further tests, 
#including tests of the combined fixed + random effect for each variable and effect sizes based off the independent change in marginal and conditional R2, used to calculate a sort of cohen’s F2. 
#All of the results are available in a series of tables for any programattic use. However, for individual use or reporting, caling APAStyler() will produce a nicely formatted output for humans. 
#Confidence intervals are added in brackets and the effect sizes at the bottom are listed for stress considering fixed + random effects together.
mt3 <- modelTest(m3)
names(mt3)
APAStyler(mt3)
APAStyler(mt3,
          format = list(
            FixedEffects = "%s, %s (%s; %s)",
            RandomEffects = c("%s", "%s (%s, %s)"),
            EffectSizes = "%s, %s; %s"),
          digits = 3,
          pcontrol = list(digits = 3, stars = FALSE,
                          includeP = TRUE, includeSign = TRUE,
                          dropLeadingZero = TRUE))

mt <- modelTest(m)
APAStyler(list(Original = mt, `Outliers Removed` = mt3))
