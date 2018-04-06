# ------------------------------------------------
# Assessing factorial invariance in ordered-categorical measures
# 
# Millsap & Yun-Tein (2004)
#
# Multivariate Behavioral Research
# ------------------------------------------------

library("lavaan")
library("MASS")

# Generating data using Appendix B ----

# setting up conditions of the simulation
n1 <- 5000
n2 <- 5000
p <- 6
mu.vec <- rep(0, 6)
sigma.matrix <- diag(6)
lambda.vec <- c(.4, .5, .6, .4, .5, .6)
tau.vec <- rep(c(.25, .50, .70), each = 2)
nu <- matrix(c(-.45, .25, .95,
                           -.45, .25, .95,
                           -.30, .50, 1.30,
                           -.20, .50, 1.20,
                           0, .70, 1.40,
                           -.10, .70, 1.50), byrow = TRUE, ncol = 3)
theta <- .30
nu.two <- matrix(c(-.45, .25, .75,
                            -.45, .55, 1.10,
                            -.30, .70, 1.30,
                            -.20, .50, 1.10,
                            0, .40, 1.30,
                            -.10, .50, 1.60), byrow = TRUE, ncol = 3)
theta.three <- .49

# step 1, generating a random factor score from a normal density
set.seed(31512) # set seed for reproducibility
grp1.xi <- rnorm(n1, mean = 0, sd = sqrt(1))
grp2.xi <- rnorm(n2, mean = .25, sd = sqrt(1.2))

# step 2, generate p x 1 random vector 
grp1.mu <- mvrnorm(5000, mu = mu.vec, Sigma = sigma.matrix)
grp2.mu <- mvrnorm(5000, mu = mu.vec, Sigma = sigma.matrix)

# step 3, calculate latent response variates
# for true models one and two
x.star1.mod1 <- sweep(grp1.xi %*% t(lambda.vec), 2, tau.vec, "+") + sqrt(theta) * grp1.mu
x.star2.mod1 <- sweep(grp2.xi %*% t(lambda.vec), 2, tau.vec, "+") + sqrt(theta) * grp2.mu

# true model three and group 2
x.star2.mod3 <- sweep(grp2.xi %*% t(lambda.vec), 2, tau.vec, "+") + sqrt(theta.three) * grp2.mu

# step 4, calculate observed ordinal variables 
xObsFromThreshold <- function(dat, thres){
  xobsMat <- matrix(NA, nrow = nrow(dat), ncol = ncol(dat))
  for(i in 1:6){
  xobsMat[, i] <- ifelse(dat[, i] < thres[i, 1], 0,
                         ifelse(dat[, i] >= thres[i, 1] & dat[, i] < thres[i, 2], 1,
                                ifelse(dat[, i] >= thres[i, 2] & dat[, i] < thres[i, 3], 2, 3)))
  }
  return(xobsMat)
}
# true model one - group 1 & 2
x.obs1.mod1 <- xObsFromThreshold(x.star1.mod1, nu)
x.obs2.mod1 <- xObsFromThreshold(x.star2.mod1, nu)

# true model two - group 2
x.obs2.mod2 <- xObsFromThreshold(x.star2.mod1, nu.two)

# true model three
x.obs2.mod3 <- xObsFromThreshold(x.star2.mod3, nu)

# combine the data sets and convert variables to ordinal
true.m1 <- data.frame(rbind(x.obs1.mod1, x.obs2.mod1))
true.m1$group <- rep(1:2, each = 5000)

true.m2 <- data.frame(rbind(x.obs1.mod1, x.obs2.mod2))
true.m2$group <- rep(1:2, each = 5000)

true.m3 <- data.frame(rbind(x.obs1.mod1, x.obs2.mod3))
true.m3$group <- rep(1:2, each = 5000)

true.m1[,1:6] <- lapply(true.m1[, 1:6], ordered)
true.m2[,1:6] <- lapply(true.m2[, 1:6], ordered)
true.m3[,1:6] <- lapply(true.m3[, 1:6], ordered)

# Run the models in lavaan ----

# define the model
mod <- '
fac1 =~ X1 + X2 + X3 + X4 + X5 + X6
'

# true model one
baseline.fit1 <- cfa(mod, true.m1, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus")
loadings.fit1 <- cfa(mod, true.m1, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus", group.equal = "loadings")
thresholds.fit1 <- cfa(mod, true.m1, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus", group.equal = c("loadings", "thresholds"))
residuals.fit1 <- cfa(mod, true.m1, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus", group.equal = c("loadings", "thresholds", "residuals"))

table3 <- rbind(fitMeasures(baseline.fit1, 
                          fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")),
              fitMeasures(loadings.fit1,
                          fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")),
              fitMeasures(thresholds.fit1,
                          fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")),
              fitMeasures(residuals.fit1,
                          fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")))
rownames(table3) <- c("Baseline", "Invariant - Loadings", "Invariant - Loadings & Thresholds", "Invariant - Loadings, Thresholds, & Residuals")
table3
anova(baseline.fit1, loadings.fit1)
anova(loadings.fit1, thresholds.fit1)
anova(thresholds.fit1, residuals.fit1) # full invariance found!
summary(residuals.fit1, standardized = TRUE, fit.measures = TRUE)

# true model two
baseline.fit2 <- cfa(mod, true.m2, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus")
loadings.fit2 <- cfa(mod, true.m2, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus", group.equal = "loadings")
thresholds.fit2 <- cfa(mod, true.m2, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus", group.equal = c("loadings", "thresholds"))
residuals.fit2 <- cfa(mod, true.m2, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus", group.equal = c("loadings", "thresholds", "residuals"))

table5 <- rbind(fitMeasures(baseline.fit2, 
                            fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")),
                fitMeasures(loadings.fit2,
                            fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")),
                fitMeasures(thresholds.fit2,
                            fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")),
                fitMeasures(residuals.fit2,
                            fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")))
rownames(table5) <- c("Baseline", "Invariant - Loadings", "Invariant - Loadings & Thresholds", "Invariant - Loadings, Thresholds, & Residuals")
table5
anova(baseline.fit2, loadings.fit2)
anova(loadings.fit2, thresholds.fit2) # invariance only of loading found!
anova(thresholds.fit2, residuals.fit2)
summary(loadings.fit2, standardized = TRUE, fit.measures = TRUE)

# true model three
baseline.fit3 <- cfa(mod, true.m3, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus")
loadings.fit3 <- cfa(mod, true.m3, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus", group.equal = "loadings")
thresholds.fit3 <- cfa(mod, true.m3, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus", group.equal = c("loadings", "thresholds"))
residuals.fit3 <- cfa(mod, true.m3, estimator = "WLS", group = "group", parameterization = "theta", mimic = "Mplus", group.equal = c("loadings", "thresholds", "residuals"))

table7 <- rbind(fitMeasures(baseline.fit3, 
                            fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")),
                fitMeasures(loadings.fit3,
                            fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")),
                fitMeasures(thresholds.fit3,
                            fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")),
                fitMeasures(residuals.fit3,
                            fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi")))
rownames(table7) <- c("Baseline", "Invariant - Loadings", "Invariant - Loadings & Thresholds", "Invariant - Loadings, Thresholds, & Residuals")
table7
anova(baseline.fit3, loadings.fit3)
anova(loadings.fit3, thresholds.fit3)
anova(thresholds.fit3, residuals.fit3) # invariance of thresholds found!
summary(thresholds.fit3, standardized = TRUE, fit.measures = TRUE)



