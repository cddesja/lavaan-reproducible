# ------------------------------------------------
# A Non-arbitrary method of identifying and scaling
# latent variables in SEM and MACS models.
#
# Little, Slegers, & Card 
#
# Structural Equationg Modeling 2006
# ------------------------------------------------

library("lavaan")

# Data ----
# data were available as mean vectors, standard deviation vectors
# and covariance matrices of the indicators by group 

# 7th graders
n7 <- 380
means7 <- c(3.13552, 2.99061, 3.06945, 1.70069, 1.52705, 1.54483)
sd7 <- c(0.66770, 0.68506, 0.70672, 0.71418, 0.66320, 0.65276)
lower7 <- '
1.00000
0.75854 1.00000
0.76214 0.78705 1.00000
0.02766 0.00973 -.05762 1.00000
-.06112 -.06105 -.14060 0.78501 1.00000
-.02222 -.05180 -.10250 0.81616 0.81076 1.00000'
seventh.cor <- getCov(lower7, names = c("PosAFF1", "PosAFF2", "PosAFF3", "NegAFF1", "NegAFF2", "NegAFF3"))
seventh.cov <- (sd7 %*% t(sd7)) * seventh.cor

n8 <- 379
means8 <- c(3.07338, 2.84716, 2.97882, 1.71700, 1.57955, 1.55001)
sd8 <- c(0.70299, 0.71780, 0.76208, 0.65011, 0.60168, 0.61420)
lower8 <- '
1.00000
0.81366 1.00000
0.84980 0.83523 1.00000
-0.18804 -0.15524 -0.21520 1.00000
-0.28875 -0.24951 -0.33769 0.78418 1.00000
-0.29342 -0.21022 -0.30553 0.79952 0.83156 1.00000'
eighth.cor <- getCov(lower8, names = c("PosAFF1", "PosAFF2", "PosAFF3", "NegAFF1", "NegAFF2", "NegAFF3"))
eighth.cov <- (sd8 %*% t(sd8)) * eighth.cor

# Combine data into lists ----
cov.grades <- list(seventh.cov, eighth.cov)
mean.grades <- list(means7, means8)
n.grades <- list(n7, n8)

# Method 1: Fixed latent parameters
method1.mod <- '
group: 1
# Measurement Model
Pos =~ PosAFF1 + PosAFF2 + PosAFF3
Neg =~ NegAFF1 + NegAFF2 + NegAFF3

# Intercepts
PosAFF1 ~ 1
PosAFF2 ~ 1
PosAFF3 ~ 1
NegAFF1 ~ 1
NegAFF2 ~ 1
NegAFF3 ~ 1

# Latent Means
Pos ~ 0*1
Neg ~ 0*1

# Latent Covariances
Pos ~~ 1*Pos
Neg ~~ 1*Neg
Pos ~~ Neg

# Residual Variances
PosAFF1 ~~ PosAFF1
PosAFF2 ~~ PosAFF2
PosAFF3 ~~ PosAFF3
NegAFF1 ~~ NegAFF1
NegAFF2 ~~ NegAFF2
NegAFF3 ~~ NegAFF3

group: 2
# Measurement Model
Pos =~ PosAFF1 + PosAFF2 + PosAFF3
Neg =~ NegAFF1 + NegAFF2 + NegAFF3

# Intercepts
PosAFF1 ~ 1
PosAFF2 ~ 1
PosAFF3 ~ 1
NegAFF1 ~ 1
NegAFF2 ~ 1
NegAFF3 ~ 1

# Latent Means
Pos ~ 1
Neg ~ 1

# Latent Covariances
Pos ~~ Pos
Neg ~~ Neg
Pos ~~ Neg

# Residual Variances
PosAFF1 ~~ PosAFF1
PosAFF2 ~~ PosAFF2
PosAFF3 ~~ PosAFF3
NegAFF1 ~~ NegAFF1
NegAFF2 ~~ NegAFF2
NegAFF3 ~~ NegAFF3
'
method1.fit <- lavaan(method1.mod, sample.cov = cov.grades, sample.nobs = n.grades, sample.mean = mean.grades, group.equal = c("loadings", "intercepts"))
summary(method1.fit, standardized = TRUE)

# Method 2a: Fixed marker variable, lowest variance indicators ----
method2a.mod <- '
# Measurement Model
Pos =~ 1*PosAFF1 + PosAFF2 + PosAFF3
Neg =~ NegAFF1 + 1*NegAFF2 + NegAFF3

# Intercepts
PosAFF1 ~ 0*1
PosAFF2 ~ 1
PosAFF3 ~ 1
NegAFF1 ~ 1
NegAFF2 ~ 0*1
NegAFF3 ~ 1

# Latent Means
Pos ~ 1
Neg ~ 1

# Latent Covariances
Pos ~~ Pos
Neg ~~ Neg
Pos ~~ Neg

# Residual Variances
PosAFF1 ~~ PosAFF1
PosAFF2 ~~ PosAFF2
PosAFF3 ~~ PosAFF3

NegAFF1 ~~ NegAFF1
NegAFF2 ~~ NegAFF2
NegAFF3 ~~ NegAFF3
'
method2a.fit <- lavaan(method2a.mod, sample.cov = cov.grades, sample.nobs = n.grades, sample.mean = mean.grades, group.equal = c("loadings", "intercepts"))
summary(method2a.fit, standardized = TRUE)

# Method 2b: Fixed marker variable, middle variance indicators ----
method2b.mod <- '
# Measurement Model
Pos =~ PosAFF1 + 1*PosAFF2 + PosAFF3
Neg =~ NegAFF1 + NegAFF2 + 1*NegAFF3

# Intercepts
PosAFF1 ~ 1
PosAFF2 ~ 0*1
PosAFF3 ~ 1
NegAFF1 ~ 1
NegAFF2 ~ 1
NegAFF3 ~ 0*1

# Latent Means
Pos ~ 1
Neg ~ 1

# Latent Covariances
Pos ~~ Pos
Neg ~~ Neg
Pos ~~ Neg

# Residual Variances
PosAFF1 ~~ PosAFF1
PosAFF2 ~~ PosAFF2
PosAFF3 ~~ PosAFF3

NegAFF1 ~~ NegAFF1
NegAFF2 ~~ NegAFF2
NegAFF3 ~~ NegAFF3
'
method2b.fit <- lavaan(method2b.mod, sample.cov = cov.grades, sample.nobs = n.grades, sample.mean = mean.grades, group.equal = c("loadings", "intercepts"))
summary(method2b.fit, standardized = TRUE)

# Method 2c: Fixed marker variable, highest variance indicators ----
method2c.mod <- '
# Measurement Model
Pos =~ PosAFF1 + PosAFF2 + 1*PosAFF3
Neg =~ 1*NegAFF1 + NegAFF2 + NegAFF3

# Intercepts
PosAFF1 ~ 1
PosAFF2 ~ 1
PosAFF3 ~ 0*1
NegAFF1 ~ 0*1
NegAFF2 ~ 1
NegAFF3 ~ 1

# Latent Means
Pos ~ 1
Neg ~ 1

# Latent Covariances
Pos ~~ Pos
Neg ~~ Neg
Pos ~~ Neg

# Residual Variances
PosAFF1 ~~ PosAFF1
PosAFF2 ~~ PosAFF2
PosAFF3 ~~ PosAFF3

NegAFF1 ~~ NegAFF1
NegAFF2 ~~ NegAFF2
NegAFF3 ~~ NegAFF3
'
method2c.fit <- lavaan(method2c.mod, sample.cov = cov.grades, sample.nobs = n.grades, sample.mean = mean.grades, group.equal = c("loadings", "intercepts"))
summary(method2c.fit, standardized = TRUE)

# Method 3: Effects-coding constraints
method3.mod <- '
# Measurement Model
Pos =~ lp1*PosAFF1 + lp2*PosAFF2 + lp3*PosAFF3
Neg =~ ln1*NegAFF1 + ln2*NegAFF2 + ln3*NegAFF3

# Intercepts
PosAFF1 ~ tp1*1
PosAFF2 ~ tp2*1
PosAFF3 ~ tp3*1
NegAFF1 ~ tn1*1
NegAFF2 ~ tn2*1
NegAFF3 ~ tn3*1

# Latent Means
Pos ~ 1
Neg ~ 1

# Latent Covariances
Pos ~~ Pos
Neg ~~ Neg
Pos ~~ Neg

# Residual Variances
PosAFF1 ~~ PosAFF1
PosAFF2 ~~ PosAFF2
PosAFF3 ~~ PosAFF3

NegAFF1 ~~ NegAFF1
NegAFF2 ~~ NegAFF2
NegAFF3 ~~ NegAFF3

# Define constrains for effects-coding
lp1 == 3 - lp2 - lp3
ln1 == 3 - ln2 - ln3

tp1 == 0 - tp2 - tp3
tn1 == 0 - tn2 - tn3
'
method3.fit <- lavaan(method3.mod, sample.cov = cov.grades, sample.nobs = n.grades, sample.mean = mean.grades, group.equal = c("loadings", "intercepts"))
summary(method3.fit, standardized = TRUE)

# Show that the models are equivalent ----
compare.fit <- rbind(
  fitmeasures(method1.fit, fit.measures = c("TLI", "CFI", "RMSEA")),
  fitmeasures(method2a.fit, fit.measures = c("TLI", "CFI", "RMSEA")),
  fitmeasures(method2b.fit, fit.measures = c("TLI", "CFI", "RMSEA")),
  fitmeasures(method2c.fit, fit.measures = c("TLI", "CFI", "RMSEA")),
  fitmeasures(method3.fit, fit.measures = c("TLI", "CFI", "RMSEA")))

rownames(compare.fit) <- c("Method 1", paste0("Method 2", c("a", "b", "c")), "Method 3")
compare.fit
