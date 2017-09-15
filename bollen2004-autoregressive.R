# ------------------------------------------------
# Autoregressive Latent Trajectory (ALT) Models A Synthesis of Two Traditions 
#
# Bollen & Curran
#
# Sociological Methods & Research 2004
# doi.org/10.1177/0049124103260222
# ------------------------------------------------
library(lavaan)
n <- 500
lower <- '
.619
.453 .595
.438 .438 .587
.422 .430 .438 .595
.406 .422 .438 .453 .619
'
S <- getCov(lower, names = paste0("t", 1:5))

## Latent trajectory model, i.e., latent growth curve w/ random intercept & random slope ----
lt.model <- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5

# estimate the means
i ~ 1
s ~ 1

# estimate the variances/covariances
i ~~ i
s ~~ s
i ~~ s

# estimate the residual variances
t1 ~~ t1
t2 ~~ t2
t3 ~~ t3
t4 ~~ t4
t5 ~~ t5
'
lt.fit <- lavaan(lt.model, sample.cov = S, sample.nobs = n)
summary(lt.fit, fit.measures = T)
round(lt.fit@implied$cov[[1]], 3)

## Autoregressive model ----
ar.model <- '
t2 ~ t1
t3 ~ t2
t4 ~ t3
t5 ~ t4
t2 ~~ t2
t3 ~~ t3
t4 ~~ t4
t5 ~~ t5
'
ar.fit <- lavaan(ar.model, sample.cov = S, sample.nobs = n)
summary(ar.fit, fit.measures = T)
round(ar.fit@implied$cov[[1]], 3)

## ALT model ---
# Predetermined, i.e., T1 exogenous ----
alt.preT1.model <- '
i =~ 1*t2 + 1*t3 + 1*t4 + 1*t5
s =~ 1*t2 + 2*t3 + 3*t4 + 4*t5

# estimate the means
i ~ 1
s ~ 1
t1 ~ 1

# estimate the variances/covariances
i ~~ i
s ~~ s
i ~~ s
t1 ~~ t1
t1 ~~ i
t1 ~~ s

# estimate the residual variances
t2 ~~ e2*t2
t3 ~~ e3*t3
t4 ~~ e4*t4
t5 ~~ e5*t5

# autoregressive components
t2 ~ p2*t1
t3 ~ p3*t2
t4 ~ p4*t3
t5 ~ p5*t4
'
alt.preT1.fit <- lavaan(alt.preT1.model, sample.cov = S, sample.nobs = n)

# Replicates Table 2
summary(alt.preT1.fit, fit.measures = T)
round(inspect(alt.preT1.fit, "rsquare"), 2)

# Now, fit the latent trajectory model as a reduced ALT model 
# again with T1 predetermined
alt.lt.model <- '
i =~ 1*t2 + 1*t3 + 1*t4 + 1*t5
s =~ 1*t2 + 2*t3 + 3*t4 + 4*t5

# estimate the means
i ~ 1
s ~ 1
t1 ~ 1

# estimate the variances/covariances
i ~~ i
s ~~ s
i ~~ s
t1 ~~ t1
t1 ~~ i
t1 ~~ s

# estimate the residual variances
t2 ~~ e2*t2
t3 ~~ e3*t3
t4 ~~ e4*t4
t5 ~~ e5*t5

# autoregressive components
t2 ~ 0*t1
t3 ~ 0*t2
t4 ~ 0*t3
t5 ~ 0*t4
'
alt.lt.fit <- lavaan(alt.lt.model, sample.cov = S, sample.nobs = n)

# LRT, dfs are slightly different but statistics are identical
# to that present on p. 35
anova(alt.preT1.fit, alt.lt.fit)

# ALT model with T1 as endogenous ----
alt.model <- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5

# estimate the means
i ~ 1
s ~ 1

# estimate the variances/covariances
i ~~ i
s ~~ s
i ~~ s

# estimate the residual variances
t1 ~~ t1
t2 ~~ t2
t3 ~~ t3
t4 ~~ t4
t5 ~~ t5

# autoregressive components
t2 ~ t1
t3 ~ t2
t4 ~ t3
t5 ~ t4
'
alt.fit <- lavaan(alt.model, sample.cov = S, sample.nobs = n)
summary(alt.fit, fit.measures = T)
round(alt.model@implied$cov[[1]], 3)

## ALT with structured residuals example 
# From Berry & Willoughby (2016)
MEANS <- c(1.73, 1.66, 1.61, 1.36, 0.28, 0.28, 0.28, 0.33)
SDS <- c(0.62, 0.61, 0.59, 0.55, 0.18, 0.17, 0.18, 0.23)
LOWER <- '
1.00
0.58 1.00
0.61 0.68 1.00
0.39 0.45 0.51 1.00
0.30 0.16 0.23 0.09 1.00
0.30 0.20 0.28 0.12 0.56 1.00
0.28 0.24 0.28 0.10 0.56 0.55 1.00
0.15 0.18 0.17 0.08 0.32 0.37 0.36 1.00'
lansford.cor <- getCov(LOWER, names = c(paste0("spank", c(10:12, 15)), paste0("agg", c(10:12, 15))))
lansford.cov <- (SDS %*% t(SDS)) * lansford.cor

## ALT-SR model
altsr.mod <- '
# random intercepts
agg.i =~ 1*agg10 + 1*agg11 + 1*agg12 + 1*agg15
spank.i =~ 1*spank10 + 1*spank11 + 1*spank12 + 1*spank15
agg.i ~~ agg.i
spank.i ~~ spank.i
agg.i ~~ p10*spank.i
agg.i ~ 1
spank.i ~ 1

# random slopes
agg.s =~ 0*agg10 + 1*agg11 + 2*agg12 + 3*agg15
spank.s =~ 0*spank10 + 1*spank11 + 2*spank12 + 3*spank15

# constrain no variation in random slopes (Figure 4)
agg.s ~~ 0*agg.s
spank.s ~~ 0*spank.s
agg.s ~~ 0*agg.i
agg.s ~~ 0*spank.i
spank.s ~~ 0*agg.i
spank.s ~~ 0*spank.i

agg.s ~ p16*1
spank.s ~ p11*1

# create structured residuals
e.agg10 =~ 1*agg10
e.agg11 =~ 1*agg11
e.agg12 =~ 1*agg12
e.agg15 =~ 1*agg15

agg10 ~~ 0*agg10
agg11 ~~ 0*agg11
agg12 ~~ 0*agg12
agg15 ~~ 0*agg15

e.agg10 ~~ e.agg10
e.agg11 ~~ p1012*e.agg11
e.agg12 ~~ p1012*e.agg12
e.agg15 ~~ p1012*e.agg15

e.spank10 =~ 1*spank10
e.spank11 =~ 1*spank11
e.spank12 =~ 1*spank12
e.spank15 =~ 1*spank15

spank10 ~~ 0*spank10
spank11 ~~ 0*spank11
spank12 ~~ 0*spank12
spank15 ~~ 0*spank15

e.spank10 ~~ e.spank10
e.spank11 ~~ p1011*e.spank11
e.spank12 ~~ p1011*e.spank12
e.spank15 ~~ p1011*e.spank15

e.spank15 ~ p4*e.spank12 + p2*e.agg12
e.spank12 ~ p4*e.spank11 + p2*e.agg11
e.spank11 ~ p4*e.spank10 + p2*e.agg10

e.agg15 ~ p1*e.spank12 + p3*e.agg12
e.agg12 ~ p1*e.spank11 + p3*e.agg11
e.agg11 ~ p1*e.spank10 + p3*e.agg10

e.spank10 ~~ e.agg10
e.spank11 ~~ p1000*e.agg11
e.spank12 ~~ p1000*e.agg12
e.spank15 ~~ p1000*e.agg15
'
fit.altsr <- lavaan(altsr.mod, sample.mean = MEANS, sample.nobs = 290, sample.cov = lansford.cov)
summary(fit.altsr, fit.measures = T, standardized = T)
