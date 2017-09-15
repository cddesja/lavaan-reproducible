# ------------------------
# On the Practical Interpretability of Cross-Lagged Panel Models:
# Rethinking a Developmental Workhorse
#
# Berry & Willoughby
#
# Child Development 2016
# doi.org/10.1111/cdev.12660
# ------------------------
library(lavaan)
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

# Example 1
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
agg.s ~ 1
spank.s ~ 1

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

e.spank15 ~ p3*e.spank12 + p2*e.agg12
e.spank12 ~ p3*e.spank11 + p2*e.agg11
e.spank11 ~ p3*e.spank10 + p2*e.agg10

e.agg15 ~ p1*e.spank12 + p4*e.agg12
e.agg12 ~ p1*e.spank11 + p4*e.agg11
e.agg11 ~ p1*e.spank10 + p4*e.agg10

e.spank10 ~~ e.agg10
e.spank11 ~~ p1000*e.agg11
e.spank12 ~~ p1000*e.agg12
e.spank15 ~~ p1000*e.agg15
'
fit.altsr <- lavaan(altsr.mod, sample.mean = MEANS, sample.nobs = 290, sample.cov = lansford.cov)
summary(fit.altsr, fit.measures = T, standardized = T)
