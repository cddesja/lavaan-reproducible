# This script ireplicate the Curran et al., (2014) models but using R 
# instead of Mplus
#
# Christopher Desjardins
# 
# Please email: chrisd@ori.org
# if you notice syntax errors or bugs
#
#
 
# read in the data
lcmsr.sim <- read.table("data/currandemo.dat", col.names = c("id", "gen", "trt", paste0("alc", 1:5), paste0("dep", 1:5)))

# load necessary libraries
# install.packages("lavaan") # install lavaan if you haven't.
library(lavaan)

# -------------------------------
# univariate unconditional model for alcohol
# -------------------------------

# model 1
alc.mod1 <- '
# ALCOHOL #
# random intercept
alc =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4 + 1*alc5
alc ~ 1
alc ~~ alc

# create structured residuals
alc1 ~~ 0*alc1
alc2 ~~ 0*alc2
alc3 ~~ 0*alc3
alc4 ~~ 0*alc4
alc5 ~~ 0*alc5

salc1 =~ 1*alc1
salc2 =~ 1*alc2
salc3 =~ 1*alc3
salc4 =~ 1*alc4
salc5 =~ 1*alc5

salc1 ~ 0
salc2 ~ 0
salc3 ~ 0
salc4 ~ 0
salc5 ~ 0

salc1 ~~ salc1
salc2 ~~ salc2
salc3 ~~ salc3
salc4 ~~ salc4
salc5 ~~ salc5
'

alc.fit1 <- lavaan(alc.mod1, lcmsr.sim)
# summary(alc.fit1, fit.measures = T)

# model 2
alc.mod2 <- '
# ALCOHOL #
# random intercept
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4 + 1*alc5
alc.i ~ 1
alc.i ~~ alc.i

# random slope
alc.s =~ 0*alc1 + 1*alc2 + 2*alc3 + 3*alc4 + 4*alc5
alc.s ~ 1
alc.s ~~ alc.s
alc.i ~~ alc.s

# create structured residuals
alc1 ~~ 0*alc1
alc2 ~~ 0*alc2
alc3 ~~ 0*alc3
alc4 ~~ 0*alc4
alc5 ~~ 0*alc5

salc1 =~ 1*alc1
salc2 =~ 1*alc2
salc3 =~ 1*alc3
salc4 =~ 1*alc4
salc5 =~ 1*alc5

salc1 ~ 0
salc2 ~ 0
salc3 ~ 0
salc4 ~ 0
salc5 ~ 0

salc1 ~~ salc1
salc2 ~~ salc2
salc3 ~~ salc3
salc4 ~~ salc4
salc5 ~~ salc5
'

alc.fit2 <- lavaan(alc.mod2, lcmsr.sim)
# summary(alc.fit2, fit.measures = T)
# print(anova(alc.fit1, alc.fit2))

alc.mod3 <- '
# ALCOHOL #
# random intercept
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4 + 1*alc5
alc.i ~ 1
alc.i ~~ alc.i

# random slope
alc.s =~ 0*alc1 + 1*alc2 + 2*alc3 + 3*alc4 + 4*alc5
alc.s ~ 1
alc.s ~~ alc.s
alc.i ~~ alc.s

# create structured residuals
alc1 ~~ 0*alc1
alc2 ~~ 0*alc2
alc3 ~~ 0*alc3
alc4 ~~ 0*alc4
alc5 ~~ 0*alc5

salc1 =~ 1*alc1
salc2 =~ 1*alc2
salc3 =~ 1*alc3
salc4 =~ 1*alc4
salc5 =~ 1*alc5

salc1 ~ 0
salc2 ~ 0
salc3 ~ 0
salc4 ~ 0
salc5 ~ 0

salc1 ~~ salc1
salc2 ~~ salc2
salc3 ~~ salc3
salc4 ~~ salc4
salc5 ~~ salc5

# add auto-regressive paths
salc2 ~ pyy*salc1
salc3 ~ pyy*salc2
salc4 ~ pyy*salc3
salc5 ~ pyy*salc4
'

alc.fit3 <- lavaan(alc.mod3, lcmsr.sim)
# summary(alc.fit3, fit.measures = T)
# print(anova(alc.fit3, alc.fit2))

# -------------------------------
# univariate unconditional model for depression
# -------------------------------

# model 1
dep.mod1 <- '
# DEPRESSION #
# random intercept
dep =~ 1*dep1 + 1*dep2 + 1*dep3 + 1*dep4 + 1*dep5
dep ~ 1
dep ~~ dep

# create structured residuals
dep1 ~~ 0*dep1
dep2 ~~ 0*dep2
dep3 ~~ 0*dep3
dep4 ~~ 0*dep4
dep5 ~~ 0*dep5

sdep1 =~ 1*dep1
sdep2 =~ 1*dep2
sdep3 =~ 1*dep3
sdep4 =~ 1*dep4
sdep5 =~ 1*dep5

sdep1 ~ 0
sdep2 ~ 0
sdep3 ~ 0
sdep4 ~ 0
sdep5 ~ 0

sdep1 ~~ sdep1
sdep2 ~~ sdep2
sdep3 ~~ sdep3
sdep4 ~~ sdep4
sdep5 ~~ sdep5
'

dep.fit1 <- lavaan(dep.mod1, lcmsr.sim)
# summary(dep.fit1, fit.measures = T)

# model 2
dep.mod2 <- '
# DEPRESSION #
# random intercept
dep.i =~ 1*dep1 + 1*dep2 + 1*dep3 + 1*dep4 + 1*dep5
dep.i ~ 1
dep.i ~~ dep.i

# random slope
dep.s =~ 0*dep1 + 1*dep2 + 2*dep3 + 3*dep4 + 4*dep5
dep.s ~ 1
dep.s ~~ dep.s
dep.s ~~ dep.i

# create structured residuals
dep1 ~~ 0*dep1
dep2 ~~ 0*dep2
dep3 ~~ 0*dep3
dep4 ~~ 0*dep4
dep5 ~~ 0*dep5

sdep1 =~ 1*dep1
sdep2 =~ 1*dep2
sdep3 =~ 1*dep3
sdep4 =~ 1*dep4
sdep5 =~ 1*dep5

sdep1 ~ 0
sdep2 ~ 0
sdep3 ~ 0
sdep4 ~ 0
sdep5 ~ 0

sdep1 ~~ sdep1
sdep2 ~~ sdep2
sdep3 ~~ sdep3
sdep4 ~~ sdep4
sdep5 ~~ sdep5
'

dep.fit2 <- lavaan(dep.mod2, lcmsr.sim)
# summary(dep.fit2, fit.measures = T)
# print(anova(dep.fit1, dep.fit2))

# model 3
dep.mod3 <- '
# DEPRESSION #
# random intercept
dep =~ 1*dep1 + 1*dep2 + 1*dep3 + 1*dep4 + 1*dep5
dep ~ 1
dep ~~ dep

# create structured residuals
dep1 ~~ 0*dep1
dep2 ~~ 0*dep2
dep3 ~~ 0*dep3
dep4 ~~ 0*dep4
dep5 ~~ 0*dep5

sdep1 =~ 1*dep1
sdep2 =~ 1*dep2
sdep3 =~ 1*dep3
sdep4 =~ 1*dep4
sdep5 =~ 1*dep5

sdep1 ~ 0
sdep2 ~ 0
sdep3 ~ 0
sdep4 ~ 0
sdep5 ~ 0

sdep1 ~~ sdep1
sdep2 ~~ sdep2
sdep3 ~~ sdep3
sdep4 ~~ sdep4
sdep5 ~~ sdep5

# add auto-regressive paths
sdep2 ~ pzz*sdep1
sdep3 ~ pzz*sdep2
sdep4 ~ pzz*sdep3
sdep5 ~ pzz*sdep4
'

dep.fit3 <- lavaan(dep.mod3, lcmsr.sim)
# summary(dep.fit3, fit.measures = T)
print(anova(dep.fit1, dep.fit3))

# -------------------------------
# bivariate unconditional model for alcohol & depression
# -------------------------------

# model 1
ad.mod1 <- '
# ---------------------------
# latent factors
# ---------------------------
# ALCOHOL
# random intercept
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4 + 1*alc5
alc.i ~ 1
alc.i ~~ alc.i

# random slope
alc.s =~ 0*alc1 + 1*alc2 + 2*alc3 + 3*alc4 + 4*alc5
alc.s ~ 1
alc.s ~~ alc.s
alc.i ~~ alc.s

# DEPRESSION
# random intercept
dep.i =~ 1*dep1 + 1*dep2 + 1*dep3 + 1*dep4 + 1*dep5
dep.i ~ 1
dep.i ~~ dep.i
dep.i ~~ alc.i
dep.i ~~ alc.s

# ---------------------------
# create structured residuals
# ---------------------------
# ALCOHOL
alc1 ~~ 0*alc1
alc2 ~~ 0*alc2
alc3 ~~ 0*alc3
alc4 ~~ 0*alc4
alc5 ~~ 0*alc5

salc1 =~ 1*alc1
salc2 =~ 1*alc2
salc3 =~ 1*alc3
salc4 =~ 1*alc4
salc5 =~ 1*alc5

salc1 ~ 0
salc2 ~ 0
salc3 ~ 0
salc4 ~ 0
salc5 ~ 0

salc1 ~~ salc1
salc2 ~~ salc2
salc3 ~~ salc3
salc4 ~~ salc4
salc5 ~~ salc5

# DEPRESSION
dep1 ~~ 0*dep1
dep2 ~~ 0*dep2
dep3 ~~ 0*dep3
dep4 ~~ 0*dep4
dep5 ~~ 0*dep5

sdep1 =~ 1*dep1
sdep2 =~ 1*dep2
sdep3 =~ 1*dep3
sdep4 =~ 1*dep4
sdep5 =~ 1*dep5

sdep1 ~ 0
sdep2 ~ 0
sdep3 ~ 0
sdep4 ~ 0
sdep5 ~ 0

sdep1 ~~ sdep1
sdep2 ~~ sdep2
sdep3 ~~ sdep3
sdep4 ~~ sdep4
sdep5 ~~ sdep5

salc1 ~~ sdep1
salc2 ~~ vzy*sdep2
salc3 ~~ vzy*sdep3
salc4 ~~ vzy*sdep4
salc5 ~~ vzy*sdep5

# ---------------------------
# residual regressions
# ---------------------------
# ALCOHOL
salc2 ~ pyy*salc1
salc3 ~ pyy*salc2
salc4 ~ pyy*salc3
salc5 ~ pyy*salc4

# DEPRESSION
sdep2 ~ pzz*sdep1
sdep3 ~ pzz*sdep2
sdep4 ~ pzz*sdep3
sdep5 ~ pzz*sdep4
'
ad.fit1 <- lavaan(ad.mod1, lcmsr.sim)
summary(ad.fit1, fit.measures = T)

# model 2
ad.mod2 <- '
# ---------------------------
# latent factors
# ---------------------------
# ALCOHOL
# random intercept
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4 + 1*alc5
alc.i ~ 1
alc.i ~~ alc.i

# random slope
alc.s =~ 0*alc1 + 1*alc2 + 2*alc3 + 3*alc4 + 4*alc5
alc.s ~ 1
alc.s ~~ alc.s
alc.i ~~ alc.s

# DEPRESSION
# random intercept
dep.i =~ 1*dep1 + 1*dep2 + 1*dep3 + 1*dep4 + 1*dep5
dep.i ~ 1
dep.i ~~ dep.i
dep.i ~~ alc.i
dep.i ~~ alc.s

# ---------------------------
# create structured residuals
# ---------------------------
# ALCOHOL
alc1 ~~ 0*alc1
alc2 ~~ 0*alc2
alc3 ~~ 0*alc3
alc4 ~~ 0*alc4
alc5 ~~ 0*alc5

salc1 =~ 1*alc1
salc2 =~ 1*alc2
salc3 =~ 1*alc3
salc4 =~ 1*alc4
salc5 =~ 1*alc5

salc1 ~ 0
salc2 ~ 0
salc3 ~ 0
salc4 ~ 0
salc5 ~ 0

salc1 ~~ salc1
salc2 ~~ salc2
salc3 ~~ salc3
salc4 ~~ salc4
salc5 ~~ salc5

# DEPRESSION
dep1 ~~ 0*dep1
dep2 ~~ 0*dep2
dep3 ~~ 0*dep3
dep4 ~~ 0*dep4
dep5 ~~ 0*dep5

sdep1 =~ 1*dep1
sdep2 =~ 1*dep2
sdep3 =~ 1*dep3
sdep4 =~ 1*dep4
sdep5 =~ 1*dep5

sdep1 ~ 0
sdep2 ~ 0
sdep3 ~ 0
sdep4 ~ 0
sdep5 ~ 0

sdep1 ~~ sdep1
sdep2 ~~ sdep2
sdep3 ~~ sdep3
sdep4 ~~ sdep4
sdep5 ~~ sdep5

salc1 ~~ sdep1
salc2 ~~ p1*sdep2
salc3 ~~ p1*sdep3
salc4 ~~ p1*sdep4
salc5 ~~ p1*sdep5

# ---------------------------
# residual regressions
# ---------------------------
# ALCOHOL
salc2 ~ p2*salc1 + sdep1
salc3 ~ p2*salc2 + sdep2
salc4 ~ p2*salc3 + sdep3
salc5 ~ p2*salc4 + sdep4

# DEPRESSION
sdep2 ~ p3*sdep1
sdep3 ~ p3*sdep2
sdep4 ~ p3*sdep3
sdep5 ~ p3*sdep4
'
ad.fit2 <- lavaan(ad.mod2, lcmsr.sim)
# summary(ad.fit2, fit.measures = T)
# print(anova(ad.fit1, ad.fit2))

# model 3
ad.mod3 <- '
# ---------------------------
# latent factors
# ---------------------------
# ALCOHOL
# random intercept
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4 + 1*alc5
alc.i ~ 1
alc.i ~~ alc.i

# random slope
alc.s =~ 0*alc1 + 1*alc2 + 2*alc3 + 3*alc4 + 4*alc5
alc.s ~ 1
alc.s ~~ alc.s
alc.i ~~ alc.s

# DEPRESSION
# random intercept
dep.i =~ 1*dep1 + 1*dep2 + 1*dep3 + 1*dep4 + 1*dep5
dep.i ~ 1
dep.i ~~ dep.i
dep.i ~~ alc.i
dep.i ~~ alc.s

# ---------------------------
# create structured residuals
# ---------------------------
# ALCOHOL
alc1 ~~ 0*alc1
alc2 ~~ 0*alc2
alc3 ~~ 0*alc3
alc4 ~~ 0*alc4
alc5 ~~ 0*alc5

salc1 =~ 1*alc1
salc2 =~ 1*alc2
salc3 =~ 1*alc3
salc4 =~ 1*alc4
salc5 =~ 1*alc5

salc1 ~ 0
salc2 ~ 0
salc3 ~ 0
salc4 ~ 0
salc5 ~ 0

salc1 ~~ salc1
salc2 ~~ salc2
salc3 ~~ salc3
salc4 ~~ salc4
salc5 ~~ salc5

# DEPRESSION
dep1 ~~ 0*dep1
dep2 ~~ 0*dep2
dep3 ~~ 0*dep3
dep4 ~~ 0*dep4
dep5 ~~ 0*dep5

sdep1 =~ 1*dep1
sdep2 =~ 1*dep2
sdep3 =~ 1*dep3
sdep4 =~ 1*dep4
sdep5 =~ 1*dep5

sdep1 ~ 0
sdep2 ~ 0
sdep3 ~ 0
sdep4 ~ 0
sdep5 ~ 0

sdep1 ~~ sdep1
sdep2 ~~ sdep2
sdep3 ~~ sdep3
sdep4 ~~ sdep4
sdep5 ~~ sdep5

salc1 ~~ sdep1
salc2 ~~ p1*sdep2
salc3 ~~ p1*sdep3
salc4 ~~ p1*sdep4
salc5 ~~ p1*sdep5

# ---------------------------
# residual regressions
# ---------------------------
# ALCOHOL
salc2 ~ p2*salc1 + p4*sdep1
salc3 ~ p2*salc2 + p4*sdep2
salc4 ~ p2*salc3 + p4*sdep3
salc5 ~ p2*salc4 + p4*sdep4

# DEPRESSION
sdep2 ~ p3*sdep1
sdep3 ~ p3*sdep2
sdep4 ~ p3*sdep3
sdep5 ~ p3*sdep4
'
ad.fit3 <- lavaan(ad.mod3, lcmsr.sim)
# summary(ad.fit3, fit.measures = T)
# print(anova(ad.fit2, ad.fit3))

# model 4
ad.mod4 <- '
# ---------------------------
# latent factors
# ---------------------------
# ALCOHOL
# random intercept
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4 + 1*alc5
alc.i ~ 1
alc.i ~~ alc.i

# random slope
alc.s =~ 0*alc1 + 1*alc2 + 2*alc3 + 3*alc4 + 4*alc5
alc.s ~ 1
alc.s ~~ alc.s
alc.i ~~ alc.s

# DEPRESSION
# random intercept
dep.i =~ 1*dep1 + 1*dep2 + 1*dep3 + 1*dep4 + 1*dep5
dep.i ~ 1
dep.i ~~ dep.i
dep.i ~~ alc.i
dep.i ~~ alc.s

# ---------------------------
# create structured residuals
# ---------------------------
# ALCOHOL
alc1 ~~ 0*alc1
alc2 ~~ 0*alc2
alc3 ~~ 0*alc3
alc4 ~~ 0*alc4
alc5 ~~ 0*alc5

salc1 =~ 1*alc1
salc2 =~ 1*alc2
salc3 =~ 1*alc3
salc4 =~ 1*alc4
salc5 =~ 1*alc5

salc1 ~ 0
salc2 ~ 0
salc3 ~ 0
salc4 ~ 0
salc5 ~ 0

salc1 ~~ salc1
salc2 ~~ salc2
salc3 ~~ salc3
salc4 ~~ salc4
salc5 ~~ salc5

# DEPRESSION
dep1 ~~ 0*dep1
dep2 ~~ 0*dep2
dep3 ~~ 0*dep3
dep4 ~~ 0*dep4
dep5 ~~ 0*dep5

sdep1 =~ 1*dep1
sdep2 =~ 1*dep2
sdep3 =~ 1*dep3
sdep4 =~ 1*dep4
sdep5 =~ 1*dep5

sdep1 ~ 0
sdep2 ~ 0
sdep3 ~ 0
sdep4 ~ 0
sdep5 ~ 0

sdep1 ~~ sdep1
sdep2 ~~ sdep2
sdep3 ~~ sdep3
sdep4 ~~ sdep4
sdep5 ~~ sdep5

salc1 ~~ sdep1
salc2 ~~ p1*sdep2
salc3 ~~ p1*sdep3
salc4 ~~ p1*sdep4
salc5 ~~ p1*sdep5

# ---------------------------
# residual regressions
# ---------------------------
# ALCOHOL
salc2 ~ p2*salc1 
salc3 ~ p2*salc2 
salc4 ~ p2*salc3 
salc5 ~ p2*salc4 

# DEPRESSION
sdep2 ~ p3*sdep1 + salc1
sdep3 ~ p3*sdep2 + salc2
sdep4 ~ p3*sdep3 + salc3
sdep5 ~ p3*sdep4 + salc4
'
ad.fit4 <- lavaan(ad.mod4, lcmsr.sim)
# summary(ad.fit4, fit.measures = T)
# print(anova(ad.fit1, ad.fit4))

# model 5
ad.mod5 <- '
# ---------------------------
# latent factors
# ---------------------------
# ALCOHOL
# random intercept
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4 + 1*alc5
alc.i ~ 1
alc.i ~~ alc.i

# random slope
alc.s =~ 0*alc1 + 1*alc2 + 2*alc3 + 3*alc4 + 4*alc5
alc.s ~ 1
alc.s ~~ alc.s
alc.i ~~ alc.s

# DEPRESSION
# random intercept
dep.i =~ 1*dep1 + 1*dep2 + 1*dep3 + 1*dep4 + 1*dep5
dep.i ~ 1
dep.i ~~ dep.i
dep.i ~~ alc.i
dep.i ~~ alc.s

# ---------------------------
# create structured residuals
# ---------------------------
# ALCOHOL
alc1 ~~ 0*alc1
alc2 ~~ 0*alc2
alc3 ~~ 0*alc3
alc4 ~~ 0*alc4
alc5 ~~ 0*alc5

salc1 =~ 1*alc1
salc2 =~ 1*alc2
salc3 =~ 1*alc3
salc4 =~ 1*alc4
salc5 =~ 1*alc5

salc1 ~ 0
salc2 ~ 0
salc3 ~ 0
salc4 ~ 0
salc5 ~ 0

salc1 ~~ salc1
salc2 ~~ salc2
salc3 ~~ salc3
salc4 ~~ salc4
salc5 ~~ salc5

# DEPRESSION
dep1 ~~ 0*dep1
dep2 ~~ 0*dep2
dep3 ~~ 0*dep3
dep4 ~~ 0*dep4
dep5 ~~ 0*dep5

sdep1 =~ 1*dep1
sdep2 =~ 1*dep2
sdep3 =~ 1*dep3
sdep4 =~ 1*dep4
sdep5 =~ 1*dep5

sdep1 ~ 0
sdep2 ~ 0
sdep3 ~ 0
sdep4 ~ 0
sdep5 ~ 0

sdep1 ~~ sdep1
sdep2 ~~ sdep2
sdep3 ~~ sdep3
sdep4 ~~ sdep4
sdep5 ~~ sdep5

salc1 ~~ sdep1
salc2 ~~ p1*sdep2
salc3 ~~ p1*sdep3
salc4 ~~ p1*sdep4
salc5 ~~ p1*sdep5

# ---------------------------
# residual regressions
# ---------------------------
# ALCOHOL
salc2 ~ p2*salc1 
salc3 ~ p2*salc2 
salc4 ~ p2*salc3 
salc5 ~ p2*salc4 

# DEPRESSION
sdep2 ~ p3*sdep1 + p4*salc1
sdep3 ~ p3*sdep2 + p4*salc2
sdep4 ~ p3*sdep3 + p4*salc3
sdep5 ~ p3*sdep4 + p4*salc4
'
ad.fit5 <- lavaan(ad.mod5, lcmsr.sim)
# summary(ad.fit5, fit.measures = T)
# print(anova(ad.fit4, ad.fit5))

# model 6
ad.mod6 <- '
# ---------------------------
# latent factors
# ---------------------------
# ALCOHOL
# random intercept
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4 + 1*alc5
alc.i ~ 1
alc.i ~~ alc.i

# random slope
alc.s =~ 0*alc1 + 1*alc2 + 2*alc3 + 3*alc4 + 4*alc5
alc.s ~ 1
alc.s ~~ alc.s
alc.i ~~ alc.s

# DEPRESSION
# random intercept
dep.i =~ 1*dep1 + 1*dep2 + 1*dep3 + 1*dep4 + 1*dep5
dep.i ~ 1
dep.i ~~ dep.i
dep.i ~~ alc.i
dep.i ~~ alc.s

# ---------------------------
# create structured residuals
# ---------------------------
# ALCOHOL
alc1 ~~ 0*alc1
alc2 ~~ 0*alc2
alc3 ~~ 0*alc3
alc4 ~~ 0*alc4
alc5 ~~ 0*alc5

salc1 =~ 1*alc1
salc2 =~ 1*alc2
salc3 =~ 1*alc3
salc4 =~ 1*alc4
salc5 =~ 1*alc5

salc1 ~ 0
salc2 ~ 0
salc3 ~ 0
salc4 ~ 0
salc5 ~ 0

salc1 ~~ salc1
salc2 ~~ salc2
salc3 ~~ salc3
salc4 ~~ salc4
salc5 ~~ salc5

# DEPRESSION
dep1 ~~ 0*dep1
dep2 ~~ 0*dep2
dep3 ~~ 0*dep3
dep4 ~~ 0*dep4
dep5 ~~ 0*dep5

sdep1 =~ 1*dep1
sdep2 =~ 1*dep2
sdep3 =~ 1*dep3
sdep4 =~ 1*dep4
sdep5 =~ 1*dep5

sdep1 ~ 0
sdep2 ~ 0
sdep3 ~ 0
sdep4 ~ 0
sdep5 ~ 0

sdep1 ~~ sdep1
sdep2 ~~ sdep2
sdep3 ~~ sdep3
sdep4 ~~ sdep4
sdep5 ~~ sdep5

salc1 ~~ sdep1
salc2 ~~ p1*sdep2
salc3 ~~ p1*sdep3
salc4 ~~ p1*sdep4
salc5 ~~ p1*sdep5

# ---------------------------
# residual regressions
# ---------------------------
# ALCOHOL
salc2 ~ p2*salc1 
salc3 ~ p2*salc2 
salc4 ~ p2*salc3 
salc5 ~ p2*salc4 

# DEPRESSION
sdep2 ~ p3*sdep1 + p4*salc1
sdep3 ~ p3*sdep2 + p5*salc2
sdep4 ~ p3*sdep3 + p6*salc3
sdep5 ~ p3*sdep4 + p7*salc4

kappa := p5 - p4
p5 == p4 + 1*kappa
p6 == p4 + 2*kappa
p7 == p4 + 3*kappa
'
ad.fit6 <- lavaan(ad.mod6, lcmsr.sim)
# summary(ad.fit6, fit.measures = T)
# print(anova(ad.fit4, ad.fit6))

# model 7
ad.mod7 <- '
# ---------------------------
# latent factors
# ---------------------------
# ALCOHOL
# random intercept
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4 + 1*alc5
alc.i ~ 1
alc.i ~~ alc.i

# random slope
alc.s =~ 0*alc1 + 1*alc2 + 2*alc3 + 3*alc4 + 4*alc5
alc.s ~ 1
alc.s ~~ alc.s
alc.i ~~ alc.s

# DEPRESSION
# random intercept
dep.i =~ 1*dep1 + 1*dep2 + 1*dep3 + 1*dep4 + 1*dep5
dep.i ~ 1
dep.i ~~ dep.i
dep.i ~~ alc.i
dep.i ~~ alc.s

# ---------------------------
# create structured residuals
# ---------------------------
# ALCOHOL
alc1 ~~ 0*alc1
alc2 ~~ 0*alc2
alc3 ~~ 0*alc3
alc4 ~~ 0*alc4
alc5 ~~ 0*alc5

salc1 =~ 1*alc1
salc2 =~ 1*alc2
salc3 =~ 1*alc3
salc4 =~ 1*alc4
salc5 =~ 1*alc5

salc1 ~ 0
salc2 ~ 0
salc3 ~ 0
salc4 ~ 0
salc5 ~ 0

salc1 ~~ salc1
salc2 ~~ salc2
salc3 ~~ salc3
salc4 ~~ salc4
salc5 ~~ salc5

# DEPRESSION
dep1 ~~ 0*dep1
dep2 ~~ 0*dep2
dep3 ~~ 0*dep3
dep4 ~~ 0*dep4
dep5 ~~ 0*dep5

sdep1 =~ 1*dep1
sdep2 =~ 1*dep2
sdep3 =~ 1*dep3
sdep4 =~ 1*dep4
sdep5 =~ 1*dep5

sdep1 ~ 0
sdep2 ~ 0
sdep3 ~ 0
sdep4 ~ 0
sdep5 ~ 0

sdep1 ~~ sdep1
sdep2 ~~ sdep2
sdep3 ~~ sdep3
sdep4 ~~ sdep4
sdep5 ~~ sdep5

salc1 ~~ sdep1
salc2 ~~ p1*sdep2
salc3 ~~ p1*sdep3
salc4 ~~ p1*sdep4
salc5 ~~ p1*sdep5

# ---------------------------
# residual regressions
# ---------------------------
# ALCOHOL
salc2 ~ p2*salc1 + p4*sdep1
salc3 ~ p2*salc2 + p4*sdep2
salc4 ~ p2*salc3 + p4*sdep3
salc5 ~ p2*salc4 + p4*sdep4

# DEPRESSION
sdep2 ~ p3*sdep1 + p5*salc1
sdep3 ~ p3*sdep2 + p6*salc2
sdep4 ~ p3*sdep3 + p7*salc3
sdep5 ~ p3*sdep4 + p8*salc4

kappa := p6 - p5
p6 == p5 + 1*kappa
p7 == p5 + 2*kappa
p8 == p5 + 3*kappa
'
ad.fit7 <- lavaan(ad.mod7, lcmsr.sim)
# summary(ad.fit7, fit.measures = T)

# -------------------------------
# bivariate model for alcohol & depression 
# conditional on gender & treatment
# -------------------------------

# model 8
ad.mod8 <- '
# ---------------------------
# latent factors
# ---------------------------
# ALCOHOL
# random intercept
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4 + 1*alc5
alc.i ~ 1 + gen + trt
alc.i ~~ alc.i

# random slope
alc.s =~ 0*alc1 + 1*alc2 + 2*alc3 + 3*alc4 + 4*alc5
alc.s ~ 1 + gen + trt
alc.s ~~ alc.s
alc.i ~~ alc.s

# DEPRESSION
# random intercept
dep.i =~ 1*dep1 + 1*dep2 + 1*dep3 + 1*dep4 + 1*dep5
dep.i ~ 1 + gen + trt

dep.i ~~ dep.i
dep.i ~~ alc.i
dep.i ~~ alc.s

# ---------------------------
# create structured residuals
# ---------------------------
# ALCOHOL
alc1 ~~ 0*alc1
alc2 ~~ 0*alc2
alc3 ~~ 0*alc3
alc4 ~~ 0*alc4
alc5 ~~ 0*alc5

salc1 =~ 1*alc1
salc2 =~ 1*alc2
salc3 =~ 1*alc3
salc4 =~ 1*alc4
salc5 =~ 1*alc5

salc1 ~ 0
salc2 ~ 0
salc3 ~ 0
salc4 ~ 0
salc5 ~ 0

salc1 ~~ salc1
salc2 ~~ salc2
salc3 ~~ salc3
salc4 ~~ salc4
salc5 ~~ salc5

# DEPRESSION
dep1 ~~ 0*dep1
dep2 ~~ 0*dep2
dep3 ~~ 0*dep3
dep4 ~~ 0*dep4
dep5 ~~ 0*dep5

sdep1 =~ 1*dep1
sdep2 =~ 1*dep2
sdep3 =~ 1*dep3
sdep4 =~ 1*dep4
sdep5 =~ 1*dep5

sdep1 ~ 0
sdep2 ~ 0
sdep3 ~ 0
sdep4 ~ 0
sdep5 ~ 0

sdep1 ~~ sdep1
sdep2 ~~ sdep2
sdep3 ~~ sdep3
sdep4 ~~ sdep4
sdep5 ~~ sdep5

salc1 ~~ sdep1
salc2 ~~ p1*sdep2
salc3 ~~ p1*sdep3
salc4 ~~ p1*sdep4
salc5 ~~ p1*sdep5

# ---------------------------
# residual regressions
# ---------------------------
# ALCOHOL
salc2 ~ p2*salc1 + p4*sdep1
salc3 ~ p2*salc2 + p4*sdep2
salc4 ~ p2*salc3 + p4*sdep3
salc5 ~ p2*salc4 + p4*sdep4

# DEPRESSION
sdep2 ~ p3*sdep1 + p5*salc1
sdep3 ~ p3*sdep2 + p6*salc2
sdep4 ~ p3*sdep3 + p7*salc3
sdep5 ~ p3*sdep4 + p8*salc4

kappa := p6 - p5
p6 == p5 + 1*kappa
p7 == p5 + 2*kappa
p8 == p5 + 3*kappa
'
ad.fit8 <- lavaan(ad.mod8, lcmsr.sim)
summary(ad.fit8, fit.measures = T)
