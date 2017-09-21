# ------------------------------------------------
# The Influence of changes in marital status on
# development trajectories of alcohol use
# in young adults.
#
# Curran, Muthen, & Harford
#
# Journal of Studies on Alcohol 1998
# ------------------------------------------------

library("lavaan")
MEANS <- c(0.92, 0.76, 0.73, 0.62, 0.08, 0.08, 0.08, 0.07, 22.69, 0.54, 0.30, 0.14, 12.72)
SDS <- c(1.68, 1.35, 1.26, 1.08, 0.27, 0.27, 0.27, 0.26, 1.38, 0.49, 0.46, 0.35, 2.02)
LOWER <- '
1.00
.494 1.00
.440 .519 1.00
.382 .471 .510 1.00
-.074 -.068 -.062 -.035 1.00
-.023 -.048 -.057 -.055 -.086 1.00
.009 -.003 -.036 -.039 -.085 -.083  1.00
.043 .025 .011 -.022 -.081 -.080 -.079 1.00
.032 .020 -.005 .010 .050 .028 .007 .028 1.00
.231, .238 .252 .264 -.048 -.005 -.028 -.003 .011 1.00
-.142 -.146 -.120 -.118 -.085 -.080 -.067 -.071 -.014 -.013 1.00
-.025 -.014 -.028 -.001 .012 .019 -.021 -.013 -.023 .040 -.265 1.00
.012 -.005 -.021 -.018 .006 .006 .009 .049 .222 -.110 -.149 -.125 1.00'
alcuse.cor <- getCov(LOWER, names = c(paste0("alc", 1:4), paste0("mar", 1:4), "age", "gender", "black", "hispanic", "education"))
alcuse.cov <- (SDS %*% t(SDS)) * alcuse.cor

# Sample sizes
total.n <- 4052
female.n <- 1869
male.n <- 2183
black.n <- 1232
hispanic.n <- 562
white.n <- 2258
blackFemale.n <- 581
whiteFemale.n <- 1057
blackMale.n <- 651
whiteMale.n <- 1201

model.1 <- '
# ----------------
# latent factors
# ----------------
# alcohol
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4
alc.s =~ 0*alc1 + 1*alc2 + p3*alc3 + p4*alc4 

# p3 is freely estimated to be 1.54 
# and p4 is freely estimated to be 2.46

alc.i ~~ alc.s
alc.i ~~ alc.i
alc.s ~~ alc.s

alc.i ~ 1
alc.s ~ 1

alc1 ~~ alc1
alc2 ~~ alc2
alc3 ~~ alc3
alc4 ~~ alc4
'
fit.1 <- lavaan(model.1, sample.mean = MEANS, sample.nobs = total.n, sample.cov =  alcuse.cov, start = "simple") # won't converge
summary(fit.1, fit.measures = T)

model.2 <- '
# ----------------
# latent factors
# ----------------
# alcohol
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4
alc.s =~ 0*alc1 + 1*alc2 + p3*alc3 + p4*alc4

alc.i ~~ alc.s
alc.i ~~ alc.i
alc.s ~~ alc.s

alc.i ~ 1 + age + gender + education  + black + hispanic
alc.s ~ 1 + age + gender + education  + black + hispanic

alc1 ~~ alc1
alc2 ~~ alc2
alc3 ~~ alc3
alc4 ~~ alc4

lmar1 =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4
lmar2 =~ 1*alc2 + 1*alc3 + 1*alc4
lmar3 =~ 1*alc3 + 1*alc4
lmar4 =~ 1*alc4

lmar1 ~ mar1
lmar2 ~ mar2
lmar3 ~ mar3
lmar4 ~ mar4
'

fit.2 <- lavaan(model.2, sample.mean = MEANS, sample.nobs = total.n, sample.cov =  alcuse.cov)
summary(fit.2, standardized = T, fit.measures = T)

model.2b <- '
# ----------------
# latent factors
# ----------------
# alcohol
alc.i =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4
alc.s =~ 0*alc1 + 1*alc2 + p3*alc3 + p4*alc4

alc.i ~~ alc.s
alc.i ~~ alc.i
alc.s ~~ alc.s

alc.i ~ 1 + age + gender + education  + black + hispanic
alc.s ~ 1 + age + gender + education  + black + hispanic

alc1 ~~ alc1
alc2 ~~ alc2
alc3 ~~ alc3
alc4 ~~ alc4

lmar1 =~ 1*alc1 + 1*alc2 + 1*alc3 + 1*alc4
lmar2 =~ 1*alc2 + 1*alc3 + 1*alc4
lmar3 =~ 1*alc3 + 1*alc4
lmar4 =~ 1*alc4

lmar1 ~ mar1
lmar2 ~ mar2
lmar3 ~ mar3
lmar4 ~ mar4

# authors says no effect, these are significant here
alc1 ~ lmar2
alc2 ~ lmar3
alc3 ~ lmar4
'

fit.2b <- lavaan(model.2b, sample.mean = MEANS, sample.nobs = total.n, sample.cov =  alcuse.cov)
summary(fit.2b, standardized = T, fit.measures = T)
# actually i find there is an effect ...

# Can't replicate his multi-group model because the data are not on the web.


