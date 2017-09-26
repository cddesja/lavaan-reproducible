# ------------------------
# Analyzing Multiple Outcomes in Clinical Research 
# Using Multivariate Multilevel Models
#
# Baldwin, Imel, Braithwaite, Atkins
#
# Journal of Consulting and Clinicial Psychology 2014
# http://dx.doi.org/10.1037/a0035628
# ------------------------
library(lavaan)

# Read in data in wide format
baldwin_data <- read.csv("http://supp.apa.org/psycarticles/supplemental/a0035628/appendix_example1_wide.SUPP.FINAL.csv")
# y1: depression
# y2: quality of life

# need to make even wider for lavaan``
baldwin_wide <- reshape(baldwin_data,
                        direction = "wide",
                        v.names = c("y1", "y2"),
                        timevar = "time",
                        idvar = "pid") 
colnames(baldwin_wide) <- c("pid", "tx", paste0(rep(c("dep", "qol"), 3), rep(0:2, each = 2)))
  
# Table 2: Univariate models ----
# Depression 
dep.mod <- '
dep.i =~ 1*dep0 + 1*dep1 + 1*dep2
dep.s =~ 0*dep0 + 1*dep1 + 2*dep2
dep.i ~~ dep.i
dep.s ~~ dep.s
dep.i ~~ dep.s
dep.i ~ 1 + tx
dep.s ~ 1 + beta13*tx

# set residuals to be equal with a measure
dep0 ~~ e1*dep0
dep1 ~~ e1*dep1
dep2 ~~ e1*dep2
'
dep.fit <- lavaan(dep.mod, baldwin_wide)
summary(dep.fit)

# Quality of Life 
qol.mod <- '
qol.i =~ 1*qol0 + 1*qol1 + 1*qol2
qol.s =~ 0*qol0 + 1*qol1 + 2*qol2
qol.i ~~ qol.i
qol.s ~~ qol.s
qol.i ~~ qol.s
qol.i ~ 1 + tx
qol.s ~ 1 + beta23*tx

# set residuals to be equal with a measure
qol0 ~~ e2*qol0
qol1 ~~ e2*qol1
qol2 ~~ e2*qol2
'
qol.fit <- lavaan(qol.mod, baldwin_wide)
summary(qol.fit)

# Table 2: Multivariate independent outcomes ----
ind.mod <- '
# depression
dep.i =~ 1*dep0 + 1*dep1 + 1*dep2
dep.s =~ 0*dep0 + 1*dep1 + 2*dep2
dep.i ~~ dep.i
dep.s ~~ dep.s
dep.i ~~ dep.s
dep.i ~ 1 + tx
dep.s ~ 1 + tx

# quality of life
qol.i =~ 1*qol0 + 1*qol1 + 1*qol2
qol.s =~ 0*qol0 + 1*qol1 + 2*qol2
qol.i ~~ qol.i
qol.s ~~ qol.s
qol.i ~~ qol.s
qol.i ~ 1 + tx
qol.s ~ 1 + tx

# set residuals to be equal with a measure
dep0 ~~ e1*dep0
dep1 ~~ e1*dep1
dep2 ~~ e1*dep2

qol0 ~~ e2*qol0
qol1 ~~ e2*qol1
qol2 ~~ e2*qol2
'
ind.fit <- lavaan(ind.mod, baldwin_wide)
summary(ind.fit)

# Test of different treatement effects based on
# dep.fit and qol.fit
se.z <- sqrt(0.098^2 + 0.108^2)
beta.diff <- (-0.412 - -0.089)
z <- beta.diff/se.z
pnorm(z,lower.tail = T)*2

# Table 2: Multivariate related outcomes, main model ----
rel.mod <- '
# depression
dep.i =~ 1*dep0 + 1*dep1 + 1*dep2
dep.s =~ 0*dep0 + 1*dep1 + 2*dep2
dep.i ~~ u1*dep.i
dep.s ~~ v1*dep.s
dep.i ~~ u1v1*dep.s
dep.i ~ 1 + beta11*tx
dep.s ~ 1 + beta13*tx

# quality of life
qol.i =~ 1*qol0 + 1*qol1 + 1*qol2
qol.s =~ 0*qol0 + 1*qol1 + 2*qol2
qol.i ~~ u2*qol.i
qol.s ~~ v2*qol.s
qol.i ~~ u2v2*qol.s
qol.i ~ 1 + beta21*tx
qol.s ~ 1 + beta23*tx

# covariances across outcome
dep.i ~~ u1u2*qol.i
dep.i ~~ u1v2*qol.s
dep.s ~~ v1u2*qol.i
dep.s ~~ v1v2*qol.s

# set residuals to be equal with a measure
dep0 ~~ e1*dep0
dep1 ~~ e1*dep1
dep2 ~~ e1*dep2

qol0 ~~ e2*qol0
qol1 ~~ e2*qol1
qol2 ~~ e2*qol2

# correlated residuals across outcome within timer
dep0 ~~ e3*qol0
dep1 ~~ e3*qol1
dep2 ~~ e3*qol2

# Test of different trt effects of beta13 and beta23
diff.beta := beta13 - beta23

# Calculation of various correlations
r.int := u1u2 / (sqrt(u1)*sqrt(u2))
r.slope := v1v2 / (sqrt(v1)*sqrt(v2))
r.qoli.deps := v1u2 / (sqrt(u2)*sqrt(v1))
r.depi.qols := u1v2 / (sqrt(u1)*sqrt(v2))
'
rel.fit <- lavaan(rel.mod, baldwin_wide)
summary(rel.fit, fit.measures = T)
anova(ind.fit, rel.fit)

## Alternative LRT test fixing trt effects to be identical 
## - fit just to perform LRT
lrt.mod <- '
# depression
dep.i =~ 1*dep0 + 1*dep1 + 1*dep2
dep.s =~ 0*dep0 + 1*dep1 + 2*dep2
dep.i ~~ u1*dep.i
dep.s ~~ v1*dep.s
dep.i ~~ u1v1*dep.s
dep.i ~ 1 + beta11*tx
dep.s ~ 1 + beta3*tx

# quality of life
qol.i =~ 1*qol0 + 1*qol1 + 1*qol2
qol.s =~ 0*qol0 + 1*qol1 + 2*qol2
qol.i ~~ u2*qol.i
qol.s ~~ v2*qol.s
qol.i ~~ u2v2*qol.s
qol.i ~ 1 + beta21*tx
qol.s ~ 1 + beta3*tx

# covariances across outcome
dep.i ~~ u1u2*qol.i
dep.i ~~ u1v2*qol.s
dep.s ~~ v1u2*qol.i
dep.s ~~ v1v2*qol.s

# set residuals to be equal with a measure
dep0 ~~ e1*dep0
dep1 ~~ e1*dep1
dep2 ~~ e1*dep2

qol0 ~~ e2*qol0
qol1 ~~ e2*qol1
qol2 ~~ e2*qol2

# correlated residuals across outcome within timer
dep0 ~~ e3*qol0
dep1 ~~ e3*qol1
dep2 ~~ e3*qol2
'
lrt.fit <- lavaan(lrt.mod, baldwin_wide)
anova(lrt.fit, rel.fit)