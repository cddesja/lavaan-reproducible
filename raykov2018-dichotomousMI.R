# ------------------------------------------------
# Examining measurement invariance and differential item functioning with
# discrete latent construct indicators: A note on a multiple testing procedure
#
# Raykov, Dimitrov, Marcoulides, Li, & Menold
#
# Educational and Psychological Measurement 2018
# ------------------------------------------------

# convert a factor to a character
factorNumeric <- function(x) as.numeric(as.character(x))

library("lavaan")

# Read in data
masc.math <- read.table("data/masc_math.dat", header = TRUE)

# Assuming items 1 through 9 are the 9 items
# Note, gender == 0 is male and gender == 1 is female
masc.sub <- subset(masc.math, select = c("gender", paste0("item.", 1:9)))

cfa.mod <- "
math =~ 1*item.1 + item.2 + item.3 + item.4 + item.5 +
        item.6 + item.7 + item.8 + item.9

math ~ 1
math ~~ math
  
item.1 | 1 * t1
item.2 | t1
item.3 | t1
item.4 | t1
item.5 | t1
item.6 | t1
item.7 | t1
item.8 | t1
item.9 | t1
"

# step 1 - assess acceptable fit boys & girls model
male.fit <- cfa(cfa.mod, data = subset(masc.sub, gender == 0), 
               ordered = paste0("item.", 1:9), estimator = "WLSMV", parameterization = "theta")
female.fit <- cfa(cfa.mod, data = subset(masc.sub, gender == 1), 
               ordered = paste0("item.", 1:9), estimator = "WLSMV")
gender.fit <- rbind(fitMeasures(male.fit, c("chisq", "df", "rmsea", "cfi", "tli")),
                    fitMeasures(female.fit, c("chisq", "df", "rmsea", "cfi", "tli")))
rownames(gender.fit) <- c("male", "female")
gender.fit

# Note, the model fits well for both groups, but(!) this is not the clearly not the
# data used in the manuscript

# step 2 - fit the configural model 
configural.mod <- "
group: 1
math =~ lam1 * item.1 + lam2 * item.2 + lam3 * item.3 + lam4 * item.4 +
        lam5 * item.5 + lam6 * item.6 + lam7 * item.7 + lam8 * item.8 +
        lam9 * item.9

math ~ 0
math ~~ 1 * math

item.1 | tau1 * t1
item.2 | tau2 * t1
item.3 | tau3 * t1
item.4 | tau4 * t1
item.5 | tau5 * t1
item.6 | tau6 * t1
item.7 | tau7 * t1
item.8 | tau8 * t1
item.9 | tau9 * t1

group: 2
math =~ lam1 * item.1 + lam2 * item.2 + lam3 * item.3 + lam4 * item.4 +
        lam5 * item.5 + lam6 * item.6 + lam7 * item.7 + lam8 * item.8 +
lam9 * item.9

math ~ 1
math ~~ var1 * math

item.1 | tau1 * t1
item.2 | tau2 * t1
item.3 | tau3 * t1
item.4 | tau4 * t1
item.5 | tau5 * t1
item.6 | tau6 * t1
item.7 | tau7 * t1
item.8 | tau8 * t1
item.9 | tau9 * t1
"
configural.fit <- lavaan(configural.mod, data = masc.sub, 
                              ordered = paste0("item.", 1:9), estimator = "WLSMV", group = "gender", parameterization = "theta")
fitMeasures(configural.fit, c("chisq", "df", "rmsea"))
summary(configural.fit, fit.measures = TRUE)

# step 3 - test invariance of all the parameters
params.to.test <- paste0(rep(c("lam", "tau"), each = 9), 1:9)
param.matrix <- NULL

for(i in params.to.test){
  tmp.mod <- gsub(paste(i, "\\* "), "", configural.mod)
  tmp.fit <- lavaan(tmp.mod, data = masc.sub, 
                    ordered = paste0("item.", 1:9), estimator = "WLSMV", group = "gender",
                    parameterization = "theta")
  param.matrix <- rbind(param.matrix,
                         cbind(i, 
                               anova(configural.fit, tmp.fit)$`Chisq diff`[2], 
                               1, 
                               1 - pchisq(anova(configural.fit, tmp.fit)$`Chisq diff`[2], 1)))
  
}

params.mi <- data.frame(param.matrix)
params.mi[, 2:4] <- lapply(params.mi[, 2:4], factorNumeric)

# apply BH corections
params.mi$p.adj <- p.adjust(params.mi$V4, method = "BH") 
colnames(params.mi) <- c("params", "chisq.diff", "df", "p", "p.adj")
print(params.mi, digits = 3)
