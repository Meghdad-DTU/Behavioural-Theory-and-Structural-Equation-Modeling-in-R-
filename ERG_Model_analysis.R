
ERG_All <- read.csv("ERG_model_dataset.csv", sep=",", check.names=FALSE)

# install.packages("lavaan")
require(lavaan)
head(ERG_All)
nrow(ERG_All)
ncol(ERG_All)
which(colnames(ERG_All)=="Q10a" )
which(colnames(ERG_All)=="Q14nR" )
chron_data <- ERG_All[c("Q10a","Q10b","Q10c","Q10d","Q10e","Q10f","Q10g","Q10h","Q10i","Q10j","Q10k","Q10l",
                        "Q10m","Q10n","Q10o","Q11a","Q11b","Q11c","Q11d","Q11e","Q11f","Q11g","Q13a","Q13b",
                        "Q13c","Q13d","Q13e","Q13f","Q13g","Q13h","Q13i","Q13j","Q13k","Q14a","Q14b","Q14c",
                        "Q14d","Q14e","Q14fR","Q14gR","Q14hR","Q14iR","Q14j","Q14k","Q14l","Q14m","Q14n")]
head(chron_data)
ncol(chron_data)

# Cronbach's alpha reliability test: Used to measure the reliability of two or more construct indicators
# Some professionals insist on a reliability score of 0.70 or higher in order to use a psychometric instrument. 

# install.packages("psy")
require(psy)
cronbach(chron_data)

# Kaiser-Meyer-Olkin (KMO) Test: is a measure of how suited your data is for Factor Analysis. 
# The test measures sampling adequacy for each variable in the model and for the complete model. 
# KMO values between 0.8 and 1 indicate the sampling is adequate.

# install.packages("psych")
require (psych)
KMO(chron_data)

# correlation matrix with p-value
# install.packages("Hmisc")
library(Hmisc)
head(chron_data)
pairs(chron_data,panel=panel.smooth)
mycor = rcorr(as.matrix(chron_data), type="pearson")
head(mycor)

# there are two  Bartlett Tests, For factor analysis the second test is important
# 1) Bartlett test to test whether or not multiple samples have equal variance.
# Equal variance across samples is called homegeneity of variances.
# H0: the samples have equal variances
# H1: at least one sample has a significantly different variance.
bartlett.test(chron_data)

# 2) Bartlett's test for sphericity to test whether or not a correlation matrix is an identity matrix. 
# In other words, the population correlation matrix is an identity matrix; each variable correlates 
# perfectly with itself (r = 1) but has no correlation with the other variables (r = 0). If this hypothesis 
# cannot be rejected, then the appropriateness of factor analysis should be questioned. 
# H0: correlation matrix is an identity matrix 
# H1: correlation matrix is not an identity matrix

cortest.bartlett(cor(chron_data), n = 1481)



# Determinat of the Spearman correlation matrix: if the determinant is close to zero
# then it shows the existance of correlatins without multi-collinearity.
# multicollinearity (also collinearity) is a phenomenon in which two or more predictor 
# variables in a multiple regression model are highly correlated.
det(cor(chron_data))

## EFA 
require (psych)
fa.parallel(cor(chron_data), n.obs=1481, fa="both", n.iter=100)
# fm="pa" will do the principal factor solution, jasper used this method
fa(cor(chron_data), nfactors=7, n.obs = 1481, rotate="varimax", fm="pa")
# Factor analysis provides an account of the variance of each variable as
# common variance (communality = h2) and unique variance (uniqueness=u2).

## positive cycling self-concept (F1):     Q10a,Q10b,Q13b,Q13c,Q13d,Q13e,Q13f
## Travel togetherness (F2):               Q11a,Q11b,Q11c,Q11d,Q11e,Q11f,Q11g
## Car use functional difficulties (F3):   Q10e,Q10i,Q10k,Q10o,Q13a,Q14a,Q14j,Q14k,Q14l,Q14m,Q14n
## Positive car self-concept (F4):         Q13g,Q13h,Q13i,Q13j,Q13k
## Satisfying functional needs (F5):       Q10c,Q10d,Q10f,Q10g,Q10j,Q10l,Q10m,Q10n
## Cycling self-efficacy (F6):             Q14fR,Q14gR,Q14hR,Q14iR
## Functional difficulties in transit (F7):Q14b,Q14c,Q14d,Q14e

TC_BC_12 <- ERG_All$TC_BC_1 + ERG_All$TC_BC_2
TT_BC_25 <- ERG_All$TT_BC_20 + ERG_All$TT_BC_30 + ERG_All$TT_BC_40 + ERG_All$TT_BC_50
TC_CR_14 <- ERG_All$TC_CR_1 + ERG_All$TC_CR_2 + ERG_All$TC_CR_3 + ERG_All$TC_CR_4
TC_PT_13 <- ERG_All$TC_PT_1 + ERG_All$TC_PT_2 + ERG_All$TC_PT_3
TT_PT_25 <- ERG_All$TT_PT_20 + ERG_All$TT_PT_30 + ERG_All$TT_PT_40 + ERG_All$TT_PT_50
TT_BC_G  <- ERG_All$TT_BC_40 + ERG_All$TT_BC_50 + ERG_All$TT_BC_60 + ERG_All$TT_BC_70


ERG_All <- data.frame(ERG_All,TC_BC_12,TT_BC_25,TC_CR_14,TC_PT_13,TT_PT_25,TT_BC_G )
head(ERG_All)
require (lavaan)

jas_model <- "
F1 =~ Q10a+Q10b+Q13b+Q13c+Q13d+Q13e+Q13f
F2 =~ Q11a+Q11b+Q11c+Q11d+Q11e+Q11f+Q11g
F3 =~ Q10e+Q10i+Q10k+Q10o+Q13a+Q14a+Q14j+Q14k+Q14l+Q14m+Q14n
F4 =~ Q13g+Q13h+Q13i+Q13j+Q13k
F5 =~ Q10c+Q10d+Q10f+Q10g+Q10j+Q10l+Q10m+Q10n
F6 =~ Q14fR+Q14gR+Q14hR+Q14iR
F7 =~ Q14b+Q14c+Q14d+Q14e

F1 ~ MALE+CAR_ACC+H_CPHMA+TT_BC_G
F2 ~ AGE45_65+EDU_VOC+EDU_S+EDU_M+EDU_L+W_CITY
F3 ~ MALE+CAR_ACC
F4 ~ AGE30_45+AGE45_65+CAR_ACC+EDU_M+EDU_L+JOB_STUD+W_CPHMA
F5 ~ MALE+AGE45_65
F6 ~ MALE+INC_H+Q5_KIDS+TC_BC_12+TT_BC_10+TT_BC_25
F7 ~ AGE30_45+AGE45_65+AGE_65

F1 ~ F2+F3+F6+BC_R+PT_R
F2 ~ BC_R
F3 ~ BC_R+PT_R+CR_R
F4 ~ BC_R+CR_R
F5 ~ F4+F7+PT_R+CR_R
F6 ~ BC_R+PT_R+CR_R
F7 ~ PT_R+CR_R

BC_SAT ~ F1+F4+F6
PT_SAT ~ F3+F5+F6+F7
CR_SAT ~ F3+F4+F6+F7
"

fit_DWLS <-sem(jas_model, data=ERG_All, estimator="DWLS", ordered = c("Q10a","Q10b","Q13b","Q13c","Q13d","Q13e","Q13f",
                                                                       "Q11a","Q11b","Q11c","Q11d","Q11e","Q11f","Q11g",
                                                                       "Q10e","Q10i","Q10k","Q10o","Q13a","Q14a","Q14j","Q14k","Q14l","Q14m","Q14n",
                                                                       "Q13g","Q13h","Q13i","Q13j","Q13k",
                                                                       "Q10c","Q10d","Q10f","Q10g","Q10j","Q10l","Q10m","Q10n",
                                                                       "Q14fR","Q14gR","Q14hR","Q14iR",
                                                                       "Q14b","Q14c","Q14d","Q14e"))
summary(fit_DWLS, standardized=T,fit.measures=T)
fitMeasures(fit_DWLS, c("pvalue", "cfi", "rmsea", "srmr"))



fit_WLSMV <-sem(jas_model, data=ERG_All, estimator="WLSMV", ordered = c("Q10a","Q10b","Q13b","Q13c","Q13d","Q13e","Q13f",
                                                                      "Q11a","Q11b","Q11c","Q11d","Q11e","Q11f","Q11g",
                                                                      "Q10e","Q10i","Q10k","Q10o","Q13a","Q14a","Q14j","Q14k","Q14l","Q14m","Q14n",
                                                                      "Q13g","Q13h","Q13i","Q13j","Q13k",
                                                                      "Q10c","Q10d","Q10f","Q10g","Q10j","Q10l","Q10m","Q10n",
                                                                      "Q14fR","Q14gR","Q14hR","Q14iR",
                                                                      "Q14b","Q14c","Q14d","Q14e"))
summary(fit_WLSMV, standardized=T,fit.measures=T)
fitMeasures(fit_WLSMV, c("pvalue.scaled", "cfi.scaled", "rmsea.scaled","srmr"))

# The parameterEstimates function extracts not only the values of the estimated parameters, 
# but also the standard errors, the z-values, the standardized parameter values, 
# and returns everything conveniently as a data frame


mod_ind <- modindices(fit_DWLS,sort. = TRUE)
# Spoting the top 10:
head(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], 10)


# Extracting factor loadings est= estimate, std = standardized
fit_DWLS_lambda <- inspect(fit_DWLS,what="est")$lambda
fit_DWLS_lambda

# Extracting betas
fit_DWLS_beta <- inspect(fit_DWLS,what="est")$beta
fit_DWLS_beta

# Extracting gammas
gamma <- data.frame(inspect(fit_DWLS,what="est")$gamma)
fit_DWLS_gamma <- gamma[c("BC_R", "PT_R", "CR_R")]
fit_DWLS_gamma


# The fitted() and fitted.values() functions return the model-implied (fitted) covariance 
# matrix (and mean vector) of a fitted model:
fitted(fit_DWLS)$cov

#observed covarience matrix
inspect(fit_DWLS,"sampstat")$cov

# The resid() or residuals() functions return (unstandardized) residuals of a fitted model. 
# This is simply the difference between the observed and implied covariance matrix and mean vector. 
# If the estimator is maximum likelihood, it is also possible to obtain the normalized and the 
# standardized residuals (note: you may observe several NA values, but they can be safely ignored)
residuals(fit_DWLS)$cov

