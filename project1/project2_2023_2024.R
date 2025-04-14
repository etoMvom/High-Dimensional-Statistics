

setwd("D:/Master/Master/M/Q1/HDS/pro2")  
data<-read.table("Data_Mvomoeto.csv",header=TRUE,sep=",",stringsAsFactors=TRUE)
attach(data)
View(data)

summary(data)
library(visdat)
vis_miss(data)


# work on the complete observations only.
data <- na.omit(data)
summary(data)


############################################################################
# 3.Graphical model of the dependence structure between the score variables#
############################################################################# 


#3.1 


library(glasso)

#-------------------
# A. Saber_s11 test-
#-------------------
# Computing the empirical covariance matrix relative to the Saber_s11 test

n <- dim(data)[1]  # Number of observations
cov_saber_11 <- cov(data[,11:15]) 
cov_saber_11

rho_values <- seq(0.1, 100, by = 0.01) 
non_zero_count <- numeric(length(rho_values))

for (i in seq_along(rho_values)) {
  res <- glasso(s = cov_saber_11, rho = rho_values[i], nobs = n)
  
  # Count number of zero entries in the upper triangle of the precision matrix theta (it is equal to the number of edges)
  non_zero_count[i] <- sum(res$wi[upper.tri(res$wi)] != 0) 
}


# Plot the halved number of zero entries against rho values
plot(rho_values, non_zero_count, type = 'l', col = 'green', xlab = 'rho', ylab = 'Number of edges',
     main = 'Number of edges for different rho values (graphical model of Saber_S11 test)')


#-------------------
# B.Saber_PRO test -
#-------------------

# Computing the empirical covariance matrix relative to the Saber_PRO test
cov_saber_PRO <- cov(data[,16:21]) 
cov_saber_PRO

rho_values <- seq(0.1, 1000, by = 0.01) 
non_zero_count <- numeric(length(rho_values))

for (i in seq_along(rho_values)) {
  res <- glasso(s = cov_saber_PRO, rho = rho_values[i], nobs = n)
  
  # Count number of non zero entries in the upper triangle of the precision matrix theta (it is equal to the number of edges)
  non_zero_count[i] <- sum(res$wi[upper.tri(res$wi)] != 0) 
}


# Plot the halved number of zero entries against rho values
plot(rho_values, non_zero_count, type = 'l', col = 'green', xlab = 'rho', ylab = 'Number of edges',
     main = 'Number of edges for different rho values (graphical model of Saber_PRO test)')


#3.2 Graphical models


# The graph associated with a given inverse covariance matrix	
plot_graph <- function(invCov,varnames=NULL,threshold=1e-16,col.nodes="grey85",col.edges="black")
{
  nvar <- ncol(invCov)
  if(is.null(varnames)) varnames=paste("Var",1:nvar)
  edges <- abs(invCov)>=threshold
  theta=seq(from=0,by=2*pi/(nvar),length=nvar)
  x=cos(theta);y=sin(theta)
  plot(1.3*x,1.3*y,pch="",frame.plot=F,axes=F,xlab="",ylab="")
  points(x,y,pch=19,cex=10,col=col.nodes)
  
  for (i in 2:nvar)
  {
    for (j in 1:(i-1))
    {
      if (edges[i,j]==TRUE) 
        segments(x[i],y[i],x[j],y[j],col=col.edges)
    }
  }
  points(x,y,pch=19,cex=10,col=col.nodes)
  points(x,y,pch=1,cex=10)
  text(x,y,varnames)
}

#-------------------
# A. Saber_S11 test-
#-------------------

# Given the previous plot relative to Saber_S11 test, one may pick 75 as regularization parameter value 
# for getting 5 edges.

rho <- 75
res <- glasso(s = cov_saber_11 , rho = rho, nobs = n)
res$wi

plot_graph(invCov=res$wi,  varnames=names(data[,11:15]))


#-------------------
# B. Saber_PRO test-
#-------------------

# Given the previous plot relative to Saber_PRO test, one may pick 410 as regularization parameter value 
# for getting 5 edges.

rho <- 410
res <- glasso(s = cov_saber_PRO , rho = rho, nobs = n)
res$wi

plot_graph(invCov=res$wi,  varnames=names(data[,16:21]))


#3.3 

library(car)

scatterplotMatrix(data[, 11:15], pch = 16)
scatterplotMatrix(data[, 16:21], pch = 16)

par(mfrow=c(1,3))
hist(MAT_S11,proba=TRUE, xlab = 'score')
a<-seq(30,100,0.1)
lines(a,dnorm(a,mean(MAT_S11),sd(MAT_S11)))

hist(CR_S11,proba=TRUE, xlab='score')
a<-seq(40,100,0.1)
lines(a,dnorm(a,mean(CR_S11),sd(CR_S11)))

hist(CC_S11,proba=TRUE, xlab ='score')
a<-seq(30,100,0.1)
lines(a,dnorm(a,mean(CC_S11),sd(CC_S11)))
par(mfrow=c(1,1))

# Checking out the skewness and kurtosis of the following KDE (Under normality, skewness should be 0 and kurtosis 3; )
library(moments)

# Score-variables relative to Saber_S11 test
skewness(cbind(MAT_S11, CR_S11, CC_S11, ENG_S11, BIO_S11))
kurtosis(cbind(MAT_S11, CR_S11, CC_S11, ENG_S11, BIO_S11))


# Score-variables relative to Saber_PRO test
skewness(cbind(QR_PRO,CR_PRO,CC_PRO, ENG_PRO, WC_PRO, FEP_PRO))
kurtosis(cbind(QR_PRO,CR_PRO,CC_PRO, ENG_PRO, WC_PRO, FEP_PRO))


##############################################
# 4. Linear modeling of the Saber_PRO scores #
##############################################


#4.1 

# In the first project, you have deleted outlying observations because they lead to a corrupted empirical covariance matrix
# and subsequently to a wrong analysis. We start by drop them also in this project with robust technique.

data[,11:21] <- scale(data[,11:21])    # Standardizing the data
mean_d <- apply(data[,11:21] , 2, mean)
cov_d <- cov(data[,11:21])

# MCD estimator

library(MASS)

h<-floor(dim(data[,11:21])[1]*0.75)
h
resrob<-cov.rob(data[,11:21],quantile.used =h,method = "mcd",cor=TRUE)
resrob$cov
resrob$cor
resrob$best
resrob$center

d1 <- mahalanobis(data[,11:21], mean_d ,cov_d)
d2 <- mahalanobis(data[,11:21], resrob$center, resrob$cov)
plot(d1, d2, main="Outliers detection using MCD")
abline(v=qchisq(0.975,11), col="red")
abline(h=qchisq(0.975,11), col="red")

x<-qchisq(0.975,11)
x

outliers_mcd <-data[,11:21][d1 > x & d2 > x, ]
dim(outliers_mcd)[1]   # 16 outliers (in the upper-right panel).

data <- data[!(rownames(data) %in% rownames(outliers_mcd)), ]   
dim(data)[1]   # Now 232 observations

plot(data$ENG_S11, data$CC_S11, xlab='ENG_S11 score', ylab='CC_S11 score')
plot(data$CR_S11, data$BIO_S11, xlab ='CR_S11 score', ylab='BIO_S11 score') 
# These score-variable pairs are not too correlated.


#4.2


# Full models :
str(data)
COMPUTER <- as.factor(COMPUTER)
TV <- as.factor(TV)
INTERNET <- as.factor(INTERNET)
ACADEMIC_PROGRAM <- as.factor(ACADEMIC_PROGRAM)
SCHOOL_NAT <- as.factor(SCHOOL_NAT)

# QR_PRO as dependent variable 
mod_QR_PRO <- lm(QR_PRO ~ COMPUTER + INTERNET + SCHOOL_NAT + TV + SCHOOL_NAT + MAT_S11 + BIO_S11 + CC_S11 + CR_S11 + ENG_S11)
summary(mod_QR_PRO)
r_squared_QR_PRO <- summary(mod_QR_PRO)$r.squared


# CR_PRO as dependent variable 
mod_CR_PRO <- lm(CR_PRO ~ COMPUTER + INTERNET + SCHOOL_NAT + TV + SCHOOL_NAT + MAT_S11 + BIO_S11 + CC_S11 + CR_S11 + ENG_S11)
summary(mod_CR_PRO)
r_squared_CR_PRO <- summary(mod_CR_PRO)$r.squared


# ENG_PRO as dependent variable 
mod_ENG_PRO <- lm(ENG_PRO ~ COMPUTER + INTERNET + SCHOOL_NAT + TV + SCHOOL_NAT + MAT_S11 + BIO_S11 + CC_S11 + CR_S11 + ENG_S11)
summary(mod_ENG_PRO)
r_squared_ENG_PRO <- summary(mod_ENG_PRO)$r.squared


# WC_PRO as dependent variable 
mod_WC_PRO <- lm(WC_PRO ~ COMPUTER + INTERNET + SCHOOL_NAT + TV + SCHOOL_NAT + MAT_S11 + BIO_S11 + CC_S11 + CR_S11 + ENG_S11)
summary(mod_WC_PRO)
r_squared_WC_PRO <- summary(mod_WC_PRO)$r.squared


# CC_PRO as dependent variable 
mod_CC_PRO <- lm(CC_PRO ~ COMPUTER + INTERNET + SCHOOL_NAT + TV + SCHOOL_NAT + MAT_S11 + BIO_S11 + CC_S11 + CR_S11 + ENG_S11)
summary(mod_CC_PRO)
r_squared_CC_PRO <- summary(mod_CC_PRO)$r.squared


# FEP_PRO as dependent variable 
mod_FEP_PRO <- lm(FEP_PRO ~ COMPUTER + INTERNET + SCHOOL_NAT + TV + SCHOOL_NAT + MAT_S11 + BIO_S11 + CC_S11 + CR_S11 + ENG_S11)
summary(mod_FEP_PRO)
r_squared_FEP_PRO <- summary(mod_FEP_PRO)$r.squared


# Making a list of R-squared obtained (report these values under table form)
cbind(r_squared_QR_PRO, r_squared_CR_PRO, r_squared_ENG_PRO, r_squared_CC_PRO, r_squared_FEP_PRO, r_squared_WC_PRO )



#4.3 

library(MASS)
step(mod_WC_PRO)
# CC_S11 and ENG_S11 are score-variables which are selected as predictors for linear modeling of WC_PRO score-
# variable.

step(mod_ENG_PRO)
# We have the same score-variables selected as the previous selection.

step(mod_CR_PRO)
# BIO_S11, CC_S11 and CR_S11 are score-variables which are chosen for the linear modeling of CR_PRO score-variable.



#4.4

library(car)
# The constrained models (simplified models): 
mod_WC_simplified <- lm(WC_PRO ~ CC_S11 + ENG_S11)
anova(mod_WC_PRO, mod_WC_simplified)    # Comparison with its corresponding full model.

summary(mod_WC_simplified)
hist(mod_WC_simplified$residuals, xlab='residual', main='Histogram of the residuals provided by the constrained model of WC_PRO score')

qqPlot(mod_WC_simplified$residuals)
shapiro.test(mod_WC_simplified$residuals)
kurtosis(mod_WC_simplified$residuals) # 2.0
skewness(mod_WC_simplified$residuals)

vif(mod_WC_simplified)  

mod_ENG_simplified <- lm(ENG_PRO ~ CC_S11 + ENG_S11) 
anova(mod_ENG_PRO, mod_ENG_simplified)    # Comparison with its corresponding full model.
summary(mod_ENG_simplified)
hist(mod_ENG_simplified$residuals, xlab='residual', main='Histogram of the residuals provided by the constrained model of ENG_PRO score')

qqPlot(mod_ENG_simplified$residuals)
shapiro.test(mod_ENG_simplified$residuals)
kurtosis(mod_ENG_simplified$residuals)  #5
skewness(mod_ENG_simplified$residuals) #-0.88
# The normality of the residual is rejected for the same reasons as previously.


mod_CR_simplified <- lm(CR_PRO ~ CC_S11 + CR_S11 + BIO_S11)
anova(mod_CR_PRO, mod_CR_simplified)    # Comparison with its corresponding full model.
summary(mod_CR_simplified)
hist(mod_CR_simplified$residuals, xlab='residual',main ='Histogram of the residuals provided by the constrained model of CR_PRO score')

qqPlot(mod_CR_simplified$residuals)
shapiro.test(mod_CR_simplified$residuals)
kurtosis(mod_CR_simplified$residuals)  #3.39
skewness(mod_CR_PRO$residuals)  # -0.68
# Same conclusion as previously.

# Assessing homoscedasticity of the different models.
library(lmtest)
bptest(mod_CR_simplified)
bptest(mod_ENG_simplified)
bptest(mod_WC_simplified)

# In all cases, homoscedasticity is not rejected, and also the constrained models obtained 
# are better than the full ones. 

detach(data)



