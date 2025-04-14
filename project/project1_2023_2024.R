
#######################
# 1. Data presentation#
#######################


setwd("D:/Master/Master/M/Q1/HDS/p1/project1")  
data<-read.table("Data_Mvomoeto.csv",header=TRUE,sep=",",stringsAsFactors=TRUE)
attach(data)
View(data)


str(data)
summary(data)
library(visdat)
vis_miss(data)



###############################
# 2. Exploratory data analysis#
###############################

# a. Statistical and graphical summary of the variables

# Mechanism of the missing values : Responding this question after reading the main text describing the 
# the whole data set.
# Subsequently when dealing the missing value, one can apply simply case-deletion strategy.

data <- na.omit(data)
str(data)              # Only four observations lost.
summary(data)
vis_miss(data)

# According to the scores of students in high school.

par(mfrow=c(2,3))
boxplot(data[,"ENG_S11"],main="ENG_S11")
abline(h = mean(data$ENG_S11), col = "red")

boxplot(data[,"BIO_S11"],main="BIO_S11") 
abline(h = mean(data$BIO_S11), col = "red")

boxplot(data[,"CC_S11"],main="CC_S11")   
abline(h = mean(data$CC_S11), col = "red")

boxplot(data[,"CR_S11"],main="CR_S11")  
abline(h = mean(data$CR_S11), col = "red")

boxplot(data[,"MAT_S11"],main="MAT_S11") 
abline(h = mean(data$MAT_S11), col = "red")

boxplot(data[,"PEOPLE_HOUSE"],main="PEOPLE_HOUSE") 
abline(h = mean(data$PEOPLE_HOUSE ), col = "red")


# According to the scores of students in high school.
par(mfrow=c(2,3))
boxplot(data[,"QR_PRO"],main="QR_PRO")
abline(h = mean(data$QR_PRO), col = "red")

boxplot(data[,"CR_PRO"],main="CR_PRO")
abline(h = mean(data$CR_PRO), col = "red")

boxplot(data[,"CC_PRO"],main="CC_PRO") 
abline(h = mean(data$CC_PRO), col = "red")

boxplot(data[,"ENG_PRO"],main="ENG_PRO")  
abline(h = mean(data$ENG_PRO), col = "red") 

boxplot(data[,"WC_PRO"],main="WC_PRO")    
abline(h = mean(data$WC_PRO), col = "red")

boxplot(data[,"FEP_PRO"],main="FEP_PRO")
abline(h = mean(data$FEP_PRO), col = "red")


data[, "ACADEMIC_PROGRAM"] <- ifelse(data["ACADEMIC_PROGRAM"] == "INDUSTRIAL ENGINEERING", 1, 0)
data[, "JOB"] <- ifelse(data["JOB"] == "Yes", 1, 0)
data[, "COMPUTER"] <- ifelse(data["COMPUTER"] == "Yes", 1, 0)
data[, "INTERNET"] <- ifelse(data["INTERNET"] == "Yes", 1, 0)
data[, "CAR"] <- ifelse(data["CAR"] == "Yes", 1, 0)


# b.Analysis of the potential association between some qualitative variables and the score-variables


par(mfrow=c(1,5))
boxplot(data$ENG_S11 ~ data$GENDER ,main="ENG_S11", xlab="GENDER", ylab="English S11")
boxplot(data$BIO_S11 ~ data$GENDER ,main="BIO_S11", xlab="GENDER", ylab="BIO S11")
boxplot(data$CC_S11 ~ data$GENDER ,main="CC_S11", xlab="GENDER", ylab="CC S11")
boxplot(data$CR_S11 ~ data$GENDER ,main="CR_S11", xlab="GENDER", ylab="CR S11")
boxplot(data$MAT_S11 ~ data$GENDER ,main="MAT_S11", xlab="GENDER", ylab="MAT S11")
par(mfrow=c(1,1))


par(mfrow=c(3,2))
boxplot(data$QR_PRO ~ data$GENDER ,main="QR_PRO", xlab="GENDER", ylab="QR PRO")    
boxplot(data$CR_PRO ~ data$GENDER ,main="CR_PRO", xlab="GENDER", ylab="CR PRO")    
boxplot(data$CC_PRO ~ data$GENDER ,main="CC_PRO", xlab="GENDER", ylab="CC PRO")     
boxplot(data$ENG_PRO ~ data$GENDER ,main="ENG_PRO", xlab="GENDER", ylab="English PRO")   
boxplot(data$WC_PRO ~ data$GENDER ,main="WC_PRO", xlab="GENDER", ylab="WC PRO")    
boxplot(data$FEP_PRO ~ data$GENDER ,main="FEP_PRO", xlab="GENDER", ylab="FEP PRO") 
par(mfrow=c(1,1))


par(mfrow=c(1,5))
boxplot(data$ENG_S11 ~ data$ACADEMIC_PROGRAM  ,main="ENG_S11", xlab="ACADEMIC PROGRAM", ylab="English S11")
boxplot(data$BIO_S11 ~ data$ACADEMIC_PROGRAM  ,main="BIO_S11", xlab="ACADEMIC PROGRAM", ylab="BIO S11")
boxplot(data$CC_S11 ~ data$ACADEMIC_PROGRAM ,main="CC_S11", xlab="ACADEMIC PROGRAM", ylab="CC S11")
boxplot(data$CR_S11 ~ data$ACADEMIC_PROGRAM  ,main="CR_S11", xlab="ACADEMIC PROGRAM", ylab="CR S11")
boxplot(data$MAT_S11 ~ data$ACADEMIC_PROGRAM  ,main="MAT_S11", xlab="ACADEMIC PROGRAM", ylab="MAT S11")
par(mfrow=c(1,1))


par(mfrow=c(3,2))
boxplot(data$QR_PRO ~ data$ACADEMIC_PROGRAM  ,main="QR_PRO", xlab="ACADEMIC PROGRAM", ylab="QR PRO")    
boxplot(data$CR_PRO ~ data$ACADEMIC_PROGRAM  ,main="CR_PRO", xlab="ACADEMIC PROGRAM", ylab="CR PRO")    
boxplot(data$CC_PRO ~ data$ACADEMIC_PROGRAM  ,main="CC_PRO", xlab="ACADEMIC PROGRAM", ylab="CC PRO")     
boxplot(data$ENG_PRO ~ data$ACADEMIC_PROGRAM  ,main="ENG_PRO", xlab="ACADEMIC PROGRAM", ylab="English PRO")   
boxplot(data$WC_PRO ~ data$ACADEMIC_PROGRAM  ,main="WC_PRO", xlab="ACADEMIC PROGRAM", ylab="WC PRO")    
boxplot(data$FEP_PRO ~ data$ACADEMIC_PROGRAM  ,main="FEP_PRO", xlab="ACADEMIC PROGRAM", ylab="FEP PRO") 

library(corrplot)
library(car)
library(moments)

scatterplotMatrix(data[, c(7,11:21)], pch = 16)
skewness(cbind(data$BIO_S11, data$ENG_S11, data$CC_S11,data$MAT_S11))
kurtosis(cbind(data$BIO_S11, data$ENG_S11, data$CC_S11, data$MAT_S11))


# c. Outlier detection based on the 
# score-variables by comparing robust distances3 and classic Mahalanobis distances

data[,11:21] <- scale(data[,11:21])    # Standardizing the data
cov1 <- cov(data[,11:21])
m1 <- apply(data[,11:21] , 2, mean)
distances <- mahalanobis(data[,11:21], m1, cov1)
plot(seq(1:dim(data[,11:21])[1]), distances, 
     main='Detection outliers using Mahalanobis distances',
     xlab='Number of observations') # Added x-axis label
abline(h=qchisq(0.95, 11), col="red")

cutoff<-qchisq(0.95, 11)
cutoff
outliers<-as.data.frame(data[,11:21][distances > cutoff, ])       


# Through Mahalanobis distance (non-robust detection technique), we see 27 observations
# above the cutoff whose value is 18.307, and displayed by red horizontal straight line in the plot.


library(MASS)

h<-floor(dim(data[,11:21])[1]*0.75)
h
resrob<-cov.rob(data[,11:21],quantile.used =h,method = "mcd",cor=TRUE)
resrob$cov
resrob$cor
resrob$best
resrob$center

d1 <- mahalanobis(data[,11:21], m1 ,cov1)
d2 <- mahalanobis(data[,11:21], resrob$center, resrob$cov)
plot(d1, d2, main="Outliers detection using MCD")
abline(v=qchisq(0.975,11), col="red")
abline(h=qchisq(0.975,11), col="red")

x<-qchisq(0.975,11)
x

robust_outliers <-data[,11:21][d1 > x & d2 > x, ]
dim(robust_outliers)[1]


library(car)

data <- data[!(rownames(data) %in% rownames(robust_outliers)), ]   
dim(data)[1]      # 232 observations now after deleting the outliers from the data set

cor(data[,11:21])  
corrplot(cor(data[,11:21]))    
corrplot(resrob$cor)
resrob$cor

correlation_bis <- cor(data[,11:21])
correlation_resrob <- resrob$cor
correlation_resrob
high_corr_pairs <- which(correlation_bis >= 0.7 & correlation_bis < 1, arr.ind = TRUE)

plot(correlation_bis, correlation_resrob, 
     xlab = "Values of the correlation matrix via empirical matrix", 
     ylab = "Values of the correlation matrix via MCD",
     main = "Scatter Plot of Correlations")
points(correlation_bis[high_corr_pairs], correlation_resrob[high_corr_pairs], col="green" ,pch = 16, cex = 1.5)

correlation_bis <- cor(data[,11:21])
correlation_resrob <- resrob$cor
correlation_resrob
high_corr_pairs <- which(correlation_bis >= 0.7 & correlation_bis < 1, arr.ind = TRUE)

plot(correlation_bis, correlation_resrob, 
     xlab = "Values of the correlation matrix via empirical matrix", 
     ylab = "Values of the correlation matrix via MCD",
     main = "Scatter Plot of Correlations",
     xlim = c(0.4, 1), ylim = c(0.4, 1))
points(correlation_bis[high_corr_pairs], correlation_resrob[high_corr_pairs], col="green" ,pch = 16, cex = 1.5)



###################################################
# 3. Further exploration via a dimension reduction#
###################################################

# By using the covariance matrix estimate by MCD method, 

resrob<-cov.rob(data[,c(11:19,21)],quantile.used =h,method = "mcd",cor=TRUE)
resrob$cov
resrob$cor
resrob$best
resrob$center

S <- resrob$cov
resPCA<-eigen(S)
TotVar <- sum(diag(S))
cbind(resPCA$values,resPCA$values/TotVar,cumsum(resPCA$values/TotVar))

plot(resPCA$values, type="b", xlab="Number of principal components", ylab="Explained variance")
abline(h=0.7258976, col="red")


data_bis <-data[,c(11:19,21)]
xbar <- apply(data_bis,2,mean)
y<-as.matrix(data_bis-matrix(xbar,nrow=dim(data_bis)[1],ncol=10,byrow=TRUE))
y <- y %*% resPCA$vectors
y     # scores on 2-D

# The cumulative percentage of explained variance attains at least 72.58 % with two first PCs.
# Scree plot

plot(resPCA$values,type="b")


library(ade4)
corrplot(cor(data[,c(11:19,21)], y))  
s.corcircle(cor(data[,c(11:19,21)], y))

Group_ACADEMIC_PROG <- data$ACADEMIC_PROGRAM
plot(y[,1],y[,2],col=Group_ACADEMIC_PROG+1)   # Industrial Engineering in red dots and Mechatronics Engineering in black dots

data$GENDER <-as.numeric(data[,"GENDER"])
Group_GENDER  <- data$GENDER 
plot(y[,1],y[,2],col=Group_GENDER+1) # Green dots correspond to the male gender while red dots correspond to female gender.


detach(data)