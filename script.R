# The data is about Medical Insurance, some details about the insuree
# and the charges associated with the Insurance.

data <- read.csv("./insurance2.csv") # choose the location of the file
data1 <- subset(data,select = c(age,bmi,smoker,insuranceclaim,region)) # choosing a set of covariates for the model

age<-data$age
sex<-data$sex
bmi<-data$bmi
children<-data$children
smoker<-data$smoker
region<-data$region
prev_claims<-data$insuranceclaim
charges<-data$charges

reg = lm(charges~age+sex+bmi+smoker+children+prev_claims+region,data = data) # Fitting the Multiple Linear Regression model
summary(reg) # Summary of the regression

library(ppcor) # Library for calculating different types of correlations
cor(data) # Covariance matrix of the data
pcor(data) # Partial Correlations
spcor(data) # Semi-partial Correlations

reg_ = lm(charges~age+bmi+smoker+prev_claims+region,data = data1) # Regressing with covariates chosen after observing the p-values
summary(reg_)

# Simulation : Simulating a part of data to answer about the insurance cost with incomplete given data
beta <- reg$coefficients
i <- matrix(1,nrow=1,ncol=10000)
i_age<-runif(10000,min=50,max=60)
i_sex<-matrix(0,nrow=1,ncol=10000)
i_bmi<-runif(10000,min=23,max=27)
u_smoker<-runif(10000)
i_smoker<-quantile(data$smoker,u_smoker)
# u_children<-runif(10000)
i_children<-matrix(0,nrow=1,ncol=10000)
i_prev_claims<-matrix(1,nrow = 1,ncol = 10000)
# u_region<-runif(10000)
i_region<-matrix(0,nrow=1,ncol=10000)
X <- matrix(c(i,i_age,i_sex,i_bmi,i_smoker,i_children,i_prev_claims,i_region),nrow=10000,ncol=8) 
mean(X %*% beta) # Getting the costs for question 1, first model

beta_ <- reg_$coefficients
X_ <- matrix(c(i,i_age,i_bmi,i_smoker,i_prev_claims,i_region),nrow=10000,ncol=6)
mean(X_ %*% beta_) # Getting the costs for question 1, better model

