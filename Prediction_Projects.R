#!/usr/bin/env Rscript

####################################
#  Load/Install required packages  #
####################################

load.packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

# load.packages(dplyr)
load.packages(leaps)
load.packages(glmnet)

##################################################
#  load data and partition into train/test data  #
##################################################

setwd(".")      # set current directory to be the working directory
set.seed(0x1234)

data.total = read.table("transcoding_mesurment.tsv", sep="\t", header=TRUE)

nrows = dim(data.total)[1]
data.total = data.total[sample(nrows+1), ]          # shuffle the data frame
data.total = data.total[, -c(2, 15)]                # remove two categority variables for simplicity

nfeat = dim(data.total)[2]

data.index = 1:(nrows/2)
# data.index = 1:(nrows-5)
data.train = data.total[data.index, ]
data.train.x = data.total[data.index, 1:(nfeat-1)]
data.train.y = data.total[data.index, nfeat]
data.test.x = data.total[-data.index, 1:(nfeat-1)]
data.test.y = data.total[-data.index, nfeat]

###########################################
#  Linear regression with all predictors  #
###########################################

ols.model   = lm(utime ~ ., data.train)
summary(ols.model)
names(ols.model)
observed.values1 =data.train$utime 
predicted.values1 = ols.model$fitted.values

training.rmse    = sqrt(   mean( (observed.values1 - predicted.values1)^2 )   )
training.rmse 

#########################################################
#  Linear regression with variables selected using BIC  #
#########################################################

bic.simple.subsets = regsubsets(utime ~ .,     data = data.train, nvmax = 16)
bic.simple.values = summary(bic.simple.subsets)$bic
# bic.simple.subsets

u1       = toString(names(coef(bic.simple.subsets, 16))[-1])
u2       = gsub(pattern = ", ",  replacement = " + ", x = toString(u1))
formula  = as.formula(paste("utime ~ ", u2, sep = ""))

bic.simple.model = lm(formula, data.train)

######################
#  Lasso regression  #
######################

lasso.model = cv.glmnet(x = as.matrix(data.train.x), y = matrix(data.train.y), alpha = 1)
# lasso.model$lambda.min
lasso.coef  = coef(lasso.model, s = lasso.model$lambda.min)

######################
#  Ridge regression  #
######################

ridge.model = cv.glmnet(x = as.matrix(data.train.x), y = as.matrix(data.train.y), alpha = 0)
# ridge.model$lambda.min
ridge.coef  = coef(ridge.model, s = ridge.model$lambda.min)

##########
#  Test  #
##########

rmse = function(a,b) { sqrt(mean((a-b)^2)) }

# ols model
print(rmse(data.test.y, predict(ols.model, data.test.x)))
# ols model with BIC
print(rmse(data.test.y, predict(bic.simple.model, data.test.x)))
# lasso
print(rmse(as.matrix(data.test.y), predict(lasso.model, newx = as.matrix(data.test.x), s = lasso.model$lambda.min)))
# ridge
print(rmse(as.matrix(data.test.y), predict(ridge.model, newx = as.matrix(data.test.x), s = ridge.model$lambda.min)))
