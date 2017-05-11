###
### Last Name:  < Zhang >
### First Name: < Yahui >
### Kaggle ID:  < YahuiZhang >
###

###
### Next, *install* and *load* all the needed packages:
###
load.package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

load.package(caret)
load.package(pROC)
install.packages("h2o")
library(h2o)

###
### Next, put your best model construction here.
###
localH2O = h2o.init(nthreads = -1)
train = h2o.importFile(path = "C:/Users/Jiawen/Desktop/Statisticlearning/train422.csv")
mnist_test = h2o.importFile(path = "C:/Users/Jiawen/Desktop/Statisticlearning/test422.csv")
train$C785 = as.factor(train$C785)

splits <- h2o.splitFrame(train, c(0.8,0.1), seed=1234)
train  <- h2o.assign(splits[[1]], "train.hex") # 80%
valid  <- h2o.assign(splits[[2]], "valid.hex") # 10%
test   <- h2o.assign(splits[[3]], "test.hex")  # 10%

set.seed(1105)
h2o.fit =  h2o.deeplearning(x = 1:784, y = 785, training_frame = train, validation_frame=valid,
                            activation = "RectifierWithDropout", input_dropout_ratio = 0.2,
                            hidden_dropout_ratios = c(0.5,0.5), adaptive_rate = TRUE,
                            balance_classes = TRUE, hidden = c(800,800), epochs = 500)

###
### Best Result: <Put your best result here.>
### Time:        <Put how long it takes (in minutes) for the final model construction>
###
yhat <- h2o.predict(h2o.fit, mnist_test)
predictions <- cbind(as.data.frame(train$C785),as.data.frame(yhat[,1]))
predictions$predict
##Time:
system.time({h2o.fit =  h2o.deeplearning(x = 1:784, y = 785, training_frame = train, validation_frame=valid,
                                         activation = "RectifierWithDropout", input_dropout_ratio = 0.2,
                                         hidden_dropout_ratios = c(0.5,0.5), adaptive_rate = TRUE,
                                         balance_classes = TRUE, hidden = c(800,800), epochs = 500)})

######################################################################
######################################################################
######################################################################
###
### From here on, put everything else like your tuning, other stuff
### you tried, etc.
###

####I tried random forest and tuned the following parameters, including hidden layers, epochs, and learning rate

###Random forest
print("Random forest")
fit1 = train(V785 ~ ., data=mnist_train, trControl=train.control, method="rf")
pred = predict(fit1, newdata=mnist_test)
confusion_matrix_final = table(predicted = pred, actual = mnist_test$V785)
error_rate_final = 1 - ( sum(diag(confusion_matrix_final)) / nrow(mnist_test))
error_rate_final
### djust the hidden layer to (100,100), (100,200)
### Adjust the epochs to 100, 200, 300, 800, 900, 1000
### Adjust the rate to 0.001,00001

