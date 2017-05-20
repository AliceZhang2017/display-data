install.packages("h2o")
library(h2o)

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

yhat <- h2o.predict(h2o.fit, mnist_test)
predictions <- cbind(as.data.frame(mnist_test$C785),as.data.frame(yhat[,1]))
h2o.confusionMatrix(h2o.fit,test)

##Store the results
Id = c(1:10000)
predictions$predict
y = as.vector(predictions$predict)
submit = cbind(Id, y)

write.table(submit, file="AliceFinal9.csv", row.names=FALSE, sep=",")