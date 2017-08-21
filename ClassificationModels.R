# read in the file, normalize values
default <- read.csv("/Volumes/SAVE2HERE/Default.csv")
normalize<-function(x) {return((x-min(x))/(max(x)-min(x)))}
default2 <- cbind(as.data.frame(lapply(default[,4:5], normalize)), default[, 2:3])

# training and testing data
training_i <- sample(1:nrow(default2), size = .70*nrow(default2))
training <- default2[training_i, ]
testing <- default2[-training_i, ]

# Predict default using knn
require(class)
# how to include student in the prediction as well?
m1 <- knn(train = training[,c(1,2)], test = testing[, c(1,2)], cl = training[, 3], k = 1)
m2 <- knn(train = training[,c(1,2)], test = testing[, c(1,2)], cl = training[, 3], k = 3)
m3 <- knn(train = training[,c(1,2)], test = testing[, c(1,2)], cl = training[, 3], k = 5)


# best k
require(caret)
require(e1071)
set.seed(3333)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_best <- train(default ~., data = default2, method = "knn", trControl = trctrl, preProcess = c("center", "scale"), tuneLength = 10)
knn_best

# logistic regression

lr1 <- glm(default ~ income + balance + factor(student), family = binomial(), data = training)
summary(lr1)
pred.prob <- predict(lr1, type = "response")
df <- data.frame(truth = training$default, predicted = pred.prob, balance = training$balance)
p <- ggplot(df, aes (balance, predicted)) + geom_point()
p2 <- p + geom_point(aes(color = truth))
p2 + geom_smooth(span = .5, col = "black")
pred.def <- pred.prob > .5


# Bayes classification technique

# LDA Classification
require(MASS)
lda1 <- lda(default ~balance + income, data = training)
summary(lda1)
lda.pred <- predict(lda1, training)
names(lda.pred)
lda.class <- lda.pred$class
mean(lda.class == 'Yes') # default rate
sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] <.5)
sum(lda.pred$posterior[,1] >= .9)
sum(lda.pred$posterior[,1] <.1)

# Confusion Matrices

### KNNS
table(factor(testing$default), m1)
table(factor(testing$default), m2)
table(factor(testing$default), m3)

### Logistic Regressions
table(pred.def, factor(training$default))

### LDA
table(lda.class, training$default)

# Use testing data to test your classifiers

### Logistic Regression
test_pred1 <- predict(lr1, newdata = testing)
test_val1 <- rep(NA, 3000)
for (i in 1:3000){
  if (test_pred1[i] >= .5){
    test_val1[i] <- "Yes"
  } else {
    test_val1[i] <- "No"
  }
}
table(factor(testing$default), factor(test_val))

