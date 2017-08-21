## Based on Boston Data in R

# Fitting Regression Trees

library(MASS)
library(tree)
set.seed(12345)
train = sample(1:nrow(Boston), nrow(Boston)/2)
head(Boston)

tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(prune.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test,ylab="Actual medv values",xlab="Estimated medv values")
abline(0,1)
title("Plotting Actual vs Predicted Values")

mean((yhat-boston.test)^2)

summary(lm(boston.test~yhat))

glm.medv <-lm(medv~.,data=Boston,subset=train)
summary(glm.medv)
library(car)
vif(glm.medv)

yhat2<-predict(glm.medv,newdata=Boston[-train,])
plot(yhat2,boston.test,ylab="Actual medv values",xlab="Estimated medv values using lm")
abline(0,1)
title("Plotting Actual vs Predicted Values")

mean((yhat2-boston.test)^2)

################################
#Random Forest
require(randomForest)
dim(Boston)
set.seed(12345)
bag0=randomForest(medv~.,data=Boston[train,],mtry=13,importance=TRUE)
summary(bag0)
plot(bag0)

importance(bag0)
varImpPlot (bag0)

bag=randomForest(medv~.,data=Boston[train,],mtry=13,importance=TRUE,ntree=100)
summary(bag)
plot(bag)
print(bag)
yhat3<-predict(bag,newdata=Boston[-train,])
plot(yhat3,boston.test,ylab="Actual medv values",xlab="Estimated medv values using RF")
abline(0,1)
title("Plotting Actual vs Predicted Values")

mean((yhat3-boston.test)^2)
summary(lm(boston.test~yhat3))


importance(bag)
varImpPlot (bag)

bagn=randomForest(medv~.,data=Boston[train,],mtry=6,importance=TRUE,ntree=100)
summary(bagn)
plot(bagn)
print(bagn)
yhat4<-predict(bagn,newdata=Boston[-train,])
plot(yhat4,boston.test,ylab="Actual medv values",xlab="Estimated medv values using RF")
abline(0,1)
title("Plotting Actual vs Predicted Values")

mean((yhat4-boston.test)^2)
summary(lm(boston.test~yhat4))
importance(bagn)
varImpPlot (bagn)

# Boosting:

# install.packages("gbm")

library (gbm)
set.seed (1234)
boost.boston<- gbm(medv~.,data=Boston[train,], distribution="gaussian",n.trees =5000 , interaction.depth =4)
summary (boost.boston)

# We see that lstat and rm are by far the most important variables. We can also produce partial dependence plots 
# for these two variables. These plots partial dependence plot illustrate the 
# marginal effect of the selected variables on the response after integrating 
# out the other variables. In this case, as we might expect, median
# house prices are increasing with rm and decreasing with lstat.

par(mfrow =c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")


yhat.boost=predict (boost.boston ,newdata =Boston [-train ,],n.trees =5000)
mean((yhat.boost -boston.test)^2)

# If we want to, we can perform boosting with a different value of the 
# shrinkage parameter ?? (Default is 0.001)

# Here we take ?? = 0.2

boost.boston =gbm(medv~.,data=Boston [train ,], distribution=
                    "gaussian",n.trees =5000 , interaction.depth =4, shrinkage =0.2,verbose =F)
par(mfrow=c(1,1))
summary(boost.boston)
yhat.boost=predict (boost.boston ,newdata =Boston[-train ,],
                      n.trees =5000)
mean(( yhat.boost -boston.test)^2)



### Based on LA Fire Dept data to predict elapsed times

# Random Forest on Mo’s Dataset +n_distinct column ------------------------

load("/Users/mvembusubramanian/Downloads/p.Rdata")
require(dplyr)
lafd_train <- left_join(lafd_train, p)
lafd_test <-left_join(lafd_test,p)
set.seed(123)
subset <- sample(1:nrow(lafd_train), 150000)
lafd_train2 <- lafd_train[subset, ]   #make dataset smaller so R doesn't get upset
lafd_train2 <- lafd_train2[complete.cases(lafd_train2), ] # not sure if these needs to happen
names(lafd_train2)[38] <- "distinct_row_ids"
names(lafd_test)[38] <- "distinct_row_ids"


### Initial Models -------------------------------
rf <- randomForest(transformed ~., data = lafd_train2[, c(3,6:38)], mtry = 5, ntree = 100, importance = TRUE)

plot(rf)
varImpPlot(rf)
importance(rf)

### Make Some Predictions

RF_pred1 <- predict(rf, newdata = lafd_test[, c(3,6:38)])
mean((exp(RF_pred1) - exp(lafd_test$transformed))^2, na.rm = TRUE)  ### 1.543196 million

### Improve Random forest ------------------------

rf2 <- randomForest(transformed ~., data = lafd_train2[, c(3,6:38)], mtry = 7, ntree = 50, importance = TRUE)
RF_pred2 <- predict(rf2, newdata = lafd_test[, c(3,6:38)])
mean((exp(RF_pred2) - exp(lafd_test$transformed))^2, na.rm = TRUE) ### 1.515 million

impute <- function(x){
  if(is.na(x)){
    val <- exp(6.016886)
  } else {
    val <- x
  }
  return(val)
}

y_hat <- sapply(pred$prediction, impute)
pred$prediction <- y_hat
write.csv(pred, file = "/Users/mvembusubramanian/Documents/Stats 101C/Stats 101C Final/RFpredicts.csv")
