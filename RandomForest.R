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




