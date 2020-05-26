library(alr3)
library(MASS)
wsj<-read.csv("[[WSJ DATA]]")

fit<- lm(DEFAULT_RATE ~ NETPRICE + INSTITUTION_GROUP , data = wsj)

predict(fit, data.frame(NETPRICE = 15000, INSTITUTION_GROUP = "bachelor"), interval = "confidence", level = .95)

x<-wsj[wsj$INSTITUTION_GROUP == "bachelor", ]
# mean price of bachelor degree institutions
y<-wsj[wsj$INSTITUTION_GROUP == "associate", ]
# mean price of associate degree institutions
z<- wsj[wsj$INSTITUTION_GROUP == "certificate", ]

data.frame(prices = c(mean(x$NETPRICE, na.rm = TRUE), mean(y$NETPRICE, na.rm = TRUE), 
                      mean(z$NETPRICE, na.rm = TRUE)), 
           default_rates = c(mean(x$DEFAULT_RATE,na.rm = TRUE), 
                             mean(y$DEFAULT_RATE, na.rm = TRUE), 
                             mean(z$DEFAULT_RATE, na.rm = TRUE)), 
           row.names = c("bachelors", "associate", "certificate"))

fit2<-lm(DEFAULT_RATE ~ MEDIAN_BORROWING + MEDIAN_PAYMENT + NETPRICE + GRAD_RATE, data = wsj)
vif(fit2)

par(mfrow = c(1,1))
mmp(model = fit3, fit3$model$NETPRICE)

mmp(fit_3, fit_3$model$`I(NETPRICE^0.5)`)

par(mfrow = (c(2,2)))
plot(fit4)
vif(fit4)
mmps(fit4)

par(mfrow = c(1,2))
wsj2<-transform(wsj, pos_def_rate = wsj$DEFAULT_RATE + 1)
wsj2<-transform(wsj, pos_def_rate = wsj$DEFAULT_RATE + 1)
wsj2<-transform(wsj2, pos_netprice = wsj$NETPRICE + 20747)
wsj2<-transform(wsj2, pos_grad_rate = wsj$GRAD_RATE + 1)
fit_5<-lm(pos_def_rate ~ pos_netprice + pos_grad_rate + MEDIAN_BORROWING, data = wsj2)
invResPlot(fit_5)
boxcox(fit_5)

wsj2<-transform(wsj2, def_rate_transformed = sqrt(wsj2$pos_def_rate))
fit6<-lm(def_rate_transformed ~ pos_netprice + pos_grad_rate + MEDIAN_BORROWING, data = wsj2)
par(mfrow = c(2,2))
plot(fit6)
vif(fit6)
mmps(fit6)


