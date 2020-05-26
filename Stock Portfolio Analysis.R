### Set up the Data

stocks <- read.csv("/Users/mohini_vem/Downloads/stocks.csv")   ### read in the file
stocks_training <- stocks[1:61,] #rows up until Jan 2015    #### years to train
stocks_testing<- stocks[61:nrow(stocks), ] #the rest   #### years to test

### Convert into Returns

simp_returns_2015 <- 
(stocks_training[-1,3:ncol(stocks_training)]-stocks_training[-nrow(stocks_training),3:ncol(stocks_training)])/stocks_training[-nrow(stocks_training),3:ncol(stocks_training)] # only up until 2015

simp_returns_2018 <- 
(stocks_testing[-1,3:ncol(stocks_testing)]-stocks_testing[-nrow(stocks_testing),3:ncol(stocks_testing)])/stocks_testing[-nrow(stocks_testing),3:ncol(stocks_testing)]

### Finding the means and variances 

R_bar <- apply(MARGIN = 2,X = simp_returns_2015, FUN = mean, na.rm = TRUE)[2:31] # mean vec of the 30 stocks
var_cov <- cov(simp_returns_2015)
sd <- apply(MARGIN = 2,X = simp_returns_2015, FUN = sd, na.rm = TRUE)[2:31] # standard deviation of the 30 stocks and S&P
var_cov30 <- var_cov[2:31,2:31] # just the 30 stocks variance
one <- rep

### Equal Allocation Portfolio

x <- rep(1/30, 30) # shares of each stock
(R_p <- t(x)%*%R_bar[2:31]) # mean of portfolio
(var_por <- t(x) %*% var_cov30 %*% x)

### Min Risk Portfolio

(Rp_min <- (t(rep(1,30) %*% solve(var_cov30) %*% R_bar[2:31]))/(t(rep(1,30)) %*% solve(var_cov30) %*% rep(1,30)))
(var_min <- 1/(t(rep(1,30)) %*% solve(var_cov30) %*% rep(1,30)))
sqrt(var_min)
sqrt(var_por) # sqrt of portfolio

### Computing the alphas, betas, and errors from SIM for each stock

beta1 <- rep(NA, 30) 
var_beta1 <- rep(NA,30)
beta_adj1 <- rep(NA,4)

for (i in 2:31){ #for each stock
  mod <- lm(data = simp_returns_2015, simp_returns_2015[,i] ~ simp_returns_2015[,1])
  beta1[i-1] <- mod$coefficients[2]
  var_beta1[i-1] <- vcov(mod)[2,2]
}

(b1_bar <- mean(beta1))
(sigma_b1 <- var(beta1))

### Construct var-cov matrix with the simple index model

sigma_m <- var(simp_returns_2015$X.GSPC) # market variance estimate 
vcov <- matrix(NA, 30,30)
for (i in 1:30){
  for (j in 1:30){
    if (i == j){
      vcov[i,j] <- var_beta1[i] # put the variance of each beta in the diagonal
    } else {
      vcov[i,j] <- beta1[i]*beta1[j]*sigma_m # the covariance terms
    }
  }
}

### Adjust the betas with Vasicek's technique

for(i in 1:30){
  beta_adj1[i] <- var_beta1[i]*mean(beta1)/(var(beta1)+var_beta1[i]) +
    var(beta1)*beta1[i]/(var(beta1)+var_beta1[i])
}

### Adjust the betas with Blume's technique (need 2 periods)

### Blume's Method
beta2 <- rep(NA, 30) 
var_beta2 <- rep(NA,30)

for (i in 2:31){ #for each stock
  mod <- lm(data = simp_returns_2018, simp_returns_2018[,i] ~ simp_returns_2018[,1])
  beta2[i-1] <- mod$coefficients[2]
  var_beta2[i-1] <- vcov(mod)[2,2]
}

blume <- lm(beta2 ~ beta1)
beta3 <- blume$coefficients[1]+blume$coefficients[2]*beta2
as.data.frame(beta3)

### Calculate the PRESS for each method

mean((beta_adj1 - beta2)^2)

### Constant Correlation Model

## we will use Rf = .0164
R_bar <- apply(MARGIN = 2,X = simp_returns_2015, FUN = mean, na.rm = TRUE)[2:31]
excess_sig <- (R_bar - .0164)/sd
excess_sig2 <- sort(excess_sig,decreasing = TRUE)
sd_sort <- sd[names(sort(excess_sig, decreasing = TRUE))]
p_bar <- 0
for (i in 1:30){
  for (j in 1:30){
    if (i == j){
      next
    } else{
      p_bar <- p_bar + cor(simp_returns_2015[,i], simp_returns_2015[,j])
    }
  }
}
(p_bar <- p_bar/(29*30))
col1 <- rep(NA, 30)
for (i in 1:30){
  col1[i] <- p_bar/(1 - p_bar + i*p_bar)
}
col2 <- cumsum(excess_sig2)
Ci <- col1 * col2

### With short sales allowed
Cstar1 <- Ci[30] # last value in C vector
z1 <- 1/(sd_sort*(1-p_bar))*(excess_sig2 - Cstar1)
x_cc_ss <- z1/sum(z1)

### With no short sales
Cstar2 <- max(Ci)
which(Ci == max(Ci)) # only keep the first 5 stocks
z2 <- 1/(sd_sort[1:5]*(1-p_bar))*(excess_sig2[1:5]-Cstar2)
x_cc_nss <- z2/sum(z2)

### MultiGroup Model with short sales

simp_returns_tech <- simp_returns_2015[, 2:7]   # have to group the stocks by industry first
simp_returns_cg <- simp_returns_2015[, 8:13]
simp_returns_fin <- simp_returns_2015[, 14:19]
simp_returns_health <- simp_returns_2015[, 20:25]
simp_returns_serv <- simp_returns_2015[, 26:31]

ptech<-mean(cor(simp_returns_tech)[lower.tri(cor(simp_returns_tech))])
pcg<-mean(cor(simp_returns_cg)[lower.tri(cor(simp_returns_cg))])
pfin<-mean(cor(simp_returns_fin)[lower.tri(cor(simp_returns_fin))])
phealth<-mean(cor(simp_returns_health)[lower.tri(cor(simp_returns_health))])
pserv<-mean(cor(simp_returns_serv)[lower.tri(cor(simp_returns_serv))])

p12 <-mean(cor(cbind(simp_returns_tech, simp_returns_cg))[lower.tri(cor(cbind(simp_returns_tech, simp_returns_cg)))])
p13 <- mean(cor(cbind(simp_returns_tech, simp_returns_fin))[lower.tri(cor(cbind(simp_returns_tech, simp_returns_fin)))])
p14 <- mean(cor(cbind(simp_returns_tech, simp_returns_health))[lower.tri(cor(cbind(simp_returns_tech, simp_returns_health)))])
p15 <- mean(cor(cbind(simp_returns_tech, simp_returns_serv))[lower.tri(cor(cbind(simp_returns_tech, simp_returns_serv)))])
p23 <- mean(cor(cbind(simp_returns_cg, simp_returns_fin))[lower.tri(cor(cbind(simp_returns_cg, simp_returns_fin)))])
p24 <- mean(cor(cbind(simp_returns_cg, simp_returns_health))[lower.tri(cor(cbind(simp_returns_cg, simp_returns_health)))])
p25 <- mean(cor(cbind(simp_returns_cg, simp_returns_serv))[lower.tri(cor(cbind(simp_returns_cg, simp_returns_serv)))])
p34 <- mean(cor(cbind(simp_returns_fin, simp_returns_health))[lower.tri(cor(cbind(simp_returns_fin, simp_returns_health)))])
p35 <- mean(cor(cbind(simp_returns_fin, simp_returns_serv))[lower.tri(cor(cbind(simp_returns_fin, simp_returns_serv)))])
p45 <- mean(cor(cbind(simp_returns_health, simp_returns_serv))[lower.tri(cor(cbind(simp_returns_health, simp_returns_serv)))])

p <- matrix(c(ptech, p12, p13, p14, p15, p12, pcg, p23, p24, p25, p13, p23, pfin, p34, p35, p14, p24, p34, phealth, p45, p15, p25, p35, p45, pserv), nrow = 5  )


### find your A and C matrix
A <- matrix(NA, 5,5)
for (i in 1:5){
  for (j in 1:5){ # what does the ith row look like
    if (i == j){
      A[i,j] <- 1 + (5*p[i,j])/(1-p[i,j])
    } else {
      A[i,j] <- (5*p[i,j])/(1 - p[i,i])
    }
  }
}

C <- c( sum((R_bar[1:6] - .0164)/(sd[1:6]*(1 - p[1,1]))) , sum((R_bar[7:12] - .0164)/(sd[7:12]*(1 - p[2,2]))) , sum((R_bar[13:18] - .0164)/(sd[13:18]*(1 - p[3,3]))) , sum((R_bar[19:24] - .0164)/(sd[19:24]*(1 - p[4,4]))) , sum((R_bar[25:30] - .0164)/(sd[25:30]*(1 - p[5,5]))) )

phi <- solve(A) %*% C
phi
Ci <- rep(NA, 5)
for ( i in 1:5){
  for ( j in 1:5){
    Ci[i] <- sum(p[i,j] * phi[j])
  }
}
z <- rep(NA, 30)
z[1:6] <- (1/(sd[1:6]*(1-p[1,1]))) * (excess_sig[1:6] - Ci[1])
z[7:12] <- (1/(sd[7:12]*(1-p[2,2]))) * (excess_sig[7:12] - Ci[2])
z[13:18] <- (1/(sd[13:18]*(1-p[3,3]))) * (excess_sig[13:18] - Ci[3])
z[19:24] <- (1/(sd[19:24]*(1-p[4,4]))) * (excess_sig[19:24] - Ci[4])
z[25:30] <- (1/(sd[25:30]*(1-p[5,5]))) * (excess_sig[25:30] - Ci[5])
x_mgr <- z/sum(z)
x_mgr
names(x_mgr) <- names(excess_sig)

## the indices might be different based on how the final data has its industries distributed

### Simple Index Model Optimal Portfolio

#### Fix the Betas
beta1 <- rep(NA, 30) 
var_beta1 <- rep(NA,30)
alphas <- rep(NA,30)
var_errors <- rep(NA,30)

for (i in 2:31){ #for each stock
  mod <- lm(data = simp_returns_2015, simp_returns_2015[,i] ~ simp_returns_2015[,1])
  beta1[i-1] <- mod$coefficients[2] # the slopes
  alphas[i-1] <- mod$coefficients[1] # the intercepts
  var_errors[i-1] <- var(mod$residuals) # get the variance of the errors
  var_beta1[i-1] <- vcov(mod)[2,2] # the variance of the coefficients
}
names(beta1) <- colnames(simp_returns_2015)[2:31]
names(var_errors) <- colnames(simp_returns_2015)[2:31]
sum(beta1 >= 0) # all of our betas are greater than 0 so we will use all of our stocks

## we will use Rf = .0164
R_bar <- apply(MARGIN = 2,X = simp_returns_2015, FUN = mean, na.rm = TRUE)[2:31]
excess_beta <- (R_bar - .0164)/beta1
excess_beta <- sort(excess_beta,decreasing = TRUE) # sort in decreasing order
beta1 <- beta1[names(excess_beta)] # sort by beta ratio
var_errors <-var_errors[names(excess_beta)] # ""
thing1 <- ((R_bar[names(excess_beta)] - .0164)*beta1)/var_errors
sum1 <- cumsum(thing1)
thing2 <- beta1^2/var_errors
sum2 <- cumsum(beta1^2/var_errors)
sigma_m <- var(simp_returns_2015$SPY) # market variance
C <- (sigma_m * sum1)/(1 + sigma_m*sum2)

### With short sales allowed
Cstar1 <- C[30] # last value in C vector
z1 <- (beta1/var_errors)*(excess_beta - Cstar1)
x_sim_ss <- z1/sum(z1)

### With no short sales
##### want the stocks that have excess ratios less than Cstar2
Cstar2 <- max(C)
# only keep the first 9 stocks
z2 <- (beta1[1:9]/var_errors[1:9])*(excess_beta[1:9] - Cstar2)
x_sim_nss <- z2/sum(z2)

### Get the Portfolios in a Dataset

### The shares of each stock
x_equal <- rep(x = 1/30, 30)
x_min <- (solve(var_cov30) %*% one)/as.numeric((t(one) %*% solve(var_cov30) %*% one))
x_cc_ss <- x_cc_ss
x_cc_nss2 <- c(x_cc_nss, rep(0, 25))
x_mgr <- x_mgr
x_sim_ss <- x_sim_ss
x_sim_nss <- c(x_sim_nss, rep(0, 21)) 

### Now put together the dataset

equal <- rep(NA, 60)
Markowitz <- rep(NA,60)
CC_SS <-  rep(NA,60)
CC_NSS <-  rep(NA,60)
MGR <-  rep(NA,60)
SIM_SS <-  rep(NA,60)
SIM_NSS <-  rep(NA,60)
shares <- data.frame(x_equal, x_min, x_mgr, x_cc_ss, x_cc_nss2, x_sim_nss, x_sim_ss)
portfolios <- data.frame(matrix(data = NA, nrow = 60, ncol = 7))

for (i in 1:60){ #cycle through each point in time
  for (j in 1:7){ #cycle through each portfolio type
    portfolios[i,j] <- as.numeric(t(shares[,j]) %*% t(simp_returns_2015[i, 2:31]))
  }
}

colnames(portfolios) <- colnames(shares)
portfolios$market <- simp_returns_2015$SPY 

### Time Plots

plot(x = 1:60, y = portfolios$market, 
     col = "black", type = "l", 
     ylim = c(-.2,1), ylab = "Returns", xlab = "Index")
for (i in 1:7){
  lines(x = 1:60, y = portfolios[,i], col = rainbow(30)[i])
}
legend("topright", legend = c("market", 
                              colnames(portfolios)[1:7]), 
       fill = c("#000000", rainbow(30)[1:7])) 

### Sharpe Ratio

```{r}
sharpe <- rep(NA, 8)
for (i in 1:8){
  sharpe[i] <- (mean(portfolios[,i]) - .0164)/sd(portfolios[,i])
}

names(sharpe) <- colnames(portfolios)
print(sort(sharpe, decreasing = TRUE)) 
```

### Differential Excess Return

### Differential excess return
## R(a) - R(a')

Raprime <- rep(NA, 7)
diff <- rep(NA,7)
for (i in 1:7){
  Raprime[i] <- .0164 - ((mean(portfolios$market) - .0164)/sd(portfolios$market))*sd(portfolios[,i])
  diff[i] <- mean(portfolios[,i] - Raprime[i])
}
names(diff) <- colnames(portfolios)[1:7]
print(sort(diff, decreasing = TRUE)) 

### Treynor Measure

trey <- rep(NA, 7)
betap <- rep(NA, 7)
for (i in 1:7){ #for each stock
  mod <- lm(data = portfolios, portfolios[,i] ~ portfolios$market)
  betap[i] <- mod$coefficients[2] # the slopes
  trey[i] <- (mean(portfolios[,i]) - .0164)/betap[i]
}
names(trey) <- colnames(portfolios)[1:7]
print(sort(trey, decreasing = TRUE)) 

### Jensen's Measure

Raprime <- rep(NA, 7)
jensen <- rep(NA,7)
for (i in 1:7){
  Raprime[i] <- .0164 - ((mean(portfolios$market) - .0164))*betap[i]
  jensen[i] <- mean(portfolios[,i]) - Raprime[i]
}
names(jensen) <- colnames(portfolios)[1:7]
print(sort(jensen, decreasing = TRUE)) 

### Fama's Decomposition

### overall performanace
overall <- apply(X = portfolios[,1:7], MARGIN = 2, FUN = mean) - .0164
print(sort(overall, decreasing = T))

### net selectivity
# Ra - Rm*Ba''
Ba_dubprim <- sqrt(apply(X = portfolios[,1:7], MARGIN = 2, FUN = var)/sd(portfolios$market))
net <- apply(X = portfolios[, 1:7], MARGIN = 2, FUN = mean) - mean(portfolios$market)*Ba_dubprim
print(sort(net, decreasing = T))

### diversification

div <- jensen - net
print(sort(div, decreasing = T))

### selectivity
print(sort(jensen, decreasing = T))

### Binomial Pricing Model

t <- 65/252
set.seed(123)
n <- 30
u <- exp(ann_vol*sqrt(t/n))
d <- 1/u

p <- (exp(r*t/n)-d) / (u-d)
p1 <- (p*u) / exp(r*t/n)

k <- log(E/(d^n*S0)) / log(u/d)

C <- S0 * pbinom(ceiling(k)-1, n, p1, lower.tail=FALSE) - 
  E*exp(-r*t)*pbinom(ceiling(k)-1, n, p, lower.tail=FALSE)
print(C)

## Black Schole's Model

t <- 65/252
d1 <- (log(S0/E) + (r+.5*sig^2)*t) / (sig*sqrt(t))
d2 <- d1 - sig*sqrt(t)

C <- S0*pnorm(d1) - E*exp(-r*t)*pnorm(d2)
print(C) 

### Implied Volatility

#Inputs:
s0 <- 21
E <- 20
r <- 0.1
t <- 0.25
c <- 1.875
#Initial value of volatility:
sigma <- 0.10
sig <- rep(0,10)
sig[1] <- sigma
#Newton-Raphson method:
for(i in 2:100){
  d1 <- (log(s0/E)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
  d2 <- d1-sigma*sqrt(t)
  f <- s0*pnorm(d1)-E*exp(-r*t)*pnorm(d2)-c
  #Derivative of d1 w.r.t. sigma:
  d11 <- (sigma^2*t*sqrt(t)-(log(s0/E)+(r+sigma^2/2)*t)*sqrt(t))/(sigma^2*t)
  #Derivative of d2 w.r.t. sigma:
  d22 <- d11-sqrt(t)
  #Derivative of f(sigma):
  f1 <- s0*dnorm(d1)*d11-E*exp(-r*t)*dnorm(d2)*d22
  #Update sigma:
  sigma <- sigma - f/f1
  sig[i] <- sigma
  if(abs(sig[i]-sig[i-1]) < 0.00000001){sig<- sig[1:i]; break}
}
print(sig)
















