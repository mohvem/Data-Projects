library(tidyverse)
library(lubridate)
library(janitor)

### Set up the Data

rm(list = ls())
options(scipen = 999)
stocks <- read.csv("stockData 2020.csv")   ### read in the file
stocks_long <- stocks  %>% 
  gather(stock,price,-c(X, Date)) %>% 
  clean_names() %>% 
  mutate(date = as.Date(date))
### Build Models using Pre 2015 data
stocks_training <- stocks_long %>%  
  filter(date < make_date(2018,1,1))
### Test Models using 2018 +  data
stocks_testing <- stocks_long %>% 
  filter(date >= make_date(2018,1,1))

### Convert into Returns

simp_returns_train <- stocks_training %>% 
  group_by(stock) %>% 
  mutate(ret = price/lag(price) - 1) %>% 
  select(-price)

simp_returns_test <- stocks_testing %>% 
  group_by(stock) %>% 
  mutate(ret = price/lag(price) - 1) %>% 
  select(-price)

### Finding the means 
r_bar_train <- simp_returns_train %>% 
  summarise(r_bar = mean(ret, na.rm = T),
            sd = sd(ret, na.rm = T))

r_bar_test <- simp_returns_test %>% 
  summarise(r_bar = mean(ret, na.rm = T),
            sd = sd(ret, na.rm = T))

### Finding the variances
var_train <- simp_returns_train %>% 
  filter(!is.na(ret)) %>% 
  spread(stock, ret) %>% 
  select(-date,-x) %>% 
  as.matrix() %>% 
  cov()
var_test <-simp_returns_test %>% 
  filter(!is.na(ret), stock != "SPY") %>% 
  spread(stock, ret) %>% 
  select(-date,-x) %>% 
  as.matrix() %>% 
  var()

var_cov30 <- var_train %>% 
  as.data.frame() %>%  
  mutate(row = rownames(.)) %>% 
  filter(row != "SPY", row != "X.GSPC") %>% 
  select(-SPY, -X.GSPC,-row)# just the 30 stocks variance, not including the S&P 500


# Equal Allocation Portfolio ----------------------------------------------
x <- rep(1/30, 30) # shares of each stock
ret_equal <- r_bar_train %>% 
  filter(stock != "SPY",!str_detect(pattern = "^X\\.", stock)) %>% 
  bind_cols(tibble(wt = x)) 
ret_eq <- crossprod(ret_equal$r_bar, ret_equal$wt) %>% as.numeric()
var_por <- t(x) %*% as.matrix(var_cov30) %*% x 

# Min Risk Portfolio ------------------------------------------------------
rp_min <- (t(rep(1,30) %*% solve(var_cov30) %*% ret_equal$r_bar))/(t(rep(1,30)) %*% solve(var_cov30) %*% rep(1,30)) %>% 
  as.numeric()
var_min <- 1/(t(rep(1,30)) %*% solve(var_cov30) %*% rep(1,30)) %>% as.numeric()
sqrt(var_min) # volatility of min risk portfolio
sqrt(var_por) # volatiility of equal allocation portfolio
x_min <- (solve(var_cov30) %*% rep(1,30))/as.numeric((t(rep(1,30)) %*% solve(var_cov30) %*% rep(1,30)))
x_min <- tibble(x_min) %>% 
  mutate(stock = rownames(x_min))
# Estimate Betas using Blume, Vasicek, and Simple Index Model -------------
### Betas are an estimation of the stock's volatility relative to the market
### If it is greater than 1, then it is more volatile than the market
#### For the first period, we will only use the returns prior to 2015
beta_unadj <- rep(NA, 30) 
err_var_unadj <- rep(NA, 30)
var_beta_unadj <- rep(NA,30)
market_rets <- simp_returns_train %>% 
  bind_rows(simp_returns_test) %>% 
  filter(stock == "X.GSPC", !is.na(ret)) %>% 
  rename(ret_mkt = ret)
##### Unadjusted Betas - regress each stock's returns on the returns of the market
#### This is effectively the simple index model
for (i in 1:30){ #for each stock
  df <- simp_returns_train %>% 
    filter(stock == ret_equal$stock[i], !is.na(ret), year(date) <2015) %>% 
    left_join(market_rets, by = c("date", "x"))
  mod <- lm(data = df, ret ~ ret_mkt)
  beta_unadj[i] <- mod$coefficients[2]
  err_var_unadj[i] <- sum(resid(mod) ^2)/(length(resid(mod) - 2))
  var_beta_unadj[i] <- vcov(mod)[2,2]
}

beta_unadj_bar <- mean(beta_unadj)
sigma_beta_unadj <- var(beta_unadj)

### Construct var-cov matrix with the simple index model
sigma_m <- var(market_rets$ret_mkt) # market variance estimate 
vcov_sim <- matrix(NA, 30,30)
for (i in 1:30){
  for (j in 1:30){
    if (i == j){
      vcov_sim[i,j] <- beta_unadj[i]^2 * sigma_m + err_var_unadj[i]
    } else {
      vcov_sim[i,j] <- beta_unadj[i]*beta_unadj[j]*sigma_m # the covariance terms
    }
  }
}
colnames(vcov_sim) <- ret_equal$stock

press <- matrix(NA,30,30)
for (i in 1:30){
  for (j in 1:30){
    press[i,j] <- as.numeric((vcov_sim[i,j] - var_cov30[i,j])^2)
  }
}
colnames(press) <- ret_equal$stock
apply(X = press, MARGIN = 2, FUN = mean)

### Adjust the betas with Vasicek's technique
betas_vas <- (var_beta_unadj * beta_unadj_bar) / (sigma_beta_unadj + var_beta_unadj) +
  (sigma_beta_unadj * beta_unadj) / (sigma_beta_unadj + var_beta_unadj)

### Adjust the betas with Blume's technique (need 2 periods)

### Blume's Method
##### We already have the betas for the first period, calculate for the second period now
beta2 <- rep(NA,30)
var2 <- rep(NA, 30)
for (i in 1:30){ #for each stock
  df <- simp_returns_train %>% 
    filter(stock == ret_equal$stock[i], !is.na(ret), year(date) > 2015) %>% 
    left_join(market_rets, by = c("date", "x"))
  mod <- lm(data = df, ret ~ ret_mkt)
  beta2[i] <- mod$coefficients[2]
  var2[i] <- vcov(mod)[2,2]
}

blume <- lm(beta2 ~ beta_unadj)
beta_blume <- blume$coefficients[1] + blume$coefficients[2]*beta2

### Betas in Testing Period
betas_actual <- var_test[1,-31]/var_test[1,31]
  
### Calculate the PRESS for each method
press_unadj <- sum((beta2-betas_actual)^2) / 30 ### use the second period unadjusted estimates
press_blume <- sum((beta_blume-betas_actual)^2) / 30 #Blume
press_vas <- sum((betas_vas-betas_actual)^2) / 30 #Vasicek
print(c(press_unadj,press_blume,press_vas))
##### In this case, Vasicek's estimates do the best, but generally speaking there is not conclusive evidence
##### of Vasicek vs. Blume.

# Constant Correlation Model ----------------------------------------------
### we will use risk free rate of .0164
const_cor_tbl <- r_bar_train %>% 
  mutate(excess_sig = (r_bar - 0.0164)/sd) %>% 
  filter(stock != "SPY", stock != "X.GSPC") %>% 
  arrange(desc(excess_sig)) 
cor_df <- simp_returns_train %>% 
  filter(stock != "SPY", stock != "X.GSPC", !is.na(ret)) %>% 
  select(stock,ret)
p_bar <- 0
for (i in 1:30){
  for (j in 1:30){
    if (i == j){
      next
    } else{
      df <- cor_df %>% 
        filter(stock == const_cor_tbl$stock[i]) %>% 
        bind_cols(cor_df %>% 
                    filter(stock == const_cor_tbl$stock[j]))
      p_bar <- p_bar + cor(df$ret,df$ret1)
    }
  }
}
(p_bar <- p_bar/(29*30))

const_cor_tbl <- const_cor_tbl %>% 
  mutate(p_bar = p_bar,
         n = 1:n(),
         p_bar_ratio = p_bar/(1 - p_bar + n*p_bar),
         sum_exc_sig = cumsum(excess_sig),
         c = p_bar_ratio * sum_exc_sig) %>% 
  select(-n)

### With short sales allowed
opt_port_short <- const_cor_tbl %>% 
  mutate(c_star = const_cor_tbl$c[30],
         z_stk = (1/((1 - p_bar)*sd)) * (excess_sig - c_star),
         x_stk = z_stk/sum(z_stk)) %>% 
  select(stock, x_stk)

### With no short sales
opt_port_long <- const_cor_tbl %>% 
  mutate(c_star_flg = ifelse(excess_sig > c,1,0),
         cstar = ifelse(c_star_flg == 1 & lead(c_star_flg == 0), c, NA),
         cstar = max(cstar,na.rm = T),
         z_stk = (1/((1 - p_bar)*sd)) * (excess_sig - cstar),
         x_stk = z_stk/sum(z_stk)) %>% 
  select(stock, x_stk)

# Simple Index Model Optimal Portfolio ------------------------------------
#### Fix the Betas
sim <- r_bar_train %>% 
  filter(stock != "SPY", stock != "X.GSPC") %>% 
  mutate(beta_sim = NA,
         var_sim = NA,
         alpha_sim = NA,
         errors_sim = NA,
         sigma_m = var(simp_returns_train[simp_returns_train$stock == "SPY","ret"], na.rm = T))
  
for (i in 1:30){ #for each stock
  df <- simp_returns_train %>% 
    filter(stock == sim$stock[i], !is.na(ret)) %>% 
    left_join(market_rets, by = c("date", "x"))
  mod <- lm(data = df, ret ~ ret_mkt)
  sim$beta_sim[i] <- mod$coefficients[2]
  sim$var_sim[i] <- vcov(mod)[2,2]
  sim$alpha_sim[i] <- mod$coefficients[1]
  sim$errors_sim[i] <- var(mod$residuals)
}

## we will use Rf = .0164
sim <- sim %>% 
  mutate(excess_beta = (r_bar - 0.0164)/beta_sim,
         excess_sig = ((r_bar - 0.0164) * beta_sim)/errors_sim,
         beta_error_ratio = beta_sim^2/errors_sim) %>% 
  arrange(desc(excess_beta)) %>% 
  mutate(ratio1 = cumsum(excess_sig),
         ratio2 = cumsum(beta_error_ratio),
         c = (sigma_m * ratio1)/(1 + sigma_m * ratio2))

### With short sales allowed
opt_port_short2 <- sim %>% 
  mutate(flg = ifelse(c < excess_beta,1 ,0),
         c_star = ifelse(flg == 1 & lead(flg) == 0, c, NA),
         c_star = max(c_star, na.rm = T)) %>% 
  select(-flg) %>% 
  mutate(z_stk = beta_error_ratio * (excess_beta - c_star),
         x_stk = z_stk/sum(z_stk)) %>% 
  select(stock, x_stk)

### With no short sales
opt_port_long2 <- sim %>% 
  mutate(flg = ifelse(c < excess_beta,1 ,0),
         c_star = ifelse(flg == 1 & lead(flg) == 0, c, NA),
         c_star = max(c_star, na.rm = T)) %>% 
  select(-flg) %>% 
  filter(excess_beta >= c) %>% 
  mutate(z_stk = beta_error_ratio * (excess_beta - c_star),
         x_stk = z_stk/sum(z_stk)) %>% 
  select(stock, x_stk) %>% 
  right_join(sim %>% select(stock)) %>% 
  mutate(x_stk = coalesce(x_stk, 0))


# Assess the Different Portfolios -----------------------------------------
### The shares of each stock
shares <- x_min %>% 
  select(stock, everything()) %>% 
  rename(min_risk = x_min) %>% 
  mutate(eq_port =rep(1/30,30)) %>% 
  left_join(opt_port_short %>% rename(cc_ss = x_stk)) %>% 
  left_join(opt_port_long %>% rename(cc_nss = x_stk)) %>% 
  left_join(opt_port_short2 %>% rename(sim_ss = x_stk)) %>% 
  left_join(opt_port_long2 %>% rename(sim_nss = x_stk))
### Now put together the dataset of portfolio returns
portfolio <- simp_returns_train %>% 
  filter(!is.na(ret), stock != "SPY", stock != "X.GSPC") %>% 
  ungroup() %>% 
  distinct(date, stock, ret) %>% 
  mutate(eq_port = NA,
         min_risk = NA,
         cc_ss = NA,
         cc_nss=NA,
         sim_ss = NA,
         sim_nss = NA) %>% 
  arrange(date) %>% 
  left_join(shares, by = "stock", suffix = c("", "_shr")) %>% 
  group_by(date) %>% 
  mutate(eq_port = sum(ret * eq_port_shr),
         min_risk = sum(ret * min_risk_shr),
         cc_ss = sum(ret * cc_ss_shr),
         cc_nss = sum(ret * cc_nss_shr),
         sim_ss = sum(ret * sim_ss_shr),
         sim_nss = sum(ret * sim_nss_shr)) %>% 
  distinct(date, eq_port, min_risk, cc_ss, cc_nss, sim_ss, sim_nss) %>% 
  bind_rows(tibble(date = make_date(2010,1,1))) %>% 
  arrange(date) %>% 
  gather(key = portfolio,value = ret, -date) %>% 
  mutate(portfolio = case_when(portfolio == "eq_port" ~ "Equal Weight",
                               portfolio == "min_risk" ~ "Minimum Risk",
                               portfolio == "cc_ss" ~ "Constant Correlation (SS)",
                               portfolio == "cc_nss" ~ "Constant Correlation (NSS)",
                               portfolio == "sim_ss" ~ "Simple Index Model (SS)",
                               T ~ "Simple Index Model (NSS)")) %>% 
  ungroup() %>% 
  bind_rows(market_rets %>% 
              mutate(portfolio = "Market",ret = ret_mkt) %>% 
              ungroup() %>% 
              bind_rows(tibble(portfolio = "Market", date = make_date(2010,1,1)))%>% 
              arrange(date) %>% 
              select(portfolio, ret, date))

cum_value <- portfolio %>% 
  mutate(growth_fac = ifelse(is.na(ret), 1000,1+ret)) %>% 
  group_by(portfolio) %>% 
  mutate(cum_value = cumprod(growth_fac))

### Time Plots
plt <- ggplot(data = portfolio %>% filter(!is.na(ret)), mapping = aes(x = date,y = ret, color = portfolio)) +
  geom_line() + 
  xlab("Date") +
  ylab("Monthly Return")+
  labs(color="Investment") +
  theme(legend.position = "bottom")
plt

plt2 <- ggplot(data = cum_value, mapping = aes(x = date,y = cum_value, color = portfolio)) +
  geom_line() + 
  xlab("Date") +
  ylab("Cumulative Value of $1000")+
  labs(color="Investment") +
  theme(legend.position = "bottom")
plt2


### Sharpe Ratio
sharpe <- portfolio %>% 
  group_by(portfolio) %>% 
  summarise(sharpe = (mean(ret) - 0.0164)/sd(ret)) %>% 
  arrange(desc(sharpe))

### Differential Excess Return
diff_excess <- portfolio %>% 
  filter(portfolio != "Market") %>% 
  left_join(market_rets %>% select(date, ret_mkt)) %>% 
  group_by(portfolio) %>% 
  summarise(ra_prime  = (mean(ret_mkt) - 0.0164)/(sd(ret_mkt) * sd(ret)),
            r_bar = mean(ret),
            r_bar_mkt = mean(ret_mkt)) %>% 
  mutate(dif = r_bar - ra_prime) %>% 
  arrange(desc(dif))

### Treynor Measure
treynor <- portfolio %>% 
  filter(portfolio != "Market") %>% 
  left_join(market_rets %>% select(date, ret_mkt)) %>% 
  group_by(portfolio) %>% 
  do(model = lm(ret ~ ret_mkt, data = .)) %>% 
  tidy(model) %>% 
  filter(term != "(Intercept)") %>% 
  select(portfolio, beta = estimate) %>% 
  left_join(diff_excess %>% ungroup()) %>% 
  mutate(treynor = (r_bar - 0.0164)/beta) %>% 
  arrange(desc(treynor))

### Jensen's Measure
jensen <- treynor %>% 
  mutate(ra_prime = 0.0164 - ((r_bar_mkt - 0.0164)*beta),
         jensen = r_bar - ra_prime) %>% 
  select(portfolio, jensen) %>% 
  arrange(desc(jensen))

# Black Scholes -----------------------------------------------------------
black_scholes <- function(price_t,exc_price,risk_free,sigma_stk,time_exp_day){
  t <- time_exp_day/252
  d1 <- (log(price_t/exc_price) + (risk_free+.5*sigma_stk^2)*t) / (sigma_stk*sqrt(t))
  d2 <- d1 - sigma_stk*sqrt(t)
  price_call <- price_t*pnorm(d1) - exc_price*exp(-risk_free*t)*pnorm(d2)
  black_scholes_out <- tibble(price_t, exc_price,t, risk_free, sigma_stk,price_call, d1,d2)
  return(black_scholes_out)
}
black_scholes(price_t = 30, exc_price = 29, time_exp_day = 40,risk_free = 0.05,sigma_stk = .3)

# Implied Volatility Using Black Scholes ----------------------------------
### Implied Volatility
#Inputs:
s0 <- 21 # stock price at time t
E <- 20 #excercise price
r <- 0.1 # risk free rate
t <- 0.25 # time to expiration in years
c <- 1.875 # price of European call
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


















