rm(list=ls())

require('stats4')
require('bbmle')
require('maxLik')
require('MASS')
source('goose_predict_gui.R')

# To ensure functions from goose_predict_gui.R work as in the Shiny app:
input <- list(input_name=data.frame(
  datapath=as.vector('~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/Islay_Stirling_2017.xls')),
  sims_in=5, yrs_in=1, maxHB_in=2000, target_in=32000)
input$input_name$datapath <- as.vector(input$input_name$datapath)
iterations <- input$sims_in
years <- input$yrs_in
proj_yrs <- years
manage_target <- input$target_in
max_HB <- input$maxHB_in
data_file <- as.vector(input$input_name$datapath)
obs_error = 1438.614
plot = TRUE
use_est    <- 0;
goose_data <- goose_clean_data(file = data_file);
last_year  <- goose_data[dim(goose_data)[1], 1];
data <- goose_data

# The following mimics the get_goose_paras() function in goose_predict_gui.R

init_params    <- c(0.1,6,0,0,0,0)    
# Copy the init paramters as a named list so it can work with mle2()
init_params2 <- as.list(init_params)          
names(init_params2) <- c('b1','b2','b3','b4','b5','b6')


### OPTIMISATION ALTERNATIVES BELOW:

get_parameters <- optim(par = init_params, fn = goose_growth, data = data,
                                  hessian = TRUE);

params <- get_parameters
# This is how I *think* the SE's should be calculated from the Hessian:
params_se <- SEfromHessian(params$hessian)
# This puts the estimates and SE's next to each other
round(data.frame(b=params$par, se=params_se),3)
# Note that the SE's here are much larger than those in Table 3.













### To visualise the effects on predictions, just plot predictions and observed numbers
### for past data:

Npred <- goose_pred(para = params$par, data = data)
Npred <- floor(Npred)

year_start = 1987
yrs <- year_start:(year_start + length(data$y) - 1)

ylim = c(10000, 60000)

par(mar = c(5, 5, 1, 1));
plot(x =  yrs, y = data$y, pch = 1, ylim = ylim, cex.lab = 1.5,
     xlab="Year", ylab="Population size")         # Observed time series
points(x = yrs, y = Npred, pch = 19, col = "red") # Predict time series
oend <- length(data$y)
points(x = yrs[3:oend], y = data$y[2:(oend - 1)], pch = 19,
       col = "blue")

par_samp <- mvrnorm(1000, mu=coef(summary(params_mle))[,1], Sigma=solve(attr(params_mle,'details')$hessian))

par_samp_pred <-  apply(par_samp, 1, function(x) rpois(nrow(data), goose_pred(para=x, data=data)))

Npred_mn <- floor(apply(par_samp_pred, 1, function(x) median(x, na.rm=T)))
Npred_lo <- floor(apply(par_samp_pred, 1, function(x) quantile(x, prob=0.025, na.rm=T)))
Npred_hi <- floor(apply(par_samp_pred, 1, function(x) quantile(x, prob=0.975, na.rm=T)))

lines(yrs, Npred)
lines(yrs, Npred_lo, col='red')
lines(yrs, Npred_hi, col='red')


### This is attempting to produce the same prediction range as above but clearly off the scale of the graph!

par_samp1 <- mvrnorm(1000, mu=params$par, Sigma=-solve(params$hessian))
par_samp_pred1 <-  apply(par_samp1, 1, function(x) rpois(nrow(data), goose_pred(para=x, data=data)))
Npred_mn1 <- floor(apply(par_samp_pred1, 1, function(x) median(x, na.rm=T)))
Npred_lo1 <- floor(apply(par_samp_pred1, 1, function(x) quantile(x, prob=0.025, na.rm=T)))
Npred_hi1 <- floor(apply(par_samp_pred1, 1, function(x) quantile(x, prob=0.975, na.rm=T)))
lines(yrs, Npred_lo1, col='red', lty='dashed')
lines(yrs, Npred_hi1, col='red', lty='dashed')


### Copy pars and SE's from Tab 3 in report
par(mar = c(5, 5, 1, 1));
plot(x =  yrs, y = data$y, pch = 1, ylim = ylim, cex.lab = 1.5,
     xlab="Year", ylab="Population size")         # Observed time series
points(x = yrs, y = Npred, pch = 19, col = "red") # Predict time series
oend <- length(data$y)
points(x = yrs[3:oend], y = data$y[2:(oend - 1)], pch = 19,
       col = "blue")
t3pars <- c(0.273, 5.595, -0.006, 0.076, 0.007, 0.062)
t3se <- c(0.051, 0.876, 0.001, 0.019, 0.001, 0.010)
t3 <- data.frame(b=t3pars, se=t3se)
par_samp3 <- apply(t3, 1, function(x) rnorm(10000, x[1], x[2]) )
#par_samp_pred3 <-  apply(par_samp3, 1, function(x) rpois(nrow(data), goose_pred(para=x, data=data)))
par_samp_pred3 <-  apply(par_samp3, 1, function(x) goose_pred(para=x, data=data))
Npred_mn3 <- floor(apply(par_samp_pred3, 1, function(x) median(x, na.rm=T)))
Npred_lo3 <- floor(apply(par_samp_pred3, 1, function(x) quantile(x, prob=0.025, na.rm=T)))
Npred_hi3 <- floor(apply(par_samp_pred3, 1, function(x) quantile(x, prob=0.975, na.rm=T)))
lines(yrs, Npred)
lines(yrs, Npred_lo3, col='red')
lines(yrs, Npred_hi3, col='red')

