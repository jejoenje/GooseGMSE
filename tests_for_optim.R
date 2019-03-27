rm(list=ls())

require('stats4')
require('bbmle')
require('maxLik')

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
    
       
# 1. This is the optim() version we were using before; it minmises SS using goose_growth()

contr_paras <- list(trace = 1, fnscale = -1, maxit = 1000, factr = 1e-8,
                  pgtol = 0);
get_parameters <- optim(par = init_params, fn = goose_growth, data = data,
                                  method = "BFGS", control = contr_paras,
                                  hessian = TRUE);
params <- get_parameters
# This is how I *think* the SE's should be calculated from the Hessian:
params_se <- sqrt(abs(diag(-solve(params$hessian))))
# This puts the estimates and SE's next to each other
round(data.frame(b=params$par, se=params_se),3)
# Note that the SE's here are much larger than those in Table 3.

          
# 2. This is optimisation using mle2() from the bbmle package:

# This uses goose_growth3() which is the same function as goose_growth() just with a parameterisation suitable
#  for mle():
params_mle <- mle2(goose_growth3, start=init_params2, optimizer='nlminb')
round(coef(summary(params_mle))[,1:2],3)
# Note that now the SE's are far smaller, both compared to the SS minimisation as well as the values in Table 3.


# 3. This is optimisation using maxLik():

# Note that this uses goose_growth2a() which again is the same function as goose_growth3() but returns the POSITIVE
#  log likelihood, because maxLik MAXIMISES by default.

params_maxLik <- maxLik(goose_growth2a, start=init_params, data=goose_data, method='NR')
round(coef(summary(params_maxLik))[,1:2],3)


### Comparing them all three again:

round(data.frame(b=params$par, se=params_se),3)
round(coef(summary(params_mle))[,1:2],3)
round(coef(summary(params_maxLik))[,1:2],3)

### So it appears from the above that the point estimates are pretty much the same for all three options.
### The SE's are different for the first (SS) from the other two (LL).
###
### A couple of notes
### - I'm not sure of the log-lik optimisations used above yet. Any thoughts let me know.
### - I don't really understand how the scaling factor of 1000 affects the SE's to be entirely honest
