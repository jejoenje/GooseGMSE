### This just sets some default paramters for a single iteration of a 10-year simulation,
###  with maxHB = 2500 and population target of 29000.
source('ggmse_default_test_pars.R')

### This replicates the first few steps of the gmse_goose() function - i.e. it
### only loads the data file and sets some basic parameters

proj_yrs   <- years

goose_data <- goose_clean_data(file = data_file)

last_year  <- goose_data[dim(goose_data)[1], 1]

### The get_goose_paras is called by the goose population model used in the call
### to GMSE inside gmse_goose(). This gives a list with an element for the point
### estimates for the parameters and the Hessian matrix, from which we can
### calculate the parameter SE's.
params <- get_goose_paras(goose_data)

### So the estimated parameters and accompanying SE's are as follows, using the
### convenience function SEfromHessian to calculate the SE's.
round(params$par,3)
round(SEfromHessian(params$hessian),3)

### The parameter estimates are exactly as per the revised report table, but
### some of the SE's are off by quite a margin; particularly those for the
### latter three (parameters b,c, and d)


