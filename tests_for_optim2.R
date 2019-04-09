library(optimParallel)

source('ggmse_default_test_pars.R')
goose_data <- goose_clean_data(file = data_file)

last_year  <- goose_data[dim(goose_data)[1], 1]

gmse_res   <- gmse_apply(res_mod = goose_gmse_popmod,
                         obs_mod = goose_gmse_obsmod,
                         man_mod = goose_gmse_manmod,
                         use_mod = goose_gmse_usrmod,
                         dat = goose_data, obs_error = obs_error,
                         manage_target = manage_target, max_HB = max_HB,
                         use_est = 0, stakeholders = 1,
                         get_res = "full")

dat <- goose_data

init_params <- NULL

if( is.null(init_params) ) {
  init_params    <- c(0.1,6,0,0,0,0);                
}

cl <- makeCluster(detectCores())
setDefaultCluster(cl=cl)

clusterExport(cl, c('goose_data','goose_pred'))

system.time({
  get_parameters <- optim(par = init_params, fn = goose_growth, dat = dat, 
                          hessian = TRUE, method = 'BFGS');  
})
get_parameters$par

system.time({
  get_parameters <- optimParallel(par = init_params, fn = goose_growth, dat = dat, 
                          hessian = TRUE, method = 'BFGS');  
})


