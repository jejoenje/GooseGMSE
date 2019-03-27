rm(list=ls())

source('goose_predict_gui.R')

input <- list(input_name=data.frame(
  datapath=as.vector('/home/jm241/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/Islay_Stirling_2017.xls')),
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

obs_error = 1438.614
goose_multidata <- NULL

# for(i in 1:iterations){
#   
#   goose_multidata[[i]] <- gmse_goose(data_file = data_file,
#                                      obs_error = obs_error,
#                                      years = proj_yrs,
#                                      manage_target = manage_target,
#                                      max_HB = max_HB, plot = FALSEgoose_growth,
#                                      use_est = use_est);
#   print(paste("Simulating ---------------------------------------> ",i));
# }

# gmse_goose()

proj_yrs   <- years;
goose_data <- goose_clean_data(file = data_file);
last_year  <- goose_data[dim(goose_data)[1], 1];
use_est    <- 0;
if(use_est == "cautious"){
  use_est <- -1;
}
if(use_est == "aggressive"){
  use_est <- 1;
}
# assign("goose_data", goose_data, envir = globalenv() );
# assign("target", manage_target, envir = globalenv() );
# assign("max_HB", max_HB, envir = globalenv() );
# assign("obs_error", obs_error, envir = globalenv() );
# assign("use_est", use_est, envir = globalenv() );
# gmse_res   <- gmse_apply(res_mod = goose_gmse_popmod, 
#                          obs_mod = goose_gmse_obsmod,
#                          man_mod = goose_gmse_manmod,
#                          use_mod = goose_gmse_usrmod,
#                          goose_data = goose_data, obs_error = obs_error,
#                          manage_target = target, max_HB = max_HB,
#                          use_est = use_est, stakeholders = 1, 
#                          get_res = "full");

# goose_gmse_popmod()

#N_pred <- goose_plot_pred(data = goose_data, plot = FALSE);

# goose_plot_pred()

# params <- get_goose_paras(data = data);

data <- goose_data

# get_goose_paras()

#if( is.null(init_params) == TRUE ){
  init_params    <- c(0.1,6,0,0,0,0)               
#

contr_paras    <- list(trace = 1, fnscale = -1, maxit = 1000, factr = 1e-8,
                       pgtol = 0)

get_parameters <- optim(par = init_params, fn = goose_growth2, data = data,
                        hessian = TRUE)

### I THINK THIS GIVES THE SE'S:
sqrt(abs(diag(-solve(get_parameters$hessian))))

### This is potentially interesting:
# https://www.rdocumentation.org/packages/HelpersMG/versions/3.5/topics/SEfromHessian


# goose_growth()

data_rows <- dim(data)[1]

para <- get_parameters$par

N_pred <- goose_pred(para = para, data = data)

# goose_pred



DEV    <- N_pred[3:data_rows] - data$y[3:data_rows]
sq_Dev <- DEV * DEV
pr_sum <- sum( sq_Dev / N_pred[3:data_rows] )
SS_tot <- (1 / pr_sum) * 1000   # Total sum of squares, scaled for convenience