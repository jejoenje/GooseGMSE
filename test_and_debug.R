rm(list=ls())

source('goose_predict_gui.R')

input <- list(input_name=data.frame(
  datapath=as.vector('~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/Islay_Stirling_2015.xls')),
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
#                                      max_HB = max_HB, plot = FALSE,
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

  ###
  ### FOR gmse_apply(res_mod = ... below)
  
    # goose_gmse_popmod()

    #N_pred <- goose_plot_pred(data = goose_data, plot = FALSE);

      # goose_plot_pred()

        # params <- get_goose_paras(data = data);

        data <- goose_data

          # get_goose_paras()

          #if( is.null(init_params) == TRUE ){
          init_params    <- c(0.1,6,0,0,0,0)               
          

          contr_paras    <- list(trace = 1, fnscale = -1, maxit = 1000, factr = 1e-8,
                                 pgtol = 0)
          
          get_parameters <- optim(par = init_params, fn = goose_growth2, data = data,
                                  hessian = TRUE)
          
          ### I THINK THIS GIVES THE SE'S:
          ses <- sqrt(abs(diag(-solve(get_parameters$hessian))))
          
          ### This is potentially interesting:
          # https://www.rdocumentation.org/packages/HelpersMG/versions/3.5/topics/SEfromHessian
          
        # RETURN get_goose_paras():
          params <- get_parameters

      # RETURNED TO goose_plot_pred() 
      year_start = 1987
      ylim = c(10000, 60000)
      
      Npred <- goose_pred(para = params$par, data = data)
      Npred <- floor(Npred)
      
      par_samp <- mvrnorm(1000, params$par, solve(params$hessian))
      
      par_samp_pred <-  apply(par_samp, 1, function(x) rpois(nrow(data), goose_pred(para=x, data=data)))
      
      Npred_mn <- floor(apply(par_samp_pred, 1, function(x) median(x, na.rm=T)))
      Npred_lo <- floor(apply(par_samp_pred, 1, function(x) quantile(x, prob=0.025, na.rm=T)))
      Npred_hi <- floor(apply(par_samp_pred, 1, function(x) quantile(x, prob=0.975, na.rm=T)))
      
      yrs    <- year_start:(year_start + length(data$y) - 1)
      
      par(mar = c(5, 5, 1, 1));
      plot(x =  yrs, y = data$y, pch = 1, ylim = ylim, cex.lab = 1.5,
           xlab="Year", ylab="Population size")         # Observed time series
      points(x = yrs, y = Npred, pch = 19, col = "red") # Predict time series
      oend <- length(data$y)
      points(x = yrs[3:oend], y = data$y[2:(oend - 1)], pch = 19,
             col = "blue")

      
      
      
      # RETURN goose_plot_pred()
      N_pred <- Npred
    # goose_gmse_popmod()
    
    N_last <- length(N_pred)
    
    # RETURN goose_gmse_popmod()   --- INTO gmse()
    New_N  <- as.numeric(N_pred[N_last])
    
  # RETURN TO goose_gmse()  
  gmse_res   <- gmse_apply(res_mod = goose_gmse_popmod, 
                             obs_mod = goose_gmse_obsmod,
                             man_mod = goose_gmse_manmod,
                             use_mod = goose_gmse_usrmod,
                             goose_data = goose_data, obs_error = obs_error,
                             manage_target = manage_target, max_HB = max_HB,
                             use_est = use_est, stakeholders = 1, 
                             get_res = "full");
    
  goose_data <- sim_goose_data(gmse_results = gmse_res$basic,
                               goose_data = goose_data);
  
  # -- Simulate --------------------------------------------------------------
  # while(years > 0){    # Count down number of years and for each add goose projections
    gmse_res_new   <- gmse_apply(res_mod = goose_gmse_popmod, 
                                 obs_mod = goose_gmse_obsmod,
                                 man_mod = goose_gmse_manmod,
                                 use_mod = goose_gmse_usrmod,
                                 goose_data = goose_data,
                                 manage_target = manage_target, use_est = use_est,
                                 max_HB = max_HB, obs_error = obs_error,
                                 stakeholders = 1, get_res = "full");
    
    ### I THINK THE FOLLOWING IS AN ISSUE RE. CATCHING POPULATION EXTINCTION:
    ### NEEDS CHECKING
    if(as.numeric(gmse_res_new$basic[1]) == 1){
      break;      
    }
    ###
    
    gmse_res   <- gmse_res_new;

    goose_data <- sim_goose_data(gmse_results = gmse_res$basic, 
                                 goose_data = goose_data);
    years <- years - 1;
  }   # Ignores the last "simulated" year as no numbers exist for it yet.
  
  goose_data <- goose_data[-(nrow(goose_data)),];
  
mvrnorm(1000, params$par, solve(params$hessian))
  
  