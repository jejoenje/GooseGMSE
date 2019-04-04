  rm(list=ls())
  
  source('goose_predict_gui.R')
  
  input <- list(input_name=data.frame(
    datapath=as.vector('~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/Islay_Stirling_2015.xls')),
    sims_in=5, yrs_in=15, maxHB_in=2000, target_in=32000)
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
  lastpars <- NULL
  Npred_lo <- NULL
  Npred_hi <- NULL
  
  proj_yrs   <- years
  goose_data <- goose_clean_data(file = data_file)
  last_year  <- goose_data[dim(goose_data)[1], 1]
  use_est    <- 0
  
  if(use_est == "cautious"){
    use_est <- -1;
  }
  if(use_est == "aggressive"){
    use_est <- 1;
  }
  
  target <- manage_target
  
  Npred_lo <- NULL
  Npred_hi <- NULL
  Npred_mn <- NULL
  
  goose_data$Npred_mn <- NA
  goose_data$Npred_lo <- NA
  goose_data$Npred_hi <- NA
  
  gmse_res   <- gmse_apply(res_mod = goose_gmse_popmod, 
                           obs_mod = goose_gmse_obsmod,
                           man_mod = goose_gmse_manmod,
                           use_mod = goose_gmse_usrmod,
                           dat = goose_data, obs_error = obs_error,
                           manage_target = target, max_HB = max_HB,
                           use_est = use_est, stakeholders = 1, 
                           get_res = "full");

  goose_data <- sim_goose_data(gmse_results = gmse_res$basic,
                               goose_data = goose_data);
  goose_data$Npred_mn[nrow(goose_data)] <- Npred_mn-gmse_res$basic$user_results
  goose_data$Npred_lo[nrow(goose_data)] <- Npred_lo-gmse_res$basic$user_results
  goose_data$Npred_hi[nrow(goose_data)] <- Npred_hi-gmse_res$basic$user_results
  
  
  # Start 'while' loop
  while(years > 1){
    gmse_res_new   <- gmse_apply(res_mod = goose_gmse_popmod, 
                                 obs_mod = goose_gmse_obsmod,
                                 man_mod = goose_gmse_manmod,
                                 use_mod = goose_gmse_usrmod,
                                 dat = goose_data,
                                 manage_target = target, use_est = use_est,
                                 max_HB = max_HB, obs_error = obs_error,
                                 stakeholders = 1, get_res = "full");

    gmse_res   <- gmse_res_new;
    
    goose_data <- sim_goose_data(gmse_results = gmse_res$basic, 
                                 goose_data = goose_data);
    
    goose_data$Npred_mn[nrow(goose_data)] <- Npred_mn-gmse_res$basic$user_results
    goose_data$Npred_lo[nrow(goose_data)] <- Npred_lo-gmse_res$basic$user_results
    goose_data$Npred_hi[nrow(goose_data)] <- Npred_hi-gmse_res$basic$user_results
    
    years <- years - 1;
  }
  
  

  
  plot(goose_data$Year, goose_data$y, lwd=2, ylim=c(10000,60000), type='l')
  lines(goose_data$Year, goose_data$Npred_lo, col='red')
  lines(goose_data$Year, goose_data$Npred_hi, col='red')
  
  
