library(parallel)
library(foreach)

source('ggmse_default_test_pars.R')
goose_data <- goose_clean_data(file = data_file)

last_year  <- goose_data[dim(goose_data)[1], 1]

dat <- goose_data

init_params <- NULL

if( is.null(init_params) ) {
  init_params    <- c(0.1,6,0,0,0,0);                
}

system.time({
  get_parameters <- optim(par = init_params, fn = goose_growth, dat = dat, 
                          hessian = TRUE);  
})
get_parameters$par

goose_multidata <- NULL

cl <- parallel::makeForkCluster(8)
doParallel::registerDoParallel(cl)

timing <- system.time({
  foreach(i=1:iterations, .combine='c') %dopar% {
    
    years <- proj_yrs
    
    print(paste('Iteration', i, 'year', years))
    
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
    
    goose_data <- sim_goose_data(gmse_results = gmse_res$basic,
                                 goose_data = goose_data)
    
    goose_data$Npred_mn[nrow(goose_data)] <- Npred_mn
    goose_data$Npred_lo[nrow(goose_data)] <- Npred_lo
    goose_data$Npred_hi[nrow(goose_data)] <- Npred_hi
    
    # Start 'while' loop
    
    while(years > 1){
      
      print(paste('Iteration', i, 'year', years-1))
      
      if(goose_data$y[nrow(goose_data)]<1) {
        print('EXTINCTION')
        extinct <- TRUE
        goose_data$y[nrow(goose_data)] <- 0
        goose_data$Count[nrow(goose_data)] <- 0
        goose_data$Npred_mn[nrow(goose_data)] <- 0
        goose_data$Npred_lo[nrow(goose_data)] <- 0
        goose_data$Npred_hi[nrow(goose_data)] <- 0
      }
      
      if(extinct==FALSE) {
        
        goose_data$y[goose_data$y<0] <- 0
        goose_data$Npred_mn[goose_data$Npred_mn<0] <- 0
        #goose_data$Npred_lo[goose_data$Npred_lo<0] <- 0
        #goose_data$Npred_hi[goose_data$Npred_hi<0] <- 0
        
        gmse_res_new   <- gmse_apply(res_mod = goose_gmse_popmod,
                                     obs_mod = goose_gmse_obsmod,
                                     man_mod = goose_gmse_manmod,
                                     use_mod = goose_gmse_usrmod,
                                     dat = goose_data,
                                     manage_target = manage_target, use_est = 0 ,
                                     max_HB = max_HB, obs_error = obs_error,
                                     stakeholders = 1, get_res = "full");
        
        gmse_res   <- gmse_res_new;
        
        goose_data <- sim_goose_data(gmse_results = gmse_res$basic,
                                     goose_data = goose_data);
        
        goose_data$Npred_mn[nrow(goose_data)] <- Npred_mn
        goose_data$Npred_lo[nrow(goose_data)] <- Npred_lo
        goose_data$Npred_hi[nrow(goose_data)] <- Npred_hi
        
      } else {
        cur_yr <- goose_data$Year[length(goose_data$Year)]
        rem_yrs <- (last_year+proj_yrs)-cur_yr
        
        adds <- data.frame(Year=(cur_yr+1):(cur_yr+rem_yrs),
                           November=NA,
                           December=NA,
                           January=NA,
                           February=NA,
                           March=NA,
                           Count=0,
                           IcelandCull=NA,
                           IslayCull=NA,
                           GreenlandCull=NA,
                           AIG=NA,
                           IslayTemp=NA,
                           AugRain=NA,
                           AugTemp=NA,
                           y=0,
                           AIG.sc=NA,
                           HB=NA,
                           gmse_pop=NA,
                           Npred_mn=0,
                           Npred_lo=NA,
                           Npred_hi=NA
        )
        goose_data <- rbind(goose_data, adds)
        years <- 1
      }
      
      years <- years - 1
    }
    goose_multidata[[i]] <- goose_data
    rm(goose_data)
  }
  
})   ### Started 10:27

