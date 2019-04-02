rm(list=ls())

source('goose_predict_gui.R')

input <- list(input_name=data.frame(
  datapath=as.vector('~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/Islay_Stirling_2015.xls')),
  sims_in=10, yrs_in=10, maxHB_in=4000, target_in=10000)
input$input_name$datapath <- as.vector(input$input_name$datapath)
iterations <- input$sims_in
years <- input$yrs_in
proj_yrs <- years
manage_target <- input$target_in
max_HB <- input$maxHB_in
data_file <- as.vector(input$input_name$datapath)
obs_error = 1438.614
plot = TRUE

extinct = FALSE

goose_multidata <- NULL

system.time({
  for(i in 1:iterations){
    
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
    
    goose_data$Npred_mn[nrow(goose_data)] <- Npred_mn-gmse_res$basic$user_results
    goose_data$Npred_lo[nrow(goose_data)] <- Npred_lo-gmse_res$basic$user_results
    goose_data$Npred_hi[nrow(goose_data)] <- Npred_hi-gmse_res$basic$user_results
    
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
        
        goose_data$Npred_mn[nrow(goose_data)] <- Npred_mn-gmse_res$basic$user_results
        goose_data$Npred_lo[nrow(goose_data)] <- Npred_lo-gmse_res$basic$user_results
        goose_data$Npred_hi[nrow(goose_data)] <- Npred_hi-gmse_res$basic$user_results
      } else {
        cur_yr <- goose_data$Year[length(goose_data$Year)]
        rem_yrs <- (last_year+proj_yrs)-cur_yr
        
        adds <- data.frame(Year=(cur_yr+1):(cur_yr+rem_yrs),
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
  
})



plot(goose_multidata[[1]]$Year, goose_multidata[[1]]$y, type='l')
for(i in 2:length(goose_multidata)) {
  lines(goose_multidata[[i]]$Year, goose_multidata[[i]]$y)
}
for(i in 1:length(goose_multidata)) {
  lines(goose_multidata[[i]]$Year, goose_multidata[[i]]$Npred_lo, col='darkgrey')
  lines(goose_multidata[[i]]$Year, goose_multidata[[i]]$Npred_hi, col='darkgrey')
}
