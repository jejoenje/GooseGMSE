rm(list=ls())

source('goose_predict_gui.R')

library(scales)

input <- list(input_name=data.frame(
  datapath=as.vector('~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/example_data_UPDATED_April2019.csv')),
  sims_in=1000, yrs_in=10, maxHB_in=2500, target_in=29000)
input$input_name$datapath <- as.vector(input$input_name$datapath)
iterations <- input$sims_in
years <- input$yrs_in
proj_yrs <- years
manage_target <- input$target_in
max_HB <- input$maxHB_in
data_file <- as.vector(input$input_name$datapath)
obs_error = 1438.614
plot = TRUE
past = FALSE

resamp = TRUE

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

save(goose_multidata, file='Example_sims_1000.Rdata')



Fig6 <- function() {
  past_years <- goose_multidata[[1]]$Year<=last_year
  con_years <- goose_multidata[[1]]$Year==2015 | goose_multidata[[1]]$Year==2016
  plot(goose_multidata[[1]]$Year, goose_multidata[[1]]$y, type='n', ylim=c(0,50000), 
       xlab='Year', ylab='Estimated population size')
  rect(last_year+1, -10000, last_year+proj_yrs+5, 70000, border=NA, col='grey')
  box()
  for(i in 1:length(goose_multidata)) {
    lines(goose_multidata[[i]]$Year[!past_years], goose_multidata[[i]]$Npred_mn[!past_years], col = alpha('black', 0.25))
  }
  #lines(goose_multidata[[1]]$Year[con_years], goose_multidata[[1]]$Count[con_years], lty='dashed')
  lines(goose_multidata[[1]]$Year[past_years], goose_multidata[[1]]$Count[past_years], type='b', pch=21, col='black', bg='black')
  
  out_lo <- as.data.frame(NULL)
  out_hi <- as.data.frame(NULL)
  for(i in 1:length(goose_multidata)) {
    out_lo <- rbind(out_lo, goose_multidata[[i]]$Npred_lo)
    out_hi <- rbind(out_hi, goose_multidata[[i]]$Npred_hi)
  }
  out_lo_mn <- as.vector(apply(out_lo, 2, mean))
  out_hi_mn <- as.vector(apply(out_hi, 2, mean))
  out_lo <- as.vector(apply(out_lo, 2, min))
  out_hi <- as.vector(apply(out_hi, 2, max))
  
  #lines(goose_multidata[[1]]$Year, out_lo_mn, lty='dotted', col='darkgrey')
  #lines(goose_multidata[[1]]$Year, out_hi_mn, lty='dotted', col='darkgrey')
  lines(goose_multidata[[1]]$Year, out_lo, lty='dotted')
  lines(goose_multidata[[1]]$Year, out_hi, lty='dotted')
  
  abline(h=manage_target, col='darkgrey', lty='dashed')
  
  text(x=goose_multidata[[1]]$Year[length(goose_multidata[[1]]$Year)]-6, y=50000, 'Projected', pos=4)
  text(x=goose_multidata[[1]]$Year[1], y=50000, 'Observed', pos=4)
}

tiff(file = 'Figure6_2.tiff')
Fig6()
dev.off()


Fig5 <- function() {
  past_years <- goose_multidata[[1]]$Year<=last_year
  con_years <- goose_multidata[[1]]$Year==2015 | goose_multidata[[1]]$Year==2016
  plot(goose_multidata[[1]]$Year, goose_multidata[[1]]$y, type='n', ylim=c(0,50000), 
       xlab='Year', ylab='Estimated population size')
  rect(last_year+1, -10000, last_year+proj_yrs+5, 70000, border=NA, col='grey')
  box()
  lines(goose_multidata[[1]]$Year[past_years], goose_multidata[[1]]$Count[past_years], type='b', pch=21, col='black', bg='black')
  
  out_lo <- as.data.frame(NULL)
  out_hi <- as.data.frame(NULL)
  out_mn <- as.data.frame(NULL)
  for(i in 1:length(goose_multidata)) {
    out_lo <- rbind(out_lo, goose_multidata[[i]]$Npred_lo)
    out_hi <- rbind(out_hi, goose_multidata[[i]]$Npred_hi)
    out_mn <- rbind(out_mn, goose_multidata[[i]]$Npred_mn)
  }
  out_mn <- as.vector(apply(out_mn, 2, mean))
  out_lo <- as.vector(apply(out_lo, 2, min))
  out_hi <- as.vector(apply(out_hi, 2, max))
  
  #lines(goose_multidata[[1]]$Year, out_lo_mn, lty='dotted', col='darkgrey')
  #lines(goose_multidata[[1]]$Year, out_hi_mn, lty='dotted', col='darkgrey')
  lines(goose_multidata[[1]]$Year, out_mn, col='red')
  lines(goose_multidata[[1]]$Year, out_lo, lty='dotted')
  lines(goose_multidata[[1]]$Year, out_hi, lty='dotted')
  
  abline(h=manage_target, col='darkgrey', lty='dashed')
  
  text(x=goose_multidata[[1]]$Year[length(goose_multidata[[1]]$Year)]-6, y=50000, 'Projected', pos=4)
  text(x=goose_multidata[[1]]$Year[1], y=50000, 'Observed', pos=4)
}

tiff(file = 'Figure5_2.tiff')
Fig5()
dev.off()
