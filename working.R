library(parallel)
library(foreach)

source('ggmse_default_test_pars.R')

goose_multidata <- NULL

cl <- parallel::makeForkCluster(8)
doParallel::registerDoParallel(cl)

system.time({
  for(i in 1:iterations){
    
    goose_multidata[[i]] <- gmse_goose(data_file = data_file,
                                       obs_error = obs_error,
                                       years = proj_yrs,
                                       manage_target = manage_target,
                                       max_HB = max_HB, plot = FALSE,
                                       use_est = 0)
  }
  
})

Fig7 <- function() {
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

