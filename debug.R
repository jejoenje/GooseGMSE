### 200 runs of 10:

for(z in 1:200) {
  source('ggmse_default_test_pars.R')
  
  ### Non-parallel version of the below:
  # system.time({
  #   for(i in 1:iterations) {
  #     print(paste("Iteration",i))
  #     years <- proj_yrs
  #     prev_params <- NULL
  #     goose_multidata[[i]] <- gmse_goose(data_file = data_file,
  #                                        obs_error = obs_error,
  #                                        years = proj_yrs,
  #                                        manage_target = manage_target,
  #                                        max_HB = max_HB, plot = FALSE,
  #                                        use_est = 0)
  #   }
  #   
  # })
  
  cl <- parallel::makeForkCluster(8, outfile="cl_log.txt")
  doParallel::registerDoParallel(cl)
  
  system.time({
    goose_multidata <- foreach(i=1:iterations) %dopar% {
      cat(sprintf("%s: Iteration %d\n", as.character(Sys.time()), i))
      years <- proj_yrs
      gmse_goose(data_file = data_file,
                 obs_error = obs_error,
                 years = proj_yrs,
                 manage_target = manage_target,
                 max_HB = max_HB, plot = FALSE,
                 use_est = 0)
    }
    
  })
  # user  system elapsed 
  # 3.252   8.403 326.707   ### 100 runs, 8-core cluster
  # user  system elapsed    
  # 2.986   7.830 317.803   ### 100 runs, 6-core
  # user  system elapsed   
  # 2.948   8.440 343.354   ### 100 runs, 4-core
  # user  system elapsed     
  # 3.640   7.753 543.525   ### 100 runs, 2-core
  
  stopCluster(cl)
  gc()
  
  save(goose_multidata, file=paste0("goose_multidata_",format(Sys.time(),"%Y%m%d%_H%M%S"),".Rdata"))
}







last_year <- tail(goose_multidata[[1]]$Year,1)-years

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


