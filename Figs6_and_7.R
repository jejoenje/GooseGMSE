rm(list=ls())

source('goose_predict_gui.R')
source('ggmse_default_test_pars.R')
goose_data <- goose_clean_data(input$input_name$datapath)
last_year <- tail(goose_data$Year, 1)

library(scales)

### Combine output files into single structure:

out_files <- list.files('out')
out_files <- c(out_files, "text.txt")
sim_files <- out_files[grepl('.Rdata', out_files)]
sims <- list()
for(i in 1:length(sim_files)) {
  load(paste0("out/",sim_files[i]))
  sims[[i]] <- goose_multidata
}
goose_multidata <- list()
for(i in 1:length(sim_files)) {
  goose_multidata <- c(goose_multidata, sims[[i]])  
}

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
  out_lo_mn <- as.vector(apply(out_lo, 2, median))
  out_hi_mn <- as.vector(apply(out_hi, 2, median))
  out_lo <- as.vector(apply(out_lo, 2, min))
  out_hi <- as.vector(apply(out_hi, 2, max))
  
  #lines(goose_multidata[[1]]$Year, out_lo_mn, lty='dotted', col='darkgrey')
  #lines(goose_multidata[[1]]$Year, out_hi_mn, lty='dotted', col='darkgrey')
  lines(goose_multidata[[1]]$Year, out_lo, lty='dotted')
  lines(goose_multidata[[1]]$Year, out_hi, lty='dotted')
  
  # Add dashed line between the last measured and first predicted year:
  lines(c(last_year, last_year+1), 
        c(goose_multidata[[1]]$Count[goose_multidata[[1]]$Year==last_year],
          goose_multidata[[1]]$Npred_mn[goose_multidata[[1]]$Year==last_year+1]), lty='dashed')
  
  abline(h=manage_target, col='darkgrey', lty='dashed')
  
  text(x=goose_multidata[[1]]$Year[length(goose_multidata[[1]]$Year)]-6, y=50000, 'Projected', pos=4)
  text(x=goose_multidata[[1]]$Year[1], y=50000, 'Observed', pos=4)
}

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
  
  # Add dashed line between the last measured and first predicted year:
  lines(c(last_year, last_year+1), 
        c(goose_multidata[[1]]$Count[goose_multidata[[1]]$Year==last_year],
          goose_multidata[[1]]$Npred_mn[goose_multidata[[1]]$Year==last_year+1]), lty='dashed')
  
  abline(h=manage_target, col='darkgrey', lty='dashed')
  
  text(x=goose_multidata[[1]]$Year[length(goose_multidata[[1]]$Year)]-6, y=50000, 'Projected', pos=4)
  text(x=goose_multidata[[1]]$Year[1], y=50000, 'Observed', pos=4)
}

#tiff('Figure6.tiff', width=800, height=800, pointsize=22)
Fig6()
#dev.off()

#tiff('Figure7.tiff', width=800, height=800, pointsize=22)
Fig7()
#dev.off()

