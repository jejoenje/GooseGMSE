rm(list=ls())

source('goose_predict_gui_TEMP.R')

library(scales)

input <- list(input_name=data.frame(
  datapath=as.vector('~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/example_data_UPDATED_April2019.csv')),
  sims_in=1, yrs_in=1, maxHB_in=2500, target_in=29000)
input$input_name$datapath <- as.vector(input$input_name$datapath)
iterations <- input$sims_in
years <- input$yrs_in
proj_yrs <- years
manage_target <- input$target_in
data_file <- as.vector(input$input_name$datapath)
obs_error = 1438.614
plot = TRUE
past = FALSE

resamp = TRUE

extinct = FALSE  

prev_params <- NULL
goose_multidata <- NULL

hb_range <- seq(0,20000,1000)

out <- as.data.frame(NULL)

for(i in 1:length(hb_range)){
    
    max_HB <- hb_range[i]
  
    years <- proj_yrs
    
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
    
    
    out <- rbind(out, data.frame(hb=max_HB, 
                                 Npred_mn=goose_data$Npred_mn[nrow(goose_data)]-goose_data$IslayCull[nrow(goose_data)], 
                                 Npred_lo=goose_data$Npred_lo[nrow(goose_data)]-goose_data$IslayCull[nrow(goose_data)],
                                 Npred_hi=goose_data$Npred_hi[nrow(goose_data)]-goose_data$IslayCull[nrow(goose_data)]
                                 )
          )
    
  }

figX <- function() {
  plot(out$hb, out$Npred_mn, ylim=c(min(out$Npred_lo)-1000,max(out$Npred_hi)+1000), type='n', 
       xlab = 'Maximum cull level in year t', ylab = 'Estimated population size at start of year t+1', col='white')
  polygon(c(out$hb, rev(out$hb)), c(out$Npred_hi, rev(out$Npred_lo)),
          col = "lightgrey", border = NA)
  lines(out$hb, out$Npred_mn)
  abline(h=goose_data$y[nrow(goose_data)-1], lty='dashed',col='blue', lwd=2)
  abline(h=manage_target, lty='dotted',col='red', lwd=2)
  
  
  ### The bottom CI overlaps the manage target at...
  coef(lm(Npred_lo ~ hb, data = out))
  bottom_cross <- abs((coef(lm(Npred_lo ~ hb, data = out))[1]-manage_target)/coef(lm(Npred_lo ~ hb, data = out))[2])
  bottom_cross
  abline(v=bottom_cross, lty="dashed")
  
}

  tiff('FigX.tiff')
  figX()
  dev.off()

