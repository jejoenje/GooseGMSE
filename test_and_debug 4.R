rm(list=ls())

source('goose_predict_gui.R')

input <- list(input_name=data.frame(
  datapath=as.vector('~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/Islay_Stirling_2015.xls')),
  sims_in=5, yrs_in=5, maxHB_in=4000, target_in=10000)
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

extinct <- FALSE

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




plot(goose_data$Year, goose_data$y, lwd=2, ylim=c(0,60000), type='l')
lines(goose_data$Year, goose_data$Npred_lo, col='red')
lines(goose_data$Year, goose_data$Npred_hi, col='red')



