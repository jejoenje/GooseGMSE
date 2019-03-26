rm(list=ls())


source('goose_predict_gui.R')

input <- list(input_name=data.frame(
  datapath=as.vector('/home/jm241/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/Islay_Stirling_2017.xls')),
  sims_in=3, yrs_in=1, maxHB_in=2000, target_in=32000)
input$input_name$datapath <- as.vector(input$input_name$datapath)
iterations <- input$sims_in
years <- input$yrs_in
proj_yrs <- years
manage_target <- input$target_in
max_HB <- input$maxHB_in
data_file <- as.vector(input$input_name$datapath)
obs_error = 1438.614
plot = TRUE


