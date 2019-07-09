rm(list=ls())

source('goose_predict_gui.R')

library(scales)

input <- list(input_name=data.frame(
  datapath=as.vector('~/example_data_UPDATED_April2019.csv')),
  sims_in=1, yrs_in=1, maxHB_in=2500, target_in=29000)
input$input_name$datapath <- as.vector(input$input_name$datapath)
iterations <- input$sims_in
years <- input$yrs_in
proj_yrs <- years
manage_target <- input$target_in
max_HB <- input$maxHB_in
data_file <- as.vector(input$input_name$datapath)
obs_error = 1438.614

plot = FALSE
past = TRUE

resamp = TRUE
extinct = FALSE  
prev_params <- NULL

#load('Example_sims_1000.Rdata')

goose_multidata <- NULL
