rm(list=ls())

source('goose_predict_gui.R')

library(scales)

input <- list(input_name=data.frame(
  datapath=as.vector('~/example_data_UPDATED_April2019.csv')),
  sims_in=10, yrs_in=10, maxHB_in=2500, target_in=29000)
input$input_name$datapath <- as.vector(input$input_name$datapath)
iterations <- input$sims_in
years <- input$yrs_in
proj_yrs <- years
manage_target <- input$target_in
max_HB <- input$maxHB_in
data_file <- as.vector(input$input_name$datapath)
obs_error = 2846/sqrt(5)

plot = FALSE
past = FALSE

resamp = TRUE
extinct = FALSE  

#load('Example_sims_1000.Rdata')

goose_multidata <- NULL
