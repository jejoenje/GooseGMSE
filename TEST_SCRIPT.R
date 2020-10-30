### TESTING SCRIPT

### The following code replicates the functionality of the Shiny app (app.R), using some default parameters and a 
###  given input file.

rm(list = ls())
source("goose_predict_gui.R")
library(GMSE)

plot <- FALSE
past <- FALSE
resamp <- TRUE
extinct <- FALSE  
prev_params <- NULL

input = list()
### UNCOMMENT THIS AND SET TO EXAMPLE FILE TARGET:
#input$input_name$datapath = "/home/jeroen/Downloads/Stirling_model_GBG_input_file_-_Oct_20.csv"
input$sims_in = 10
input$yrs_in = 2
input$maxHB_in = 4000
input$target_in = 28000

clearExistingOutput()

### FOLLOWING IS THE CORE CALL
### This function in turn calls gmse_goose, `iterations` times.
sims <- gmse_goose_multiplot(
  data_file=input$input_name$datapath,
  iterations=input$sims_in,
  proj_yrs = input$yrs_in,
  max_HB=input$maxHB_in,
  manage_target = input$target_in)

input_list <- data.frame(datapath=input$input_name$datapath, 
                         sims_in=input$sims_in, 
                         yrs_in=input$yrs_in, 
                         maxHB_in=input$maxHB_in, 
                         target_in=input$target_in)

save(input_list, file='input.Rdata')
save(sims, file='sims.Rdata')

library(htmltools)
output_summary <- genSummary()
save(output_summary, file='output_summary.Rdata')

res <- gmse_goose_summarise(sims, input)

cull_summary <- cbind( (res$last_obs_yr+1):(res$end_yr) ,
                       floor(res$proj_y_mn),
                       floor(res$mean_HB),
                       floor(res$sd_HB),
                       floor(res$min_HB),
                       floor(res$max_HB))
cull_summary <- as.data.frame(cull_summary)
save(cull_summary, file='cull_summary.Rdata')

in_summary <- genInputSummary()
save(in_summary, file='in_summary.Rdata')

### AT THIS POINT, goosegmse_output.Rmd SHOULD BE ABLE TO GENERATE THE REPORT
