# TESTING SCRIPT
rm(list = ls())
source("goose_predict_gui.R")
library(GMSE)

plot <- FALSE
past <- FALSE
resamp <- TRUE
extinct <- FALSE  
prev_params <- NULL

input = list()
input$input_name$datapath = "/home/jeroen/Downloads/example_data_CHECKED_Feb2020.csv"
input$sims_in = 10
input$yrs_in = 5
input$maxHB_in = 2000
input$target_in = 28000

clearExistingOutput()

### FOLLOWING IS THE CORE CALL:
# sims <- gmse_goose_multiplot(
#   data_file=input$input_name$datapath, 
#   iterations=input$sims_in, 
#   proj_yrs = input$yrs_in, 
#   max_HB=input$maxHB_in, 
#   manage_target = input$target_in)

### Function call:
# gmse_goose_multiplot <- function(data_file, proj_yrs, 
#                                  obs_error = 1438.614, manage_target, 
#                                  max_HB, iterations, 
#                                  use_est = "normal")
data_file=input$input_name$datapath
iterations=input$sims_in
proj_yrs = input$yrs_in
max_HB=input$maxHB_in
manage_target = input$target_in
obs_error = 1438.614

goose_multidata <- NULL

for(i in 1:iterations) {
  years <- proj_yrs
  goose_multidata[[i]] <- gmse_goose(data_file = data_file,
                                     obs_error = obs_error,
                                     years = proj_yrs,
                                     manage_target = manage_target,
                                     max_HB = max_HB, plot = FALSE,
                                     use_est = 0)
}

goose_data <- goose_clean_data(file = data_file)  
last_year  <- goose_data[dim(goose_data)[1], 1]

jpeg("test3.jpg")

### Sets up empty plot region:
past_years <- goose_multidata[[1]]$Year<=last_year
con_years <- goose_multidata[[1]]$Year==2015 | goose_multidata[[1]]$Year==2016
plot(goose_multidata[[1]]$Year, goose_multidata[[1]]$y, type='n', ylim=c(0,50000), 
     xlab='Year', ylab='Estimated population size')
rect(last_year+1, -10000, last_year+proj_yrs+5, 70000, border=NA, col='grey')
box()

### Plots projected trajectories:
if(resamp == TRUE) {
  for(i in 1:length(goose_multidata)) {
    lines(goose_multidata[[i]]$Year[!past_years], goose_multidata[[i]]$Npred_mn[!past_years], col = scales::alpha('red', 0.5))
  }
  
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
  
  lines(goose_multidata[[1]]$Year, out_lo, lty='dotted', col = scales::alpha('red', 0.5))
  lines(goose_multidata[[1]]$Year, out_hi, lty='dotted', col = scales::alpha('red', 0.5))
  
}
if(resamp == FALSE) {
  for(i in 1:length(goose_multidata)) {
    lines(goose_multidata[[i]]$Year[!past_years], goose_multidata[[i]]$y[!past_years], col = scales::alpha('red', 0.5))
  }

}
### Plots past numbers:
lines(goose_multidata[[1]]$Year[past_years], goose_multidata[[1]]$Count[past_years], type='b', pch=21, col='black', bg='black')

### "Link" last obs and first pred with a line:
last_obs = goose_multidata[[1]]$Count[goose_multidata[[1]]$Year==last_year]
first_pred = goose_multidata[[1]]$y[goose_multidata[[1]]$Year==last_year+1]
lines(c(last_year, last_year+1),c(last_obs,first_pred), col = scales::alpha('red', 0.5))

abline(h=manage_target, col='darkgrey', lty='dashed')

text(x=goose_multidata[[1]]$Year[length(goose_multidata[[1]]$Year)]-6, y=50000, 'Projected', pos=4)
text(x=goose_multidata[[1]]$Year[1], y=50000, 'Observed', pos=4)

dev.off()


# 1min 40 (resamp = TRUE, past = TRUE, 10 sims, 5 years)
# 0min 47 (resamp = TRUE, past = FALSE, 10 sims, 5 years)
# 0min 38 (resamp = FALSE, past = FALSE, 10 sims, 5 years)