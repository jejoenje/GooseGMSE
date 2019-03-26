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

# This runs the simulations using the input vals and plots the main output graph:    
# sims <- gmse_goose_multiplot(
#   data_file=input$input_name$datapath, 
#   iterations=input$sims_in, 
#   proj_yrs = input$yrs_in, 
#   max_HB=input$maxHB_in, 
#   manage_target = input$target_in)

# gmse_goose_multiplot <- function(data_file, proj_yrs, 
#                                  obs_error = 1438.614, manage_target, 
#                                  max_HB, iterations, 
#                                  use_est = "normal"){

obs_error = 1438.614
goose_multidata <- NULL

for(i in 1:iterations){
  goose_multidata[[i]] <- gmse_goose(data_file = data_file,
                                     obs_error = obs_error,
                                     years = proj_yrs,
                                     manage_target = manage_target,
                                     max_HB = max_HB, plot = FALSE,
                                     use_est = use_est);
   print(paste("Simulating ---------------------------------------> ",i));
}


gmse_goose <- function(data_file, manage_target, max_HB, 
                       obs_error = 1438.614, years, use_est = "normal",
                       plot = TRUE){
  # -- Initialise ------------------------------------------------------------
  proj_yrs   <- years;
  goose_data <- goose_clean_data(file = data_file);
  last_year  <- goose_data[dim(goose_data)[1], 1];
  use_est    <- 0;
  if(use_est == "cautious"){
    use_est <- -1;
  }
  if(use_est == "aggressive"){
    use_est <- 1;
  }
  assign("goose_data", goose_data, envir = globalenv() );
  assign("target", manage_target, envir = globalenv() );
  assign("max_HB", max_HB, envir = globalenv() );
  assign("obs_error", obs_error, envir = globalenv() );
  assign("use_est", use_est, envir = globalenv() );
  gmse_res   <- gmse_apply(res_mod = goose_gmse_popmod, 
                           obs_mod = goose_gmse_obsmod,
                           man_mod = goose_gmse_manmod,
                           use_mod = goose_gmse_usrmod,
                           goose_data = goose_data, obs_error = obs_error,
                           manage_target = target, max_HB = max_HB,
                           use_est = use_est, stakeholders = 1, 
                           get_res = "full");
  goose_data <- sim_goose_data(gmse_results = gmse_res$basic,
                               goose_data = goose_data);
  assign("goose_data", goose_data, envir = globalenv() );
  assign("target", manage_target, envir = globalenv() );
  assign("max_HB", max_HB, envir = globalenv() );
  assign("obs_error", obs_error, envir = globalenv() );
  assign("use_est", use_est, envir = globalenv() );
  assign("gmse_res", gmse_res, envir = globalenv() );
  # -- Simulate --------------------------------------------------------------
  while(years > 0){    # Count down number of years and for each add goose projections
    gmse_res_new   <- gmse_apply(res_mod = goose_gmse_popmod, 
                                 obs_mod = goose_gmse_obsmod,
                                 man_mod = goose_gmse_manmod,
                                 use_mod = goose_gmse_usrmod,
                                 goose_data = goose_data,
                                 manage_target = manage_target, use_est = use_est,
                                 max_HB = max_HB, obs_error = obs_error,
                                 stakeholders = 1, get_res = "full");
    if(as.numeric(gmse_res_new$basic[1]) == 1){
      break;      
    }
    assign("gmse_res_new", gmse_res_new, envir = globalenv() );
    gmse_res   <- gmse_res_new;
    assign("gmse_res", gmse_res, envir = globalenv() );
    goose_data <- sim_goose_data(gmse_results = gmse_res$basic, 
                                 goose_data = goose_data);
    assign("goose_data", goose_data, envir = globalenv() );
    assign("target", manage_target, envir = globalenv() );
    assign("max_HB", max_HB, envir = globalenv() );
    assign("obs_error", obs_error, envir = globalenv() );
    assign("use_est", use_est, envir = globalenv() );
    years <- years - 1;
  }   # Ignores the last "simulated" year as no numbers exist for it yet.
  goose_data <- goose_data[-(nrow(goose_data)),];
  if(plot == TRUE){
    dat <- goose_data[-1,];
    yrs <- dat[,1];
    NN  <- dat[,10];
    HB  <- dat[,3];
    pry <- (last_year):(yrs[length(yrs)]-2+20);
    par(mar = c(5, 5, 1, 1));
    plot(x = yrs, y = NN, xlab = "Year", ylab = "Population size",
         cex = 1.25, pch = 20, type = "b", ylim = c(0, max(NN)), 
         cex.lab = 1.5, cex.axis = 1.5, lwd = 2);
    polygon(x = c(pry, rev(pry)), 
            y = c(rep(x = -10000, times = proj_yrs + 20), 
                  rep(x = 2*max(NN), times = proj_yrs + 20)),   
            col = "grey", border = NA);
    box();
    points(x = yrs, y = NN, cex = 1.25, pch = 20, type = "b");
    points(x = yrs, y = HB, type = "b", cex = 1.25, col = "red", 
           pch = 20, lwd = 2);
    abline(h = manage_target, lwd = 0.8, lty = "dotted");
    text(x = dat[5,1], y = max(NN), labels = "Observed", cex = 2.5);
    text(x = pry[5] + 1, y = max(NN), labels = "Projected", cex = 2.5);
  }
  return(goose_data);
}
  
