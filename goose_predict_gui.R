### This is a version of goose_predict() specifically for the GUI which needs to be in a specific location

library(GMSE)
library(tools)
library(readxl)

load_input <- function(in_name) {

  ### load_input()
  ###
  ### Takes single argument, which should be a character string to the input file path.
  ### Loads input data using appropriate function depending on file type.
  ### Returns a data frame of input file data.
  
    if(is.null(in_name)) { 
      # Generates for Shiny app if user tries to runs simulations w/o input file.
      stop('Please choose a base data file.') 
    } else {
      # Identify input file name extension (type), and loads the file as a dataframe using 
      #  the appropriate data loader:
        ext <- file_ext(in_name)
        if(ext=='csv' | ext=='.CSV') {
            return(read.csv(in_name))
        }
        if(ext=='xls' | ext=='XLS') {
            return(as.data.frame(read_xls(in_name)))
        }
        if(ext=='xlsx' | ext=='XLSX') {
            return(as.data.frame(read_xlsx(in_name, sheet=1)))
        }
        stop('File loading failed, file type not recognised (.csv, .xls, or .xlsx only')
    }

}

sample_noNA <- function(x) {
  
  ### sample_noNA()
  ###
  ### Helper function which randomly samples 1 value from vector x, ignoring any missing values in x.
  
  avail <- x[!is.na(x)]
  sample(avail, 1)
}

logit <- function(p){
  
  ### logit()
  ###
  ### Helper function providin logit transform
  
    size <- length(p);
    resv <- rep(x = NA, length = size)
    for(i in 1:size){
        if(p[i] >= 0 & p[i] <= 1){
            resv[i] <- log(p[i] / (1 - p[i]));   
        } 
    }
    return(resv);
}


goose_rescale_AIG <- function(data, years = 22){
  
    AIGs   <- data$AIG[1:years];         # Take the years before the change
    tii    <- 1:years;                   # Corresponding time variable
    DATg   <- data.frame(AIGs,tii);      # Create dataframe with grass and time
    get_cf <- coef(object = lm(logit(AIGs/8000)~tii, data = DATg)); # Vals
    cf_p2  <- as.numeric(get_cf[1]);
    cf_p3  <- as.numeric(get_cf[2]);
    lmodg  <- nls(formula = AIGs~phi1/(1+exp(-(phi2+phi3*tii))),
                  data    = DATg, trace = FALSE,
                  start   = list(phi1 = 7000, phi2 = cf_p2, phi3 = cf_p3));
    newx   <- data.frame(tii = years + 1);             # Predict one year ahead
    pg     <- predict(object = lmodg, newdata = newx); 
    dif    <- data$AIG[(years+1):dim(data)[1]][1]-pg;  
   
    data$AIG[(years+1):dim(data)[1]] <- data$AIG[(years+1):dim(data)[1]] - dif;
    return(data);
}

goose_clean_data <- function(file){
  
    data   <- load_input(file);              # Load dataset
    
        ### data$y is the observed count plus the number culled on Islay:
    data$y <- data$Count+data$IslayCull
      
    data   <- goose_rescale_AIG(data = data, years = 22);
  
    data$AugTemp   <- as.numeric( scale(data$AugTemp) )
    data$IslayTemp <- as.numeric( scale(data$IslayTemp) )
    data$AugRain   <- as.numeric( scale(data$AugRain) )
    data$AIG.sc    <- as.numeric( scale(data$AIG) )
    data$IcelandCull[is.na(data$IcelandCull)] <- 0
    data$GreenlandCull[is.na(data$GreenlandCull)] <- 0
    
        ### The following ensures that if either G'land or Iceland culls were unavailable (NA) but the other was, the 
        ###  missing number is treated as zero, i.e. the total culled on G'land and Iceland (HB) is always at least the number
        ###  available for one of them (i.e. avoid NA's in a sum including NA's):
    
    data$HB <- data$IcelandCull+data$GreenlandCull
    data$HB[data$HB==0] <- NA
    data$IcelandCull[data$IcelandCull==0] <- NA
    data$GreenlandCull[data$GreenlandCull==0] <- NA
    
    return(data);
}  

goose_growth <- function(para, data){
  
    data_rows <- dim(data)[1];
    N_pred <- goose_pred(para = para, data = data);
  
    DEV    <- N_pred[3:data_rows] - data$y[3:data_rows];
    sq_Dev <- DEV * DEV;
    pr_sum <- sum( sq_Dev / N_pred[3:data_rows] );
    SS_tot <- (1 / pr_sum) * 1000;
    return(SS_tot);
}

goose_pred <- function(para, data){
  r_val        <- para[1];              # Maximum growth rate
  K_val        <- para[2];              # Carrying capacity
  G_rain_coeff <- para[3];              # Effect of precipitation on Greenland in August
  G_temp_coeff <- para[4];              # Effect of temperature on Greenland in August
  I_temp_coeff <- para[5];              # Effect of temperature on Islay the previous winter
  AIG_2_yrs    <- para[6];              # Effect of area of improved grassland 2 years prior
  #hunting_bag  <- para[7];             # Effect of hunting bag on G'land and Iceland - NO LONGER USED, SEE BELOW
  
  data_rows <- dim(data)[1];
  N_pred    <- rep(x = NA, times = data_rows);
  for(time in 3:data_rows){
      goose_repr   <- r_val * data$y[time - 1];
      goose_dens   <- 1 - (data$y[time -1] / (K_val * data$AIG[time - 1]));
      goose_now    <- data$y[time - 1];
      G_rain_adj   <- G_rain_coeff * data$AugRain[time - 1];
      G_temp_adj   <- G_temp_coeff * data$AugTemp[time - 1];
      I_temp_adj   <- I_temp_coeff * data$IslayTemp[time - 1];
      AIG_2_adj    <- AIG_2_yrs    * data$AIG.sc[time - 2];
      adjusted     <- G_rain_adj + G_temp_adj + I_temp_adj + AIG_2_adj
      #hunted       <- hunting_bag  * goose_now;                                # This was the 'old' version of removing a proportion
      N_pred[time] <- goose_repr * (goose_dens + adjusted) + goose_now - mean(data$HB, na.rm=T);    
      
      ### So, the prediction N_pred[time] here is the projected population size on Islay AFTER culling on G'land and Iceland,
      ###  but EXCLUDING anything 'to be' culled on Islay at [time].
      ### data$HB for the input file is the sum of the numbers culled on G'land and Iceland (treating an NA in one or the other as a zero
      ###  but keeps NA if both values are NA.
      ### By substracting mean(data$HB) here, the number removed due to culling in G'land and Iceland becomes a 'running mean' 
      ###  (i.e. changed as new data become available) and will be sampled from randomly for future projections.
  }
  
  return(N_pred);
}

get_goose_paras <- function(data, init_params = NULL){
    if( is.null(init_params) == TRUE ){
        init_params    <- c(0.1,6,0,0,0,0, 0);
    }
    contr_paras    <- list(trace = 1, fnscale = -1, maxit = 1000, factr = 1e-8,
                           pgtol = 0);
    get_parameters <- optim(par = init_params, fn = goose_growth, data = data,
                            method = "BFGS", control = contr_paras, 
                            hessian = TRUE);
    if(exists("progress_i")) {
        progress_i <- progress_i+1
        assign("progress_i", progress_i, envir = globalenv())
        progress$set(value = progress_i)
    }
    return(get_parameters);
}

goose_plot_pred <- function(data, year_start = 1987, ylim = c(10000, 60000),
                            plot = TRUE){
    params <- get_goose_paras(data = data);
    Npred  <- goose_pred(para = params$par, data = data);
    yrs    <- year_start:(year_start + length(data$y) - 1);
    if(plot == TRUE){
        par(mar = c(5, 5, 1, 1));
        plot(x =  yrs, y = data$y, pch = 1, ylim = ylim, cex.lab = 1.5,
             xlab="Year", ylab="Population size")         # Observed time series
        points(x = yrs, y = Npred, pch = 19, col = "red") # Predict time series
        oend <- length(data$y);
        points(x = yrs[3:oend], y = data$y[2:(oend - 1)], pch = 19, 
               col = "blue");
    }
    return(Npred);
}

goose_predict_and_plot <- function(file, plot = TRUE){
    dat    <- read.csv(file);
    data   <- goose_clean_data(file);
    goosep <- goose_plot_pred(data = data, plot = plot);
    return(goosep);
}

goose_gmse_popmod <- function(goose_data){
    N_pred <- goose_plot_pred(data = goose_data, plot = FALSE);
    N_last <- length(N_pred);
    New_N  <- as.numeric(N_pred[N_last]);
    #New_N  <- New_N - (0.03 * New_N);                          #  Err no?
    if(New_N < 1){
        New_N <- 1;
        warning("Extinction has occurred");
    }
    return(New_N);
}

goose_gmse_obsmod <- function(resource_vector, obs_error, use_est){
    obs_err    <- rnorm(n = 1, mean = 0, sd = obs_error);
    obs_vector <- resource_vector + obs_err;
    if(use_est == -1){
        obs_vector <- obs_vector - abs(obs_error * 1.96);
    }
    if(use_est == 1){
        obs_vector <- obs_vector + abs(obs_error * 1.96);
    }
    return(obs_vector);
}

goose_gmse_manmod <- function(observation_vector, manage_target){
    manager_vector <- observation_vector - manage_target;
    if(manager_vector < 0){
        manager_vector <- 0;
    }
    return(manager_vector);
}

goose_gmse_usrmod <- function(manager_vector, max_HB){
    user_vector <- manager_vector;
    if(user_vector > max_HB){
        user_vector <- max_HB;
    }
    return(user_vector);
}

# goose_sim_paras <- function(goose_data){
#     last_row <- dim(goose_data)[1];
#     for(col in 1:dim(goose_data)[2]){
#         if( is.na(goose_data[last_row, col]) == TRUE ){
#             if(col < 6){
#                 goose_data[last_row, col] <- 0;
#             }else{
#                 all_dat   <- goose_data[,col];
#                 avail_dat <- all_dat[!is.na(all_dat)];
#                 rand_val  <- sample(x = avail_dat, size = 1);
#                 goose_data[last_row, col] <- rand_val;
#             }
#         }
#     }
#     return(goose_data);
# }

goose_fill_missing <- function(goose_data){

    ### goose_fill_missing()
    ### 
    ### Takes goose_data file as only argument.
    ### - Checks whether required parameters are in input data (Year, Count, IslayCull).
    ### - Deals with missing values in AIG, IslayTemp, AugRain, AugTemp, AIG.sc and HB. 
    ###   Where these values are missing, they are sampled randomly from previous values (ignoring any previous NA's):
    ### - Returns goose_data
    
    if(is.na(goose_data[nrow(goose_data),'Year'])) stop('Required data missing: Year')
    if(is.na(goose_data[nrow(goose_data),'Count'])) stop('Required data missing: Count')
    if(is.na(goose_data[nrow(goose_data),'IslayCull'])) stop('Required data missing: IslayCull')
    
    # Identify missing (NA) environmental variables:
    missing_env <- is.na(goose_data[nrow(goose_data),c('AIG','IslayTemp','AugRain','AugTemp','AIG.sc','HB')])
    missing_env <- dimnames(missing_env)[[2]][which(missing_env)]
    
    # Where missing (NA), replace with randomly sampled number from previous data (ignoring any previous NA values):
    goose_data[nrow(goose_data),missing_env] <- apply(goose_data[-nrow(goose_data),missing_env],2,function(x) sample_noNA(x))
    
    return(goose_data);
}


sim_goose_data <- function(gmse_results, goose_data){
    
    ### sim_goose_data()
    ###
    ### Takes GMSE 'basic' results (list of resource_resutls, observation_results, manager_results and user_results) and 
    ###  goose_data (input data, possibly with previously simulated new years added) as arguments. 
    ### - Calls goose_fill_missing() to ensure all previous years' data are available, or when not, sampled from previous years.
    ### - Generates a new line of future data, using output from GMSE (and thus the goose population model), and randomly samples
    ###    new environmental data for this year.
    ### - Adds this new line of data and return - value y is used as the basis for next year's projection.
    
    # gmse_pop holds the 'resource_results' from GMSE; i.e. the population projection for one year ahead.
    gmse_pop   <- gmse_results$resource_results;
    # gmse_obs holds the GMSE observation results.
    gmse_obs   <- gmse_results$observation_results;
    
    # gmse_man and gmse_cul holds GMSE manager results and GMSE user results, respectively.
    if(length(gmse_results$manager_results) > 1){
        gmse_man   <- gmse_results$manager_results[3];
    }else{
        gmse_man   <- as.numeric(gmse_results$manager_results);
    }
    if(length(gmse_results$user_results) > 1){
        gmse_cul   <- sum(gmse_results$user_results[,3]);
    }else{
        gmse_cul   <- as.numeric(gmse_results$user_results);
    }
    
    # Call goose_fill_missing() to address any missing values in the last input file line 
    #  (e.g. latest Iceland or Greenland cull data missing).
    goose_data <- goose_fill_missing(goose_data);
    
    # Some counters.
    rows       <- dim(goose_data)[1];
    cols       <- dim(goose_data)[2];
    
    # Build new line of data (new projection), same number of columns as goose_data
    new_r     <- rep(x = 0, times = cols);
    
    # YEAR: New year is last year plus 1:
    new_r[1]  <- goose_data[rows, 1] + 1;
    
    # COUNT: New population count is the number projected by GMSE minus the number proposed culled 
    #  (starting point for population projection for following year):
    new_r[2]  <- gmse_pop - gmse_cul;
    
    # ICELANDCULL: Set to NA as we are not explicitly separating numbers culled on either Iceland 
    #  or Greenland (See HB below).
    new_r[3]  <- NA;                   
    
    # ISLAYCULL: Number proposed culled on Islay in projected year./
    new_r[4]  <- gmse_cul;             
    
    # GREENLANDCULL: Set to NA as we are not explicitly separating numbers culled on either Iceland 
    #  or Greenland.
    new_r[5]  <- NA;                  
    
    # Future AIG, IslayTemp, AugRain and AugTemp are sampled randomly from previous years' data:
    new_r[6]  <- sample_noNA(goose_data[,6]);
    new_r[7]  <- sample_noNA(goose_data[,7]);
    new_r[8]  <- sample_noNA(goose_data[,8]);
    new_r[9]  <- sample_noNA(goose_data[,9]);
    
    # y: In observed years this was count + no. culled on Islay, in projected years this is GMSE 
    #  projection MINUS numbers proposed culled on Islay (i.e. the number culled on Islay is assumed 
    #  to be exactly what was set as the target cull). This is the population number used as the 
    #  starting point for next year's projection.
    new_r[10] <- gmse_pop - gmse_cul; 
    
    # Future AIG.sc and HB (total hunting bag on Iceland and Greenland) are sampled randomly from 
    #  previous years' data.
    new_r[11] <- sample_noNA(goose_data[,11]);
    new_r[12] <- sample_noNA(goose_data[,12]);
    
    # Add new projected data to existing data, and return:
    new_dat   <- rbind(goose_data, new_r);
    return(new_dat);
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
    while(years > 0){                                                # Count down number of years and for each add goose projections
        gmse_res_new   <- gmse_apply(res_mod = goose_gmse_popmod, 
                                     obs_mod = goose_gmse_obsmod,
                                     man_mod = goose_gmse_manmod,
                                     use_mod = goose_gmse_usrmod,
                                     goose_data = goose_data,
                                     manage_target = target, use_est = use_est,
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
    }
    goose_data <- goose_data[-(nrow(goose_data)),]        # Ignores the last "simulated" year as no numbers exist for it yet.
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


gmse_goose_multiplot <- function(data_file, proj_yrs, 
                                 obs_error = 1438.614, manage_target, 
                                 max_HB, iterations, 
                                 use_est = "normal"){
    
    goose_multidata <- NULL;
    for(i in 1:iterations){
        
        goose_multidata[[i]] <- gmse_goose(data_file = data_file,
                                           obs_error = obs_error,
                                           years = proj_yrs,
                                           manage_target = manage_target, 
                                           max_HB = max_HB, plot = FALSE,
                                           use_est = use_est);
        print(paste("Simulating ---------------------------------------> ",i));
    }
    goose_data <- goose_multidata[[1]];
    dat        <- goose_data[-1,];
    last_year  <- dat[dim(dat)[1], 1];
    yrs        <- dat[,1];
    NN         <- dat[,10];
    HB         <- dat[,3];
    pry        <- (last_year - proj_yrs):last_year;
    obsrvd     <- 1:(dim(dat)[1] - proj_yrs - 1);
    par(mar = c(5, 5, 1, 1));
    plot(x = yrs, y = NN, xlab = "Year", ylab = "Population size",
         cex = 1.25, pch = 20, type = "n", ylim = c(0, max(NN+20)), 
         cex.lab = 1.1, cex.axis = 1.1, lwd = 2);
    polygon(x = c(pry, 2*last_year, 2*last_year, rev(pry)), 
            y = c(rep(x = -10000, times = length(pry) + 1), 
                  rep(x = 2*max(NN), times = length(pry) + 1)), 
            col = "grey", border = NA);
    box();
    points(x = yrs[obsrvd], y = NN[obsrvd], cex = 1.25, pch = 20, type = "b");
    abline(h = manage_target, lwd = 0.8, lty = "dotted");
    text(x = dat[5,1], y = max(NN+10), labels = "Observed", cex = 1.25);
    text(x = pry[length(pry)], y = max(NN+10), labels = "Projected", cex = 1.25, pos = 2);
    for(i in 1:length(goose_multidata)){
        goose_data <- goose_multidata[[i]];
        dat <- goose_data[-1,];
        yrs <- dat[,1];
        NN  <- dat[,10];
        HB  <- dat[,3];
        pry <- (last_year):(yrs[length(yrs)]-2+20);
        points(x = yrs, y = NN, pch = 20, type = "l", lwd = 0.6);
    }
    dev.copy(png,file="mainPlot.png", width=800, height=600)
    dev.off()
    return(goose_multidata);
}


gmse_print_multiplot <- function(goose_multidata, manage_target, proj_yrs){
    iters      <- length(goose_multidata);
    rows       <- dim(goose_multidata[[1]])[1];
    goose_data <- goose_multidata[[1]];
    dat        <- goose_data;
    last_year  <- dat[dim(dat)[1], 1];
    yrs        <- dat[,1];
    NN         <- dat[,10];
    HB         <- dat[,3];
    pry        <- (last_year - proj_yrs):last_year;
    obsrvd     <- 1:(dim(dat)[1] - proj_yrs);
    
    par(mar = c(5, 5, 1, 1));
    
    yrs_plot <- which(yrs %in% pry[1]:last_year)
    
    png(file='zoomPlot.png',width=800, height=600)
    
    plot(x = yrs[yrs_plot], y = NN[yrs_plot], xlab = "Year", ylab = "Population size",
         cex = 1.25, pch = 20, type = "n", ylim = c(0, max(NN)), xaxt='n',
         cex.lab = 1.1, cex.axis = 1.1, lwd = 2);
    axis(1, at=yrs[yrs_plot], labels=yrs[yrs_plot])
    
    points(x = yrs[yrs_plot], y=NN[yrs_plot], cex = 1.25, pch = 20, type = "l")
    points(pry[1], NN[which(yrs==pry[1])], col='red', cex=1.5, pch=16)
    abline(h = manage_target, lwd = 0.8, lty = "dotted");

    for(i in 2:iters){
        goose_data <- goose_multidata[[i]];
        dat <- goose_data;
        NN  <- dat[yrs_plot,10];
        points(x = yrs[yrs_plot], y = NN, pch = 20, type = "l", lwd = 0.6);
    }
    
    dev.off()
    
}

gmse_goose_summarise <- function(multidat, input) {
    
    orig_data <- goose_clean_data(input$input_name$datapath)
    last_obs_yr <- max(orig_data$Year)
    proj_y <- lapply(multidat, function(x) x$y[x$Year>last_obs_yr])
    proj_y <- do.call(rbind, proj_y)
    proj_HB <- lapply(multidat, function(x) x$IslayCull[x$Year>last_obs_yr])
    proj_HB <- do.call(rbind, proj_HB)
    
    end_NN <- unlist(lapply(multidat, function(x) x$y[which.max(x$Year)]))
    end_yr <- max(sims[[1]]$Year)
    target_overlap <- input$target_in>apply(proj_y,2,min) & input$target_in<apply(proj_y,2,max)
    proj_y_mn <- apply(proj_y,2,mean)
    
    if(sum(target_overlap)==0) {
        first_overlap <- NA
    } else {
        first_overlap=min(((last_obs_yr+1):end_yr)[target_overlap])
    }
    
    return(list(end_yr=end_yr,                     # Last projected year
                end_min=min(end_NN),               # Minimum pop size in last projected year
                end_max=max(end_NN),               # Maximum pop size in last projected year
                end_mean=mean(end_NN),             # Mean pop size in last projected year
                all_NN=end_NN,                     # All population sizes in last projected year (across sims)
                proj_y_mn=proj_y_mn,               # Mean projected population size in each year (across sims)
                last_obs_yr=last_obs_yr,           # Last observed year
                proj_y=proj_y,                     # All projected population sizes (rows are sims, cols are years)
                proj_HB=proj_HB,                   # As above but for hunting bag (number culled)
                target_overlap=target_overlap,     # Logical: does range of pop sizes across sims overlap the target
                first_overlap=first_overlap,       # Year of first overlap for above
                mean_HB=apply(proj_HB, 2, mean),   # Mean number culled in each projected year across sims
                sd_HB=apply(proj_HB, 2, sd),       # SD of above
                min_HB=apply(proj_HB, 2, min),     # Minimum of above
                max_HB=apply(proj_HB, 2, max)      # Maximum of above
                ))

}

genInputSummary <- function() {
    data.frame(
        Parameter=c("No. of simulations", "Number of years projected", "Maximum no. culled", "Population target"), 
        Value=c(input$sims_in, input$yrs_in, input$maxHB_in, input$target_in)    
    )
}

genSummary <- function() {
    res <- gmse_goose_summarise(sims, input)
    
    if(is.na(res$first_overlap)) {
        first_overlap <- paste("The projected population does not overlap the target in any of the projected years")
    } else {
        first_overlap <- paste("The range of projected population sizes overlapped the target of", 
                               input$target_in, "individuals in", sum(res$target_overlap), "out of", res$end_yr-res$last_obs_yr, "projected years.",
                               "The first year in which the range of projected population sizes overlapped with the population target was", res$first_overlap)
    }
    
    p1 <- paste("In", res$end_yr,"the projected population size was between", floor(res$end_min), "and", floor(res$end_max), "individuals.") 
    p2 <- paste("The projected population in ", res$end_yr, 
                switch(as.character(res$end_min<input$target_in & res$end_max>input$target_in), 'TRUE'='does', 'FALSE'='does not'),
                "overlap with the population target of", input$target_in
    )
    p3 <- paste("After ", res$end_yr-res$last_obs_yr, "projected years, the mean population size is predicted to be", 
                abs(floor(res$end_mean)-input$target_in), "individuals", 
                switch(as.character(sign(floor(res$end_mean)-input$target_in)), "-1"="below", "1"="above"), 
                "the population target of", input$target_in
    )
    p4 <- first_overlap
    
    div(
        tags$ul(
            tags$li(p1),
            tags$li(p2),
            tags$li(p3),
            tags$li(p4, style = "color:red; font-weight: bold")
        )
    )
}


