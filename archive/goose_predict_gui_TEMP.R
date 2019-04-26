### This is a version of goose_predict() specifically for the GUI which needs to be in a specific location

library(GMSE)
library(tools)
library(readxl)
library(HelpersMG)
library(parallel)

qsave <- function(dat, fname='temp.csv') {
  pth <- paste('~/Documents/docs/temp/',fname,sep='')
  dat$AIG <- NULL
  dat$IslayTemp <- NULL
  dat$AugRain <- NULL
  dat$AIG.sc <- NULL
  dat$HB <- round(dat$HB,1)
  dat$Npred_mn <- round(dat$Npred_mn,1)
  dat$Npred_lo <- round(dat$Npred_lo,1)
  dat$Npred_hi <- round(dat$Npred_hi,1)
  write.csv(dat,pth,row.names=F)
}

flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}

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
  ### Helper function which randomly samples 1 value from vector x, 
  ### ignoring any missing values in x.
  
  avail <- x[!is.na(x)]
  sample(avail, 1)
}

# This is in response to reviewer comments. It first fits a linear model to
# detect any trend in a particular variable over years, then samples from the
# residuals of the model. The returned value preserves any linear trend with
# an error that is sampled from the error structure of the model.
# Note: This handles NA values without any problems.
sample_trended <- function(x){
    yr  <- 1:length(x);
    mod <- lm(x ~ yr);
    b0  <- as.numeric(mod$coefficients[1]);
    b1  <- as.numeric(mod$coefficients[2]);
    er  <- as.numeric(mod$residuals);
    sr  <- sample(x = er, size = 1);
    val <- b0 + b1 * (length(x)+1) + sr;
    return(val);
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
  
    ### goose_rescale_AIG()
    ###
    ### Rescales AIG values in input data and produces AIG.sc column.
    ### Addresses the fact pre-2008, AIG was measured in a different way to later years.
  
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
  
  ### goose_clean_data()
  ### 
  ### Single argument, which is a character string of the input file to be loaded.
  ### - Calls load_input()
  ### - Calculates 'working' data, i.e. total number of geese, scales/standardises 
  ###    input variables for model fit.
  ### - Calculates total number culled on Iceland and Greenland as variable HB, 
  ###    ignoring any NA's.
  
  data   <- load_input(file);              # Load dataset
  
  av.cull.per.month <- round(data$IslayCull/5)
  
  # Add to count
  monthly.count.plu.av.cull <- data[1:nrow(data),c("November","December","January","February","March")]+av.cull.per.month
  
  # data$y is the observed count plus the number culled on Islay:
  #data$y <- data$Count+data$IslayCull
  data$y <- round(apply(monthly.count.plu.av.cull,1,mean,na.rm=T))
  
  # Rescale AIG ensuring consistency in measurementS:
  data   <- goose_rescale_AIG(data = data, years = 22);
  
  # Scale/standardise environmental variables:
  data$AugTemp   <- as.numeric( scale(data$AugTemp) )
  data$IslayTemp <- as.numeric( scale(data$IslayTemp) )
  data$AugRain   <- as.numeric( scale(data$AugRain) )
  data$AIG.sc    <- as.numeric( scale(data$AIG) )
  
  # The following ensures that if either G'land or Iceland culls were unavailable 
  #  (NA) but the other was, the missing number is treated as zero, i.e. the total 
  #  culled on G'land and Icelan#d (HB) is always at least the number available for 
  #  one of them (i.e. avoid NA's in a sum including NA's):
  
  data$IcelandCull[is.na(data$IcelandCull)] <- 0
  data$GreenlandCull[is.na(data$GreenlandCull)] <- 0
  data$HB <- data$IcelandCull+data$GreenlandCull
  #data$HB[data$HB==0] <- NA
  #data$IcelandCull[data$IcelandCull==0] <- NA
  #data$GreenlandCull[data$GreenlandCull==0] <- NA
  
  return(data);
}  

# goose_growth <- function(para, data){
#   
#   ### goose_growth()
#   ### OLD FUNCTION VERSION - HERE JUST FOR REFERENCE
#   ### Takes fitted model parameters and data as arguments.
#   ###  - Calculates prediction using goose population model (goose_pred()) and returns 
#   ###    measure of model fit (sum of squared deviations).
#   ###  - Used in model fitting (optimisation) function
#   
#     data_rows <- dim(data)[1];
#     N_pred <- goose_pred(para = para, data = data);
#   
#     DEV    <- N_pred[3:data_rows] - data$y[3:data_rows];
#     sq_Dev <- DEV * DEV;
#     pr_sum <- sum( sq_Dev / N_pred[3:data_rows] );
#     SS_tot <- (1 / pr_sum) * 1000;   # Total sum of squares, scaled for convenience
#     return(SS_tot);
# }

### This is a start at an alternative for goose_growth(),
###  this one maximising (Poisson) likelihood instead.
goose_growth <- function(para, dat) {
  data_rows <- dim(dat)[1];
  N_pred <- goose_pred(para = para, dat = dat);
  return(-sum(dpois(dat$y, N_pred, log=T),na.rm=T))
}

### This is a start at an alternative for goose_growth(),
###  this one maximising (Poisson) likelihood instead.
## RETURNS NEGATIVE LOG LIKELIHOOD  - for mle2()
goose_growth_mle <- function(b1, b2, b3, b4, b5, b6, dat=goose_data) {
  data_rows <- dim(dat)[1];
  N_pred <- goose_pred_mle(b1, b2, b3, b4, b5, b6, dat = dat);
  return(-sum(dpois(dat$y, N_pred, log=T),na.rm=T))
}

# Parameterisation of goose_pred() to work with bbmle::mle2()
goose_pred_mle <- function(b1, b2, b3, b4, b5, b6, dat){  

  ### goose_pred()
  ### 
  ### MAIN POPULATION MODEL PREDICTION FUNCTION
  ###
  ### Takes optimised parameters and data as input
  ###  - Extract input parameters from para vector.
  ###  - Produce a prediction for each line in the data, given environmental variables.
  ###  - Returns vector of predictions.
  
  r_val        <- b1             # Maximum growth rate
  K_val        <- b2              # Carrying capacity
  G_rain_coeff <- b3              # Effect of precipitation on Greenland in August
  G_temp_coeff <- b4              # Effect of temperature on Greenland in August
  I_temp_coeff <- b5              # Effect of temperature on Islay the previous winter
  AIG_2_yrs    <- b6              # Effect of area of improved grassland 2 years prior
  
  # Make as many predictions as there are lines in data:
  data_rows <- nrow(dat)
  N_pred    <- rep(NA, data_rows)
  
  # Starting at year 3 (we need data from at least 2 years ago), run through each input line.
  for(time in 3:data_rows){
    # Reproduction rate is max growth rate times previous years' population size
    goose_repr   <- r_val * dat$y[time - 1];
    # Goose density/carrying capacity term is function of numbers in previous year, K and AIG
    goose_dens   <- 1 - (dat$y[time -1] / (K_val * dat$AIG[time - 1]));
    # 'Current' population (previous year)
    goose_now    <- dat$y[time - 1];
    # Effect of rainfall in Greenland in previous year
    G_rain_adj   <- G_rain_coeff * dat$AugRain[time - 1];
    # Effect of temperature of Greenland in previous year
    G_temp_adj   <- G_temp_coeff * dat$AugTemp[time - 1];
    # Effect of temperature on Islay in previous year
    I_temp_adj   <- I_temp_coeff * dat$IslayTemp[time - 1];
    # Effect of AIG on Islay in the year before last
    AIG_2_adj    <- AIG_2_yrs    * dat$AIG.sc[time - 2];
    # Sum the combined effect of above effects for convenience
    adjusted     <- G_rain_adj + G_temp_adj + I_temp_adj + AIG_2_adj
    # Next years' population size is reproduction rate times the density term, adjusted by 
    #  environmental effects, minus the number taken on Greenland and Iceland, which is taken 
    #  as the mean of the previous years' take there.
    N_pred[time] <- goose_repr * (goose_dens + adjusted) + goose_now - mean(dat$HB/dat$y, na.rm=T)    
    
    ### So, the prediction N_pred[time] here is the projected population size on Islay AFTER 
    ###   culling on G'land and Iceland, but EXCLUDING anything 'to be' culled on Islay at [time].
    ### data$HB for the input file is the sum of the numbers culled on G'land and Iceland (treating 
    ###  an NA in one or the other as a zero but keeps NA if both values are NA.
    ### By substracting mean(data$HB/data$y) here, the number removed due to culling in G'land and Iceland 
    ###  becomes a 'running mean' (i.e. changed as new data become available) and will be sampled 
    ###  from randomly for future projections.
  }
  
  N_pred <- floor(N_pred)
  
  return(N_pred)
}

## Parameterisation of goose_pred() to work with optim()
goose_pred <- function(para, dat){  
  ### goose_pred()
  ### 
  ### MAIN POPULATION MODEL PREDICTION FUNCTION
  ###
  ### Takes optimised parameters and data as input
  ###  - Extract input parameters from para vector.
  ###  - Produce a prediction for each line in the data, given environmental variables.
  ###  - Returns vector of predictions.
  
  r_val        <- para[1]              # Maximum growth rate
  K_val        <- para[2]              # Carrying capacity
  G_rain_coeff <- para[3]              # Effect of precipitation on Greenland in August
  G_temp_coeff <- para[4]              # Effect of temperature on Greenland in August
  I_temp_coeff <- para[5]              # Effect of temperature on Islay the previous winter
  AIG_2_yrs    <- para[6]              # Effect of area of improved grassland 2 years prior
  
  
  # Make as many predictions as there are lines in data:
  data_rows <- length(dat[,1])
  N_pred    <- rep(NA, data_rows)
  
  # Starting at year 3 (we need data from at least 2 years ago), run through each input line.
  for(time in 3:data_rows){
      # Reproduction rate is max growth rate times previous years' population size
      goose_repr   <- r_val * dat$y[time - 1];
      # Goose density/carrying capacity term is function of numbers in previous year, K and AIG
      goose_dens   <- 1 - (dat$y[time -1] / (K_val * dat$AIG[time - 1]));
      # 'Current' population (previous year)
      goose_now    <- dat$y[time - 1];
      # Effect of rainfall in Greenland in previous year
      G_rain_adj   <- G_rain_coeff * dat$AugRain[time - 1];
      # Effect of temperature of Greenland in previous year
      G_temp_adj   <- G_temp_coeff * dat$AugTemp[time - 1];
      # Effect of temperature on Islay in previous year
      I_temp_adj   <- I_temp_coeff * dat$IslayTemp[time - 1];
      # Effect of AIG on Islay in the year before last
      AIG_2_adj    <- AIG_2_yrs    * dat$AIG.sc[time - 2];
      # Sum the combined effect of above effects for convenience
      adjusted     <- G_rain_adj + G_temp_adj + I_temp_adj + AIG_2_adj
      # Next years' population size is reproduction rate times the density term, adjusted by 
      #  environmental effects, minus the number taken on Greenland and Iceland, which is taken 
      #  as the mean of the previous years' take there.
      N_pred[time] <- goose_repr * (goose_dens + adjusted) + goose_now - mean(dat$HB/dat$y, na.rm=T)    
      
      ### So, the prediction N_pred[time] here is the projected population size on Islay AFTER 
      ###   culling on G'land and Iceland, but EXCLUDING anything 'to be' culled on Islay at [time].
      ### data$HB for the input file is the sum of the numbers culled on G'land and Iceland (treating 
      ###  an NA in one or the other as a zero but keeps NA if both values are NA.
      ### By substracting mean(data$HB/data$y) here, the number removed due to culling in G'land and Iceland 
      ###  becomes a 'running mean' (i.e. changed as new data become available) and will be sampled 
      ###  from randomly for future projections.
  }
  
  N_pred <- floor(N_pred)
  
  return(N_pred)
}

get_goose_paras <- function(dat, init_params = NULL){
    
    ### get_goose_paras()
    ###
    ### Runs optimisation for population model parameters.
    ### Takes goose_data and initial parameters as input (latter not required)
    
    # Set init parameters if not provided
    if( is.null(init_params) ) {
        init_params    <- c(0.1,6,0,0,0,0);                
    }
  
    # Run optimisation routine, using the goose_growth() function, goose_data and init_params
    ## PREVIOUS:
    # Set control parameters for optim() function:
    # contr_paras    <- list(trace = 1, fnscale = -1, maxit = 1000, factr = 1e-8,
    #                      pgtol = 0);
    # get_parameters <- optim(par = init_params, fn = goose_growth, data = data,
    #                         method = "BFGS", control = contr_paras, 
    #                         hessian = TRUE);
    
    ## NEW ATTEMPT (LL)
    ## Note I'm currently not using any of the control pars; didn't seem to be necessary for now...
    dat$y <- floor(dat$y)
    #dat$y[dat$y<0] <- 0
    contr_paras    <- list(trace = 1, maxit = 500);
    get_parameters <- optim(par = init_params, fn = goose_growth, dat = dat, method='L-BFGS-B', control = contr_paras,
                            hessian = TRUE);
    
    ### ALTERNATIVE:
    #bbmle::mle2(goose_growth_mle, start=test2, optimizer='nlminb')
    
    # Updates progress bar when running as Shiny app
    if(exists("progress_i")) {
        progress_i <- progress_i+1
        assign("progress_i", progress_i, envir = globalenv())
        progress$set(value = progress_i)
    }
    
    # Return optimised parameters
    return(get_parameters);
}

### This is an attempt at a function that simulates from parameter distributions
res_sim <- function(pars, dat, past=FALSE, reps=1000) {
  
  #params <- pars$par
  ses <- SEfromHessian(pars$hessian)
  dat$y_mn <- NA
  dat$y_lo <- NA
  dat$y_hi <- NA
  
  if(past==FALSE) {
    calc_dat <- dat[dat$Year>(last_year-3),]
  } else {
    calc_dat <- dat
  }
  
  res_sim <- matrix(nrow=reps,ncol=nrow(calc_dat))
  
  datr <- list(y_HB_ER(calc_dat)) # Starts with a single resampled data frame to base the list off
  for (r in 1:(reps)){
    datr[[length(datr)+1]] <- y_HB_ER(calc_dat)
  }
  datr[[length(datr)]] <- NULL  # Remove the reps + 1 (because we started with one)
  
  pred_with_new <- function(z) {
    goose_pred(ER(pars$par,ses), z)
  }
  res_sim_list <- mclapply(datr, pred_with_new, mc.cores = 8)
  
  res_sim <- as.data.frame(NULL)
  for(i in 1:length(res_sim_list)) {
    res_sim <- rbind(res_sim, res_sim_list[[i]])
  }
  
  mns <- as.vector(apply(res_sim, 2, function(x) mean(x, na.rm=T)))
  lws <- as.vector(apply(res_sim, 2, function(x) quantile(x, probs=0.025, na.rm=T)))
  his <- as.vector(apply(res_sim, 2, function(x) quantile(x, probs=0.975, na.rm=T)))
  
  dat[row.names(calc_dat),'y_mn'] <- mns
  dat[row.names(calc_dat),'y_lo'] <- lws
  dat[row.names(calc_dat),'y_hi'] <- his
  
  return(dat)
}


goose_plot_pred <- function(dat, year_start = 1987, ylim = c(10000, 60000),
                            plot = TRUE, resamp = TRUE, prev_params = prev_params) {

    ### goose_plot_pred()
    ###
    ### Called by goose_gmse_popmod, produces population prediction.
    ### - Calls get_goose_paras() to obtain optimised parameter estimates
    ### - Calls goose_pred() to obtain population prediction using optimised parameter estimates
    ### - May produce a plot of predictions if requested (plot= argument)
    ### - Returns vector of population predictions (Npred)  

    params <- get_goose_paras(dat, init_params=prev_params);
    prev_params <- params$par
    assign("prev_params", prev_params, envir = globalenv())
    
    if (resamp == FALSE) {
      Npred  <- goose_pred(para = params$par, dat = dat);    ## POINT PREDICTION ONLY 
      
      Npred_lo <- NA
      Npred_hi <- NA
      Npred_mn <- NA
      
      
    } else {
      
      data_sims <- res_sim(params, dat=dat, past=past)       ## POINT PREDICTION WITH UPPER/LOWER
      
      Npred <- data_sims$y_mn                                ## SO Npred is y_mn; which is the GMSE PROJECTION?
      
      Npred_lo <- data_sims$y_lo[length(data_sims$y_lo)]
      Npred_hi <- data_sims$y_hi[length(data_sims$y_hi)]
      Npred_mn <- data_sims$y_mn[length(data_sims$y_mn)]

    }
    
    assign("Npred_lo", Npred_lo, envir = globalenv())
    assign("Npred_hi", Npred_hi, envir = globalenv())
    assign("Npred_mn", Npred_mn, envir = globalenv())
    
  
    if(plot == TRUE){      ## DEFAULT == FALSE
        yrs    <- year_start:(year_start + length(dat$y) - 1);
        par(mar = c(5, 5, 1, 1));
        plot(x =  yrs, y = dat$y, pch = 1, ylim = ylim, cex.lab = 1.5,
             xlab="Year", ylab="Population size")         # Observed time series
        points(x = yrs, y = Npred, pch = 19, col = "red") # Predict time series
        oend <- length(dat$y);
        points(x = yrs[3:oend], y = dat$y[2:(oend - 1)], pch = 19,
               col = "blue");
    }
    return(Npred);
}

# goose_predict_and_plot <- function(file, plot = TRUE){
#     dat    <- read.csv(file);
#     data   <- goose_clean_data(file);
#     goosep <- goose_plot_pred(data = data, plot = plot);
#     return(goosep);
# }

goose_gmse_popmod <- function(dat){
  
    ### goose_gmse_popmod()
    ###
    ### Population model for use in gmse().
    ### - Calls goose_plot_pred(), which in turns obtains optimised parameter estimates and a
    ###    population model prediction.
    ### - Returns a single new population projection for a following year.
  
    N_pred <- goose_plot_pred(dat = dat, plot = FALSE, resamp = resamp, prev_params = prev_params);
    N_pred <- floor(N_pred)
    N_last <- length(N_pred);
    New_N  <- as.numeric(N_pred[N_last]);

    if(New_N < 1){
        New_N <- 1;
        warning("Extinction has occurred");
    }
    return(New_N);                           ### New_N is the POINT ESTIMATE FOR GMSE
}

goose_gmse_obsmod <- function(resource_vector, obs_error, use_est){
  
    ### goose_gmse_obsmod()
    ###
    ### Observation model for gmse()
    ###  - Returns an observed population size given obs_error in the observation 
    ###  - use_est allows for return optimistic or pessimistic bounds.
    
    obs_err    <- rnorm(n = 1, mean = 0, sd = obs_error);    
    #params <- get_goose_paras(data = data);
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
  
    ### goose_gmse_manmod()
    ###
    ### Manager model for gmse()
    ###  - Simply returns the difference between the observed population size
    ###     and the population target (manage_target), or zero if this difference
    ###     is negative. This is the target cull number.
  
    manager_vector <- observation_vector - manage_target;
    if(manager_vector < 0){
        manager_vector <- 0;
    }
    return(manager_vector);
}

goose_gmse_usrmod <- function(manager_vector, max_HB){
  
    ### goose_gmse_usrmod()
    ###
    ### User model for gmse()
    ###  - Simply implements the manager_vector, or max_HB. I.e. the user does exactly 
    ###     what the manager says.
  
    user_vector <- manager_vector;
    if(user_vector > max_HB){
        user_vector <- max_HB;
    }
    return(user_vector);
}

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
    goose_data[nrow(goose_data),missing_env] <- apply(goose_data[-nrow(goose_data),missing_env],2,function(x) sample_trended(x))
    
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
    gmse_pop   <- gmse_results$resource_results
    # gmse_obs holds the GMSE observation results.
    gmse_obs   <- gmse_results$observation_results
    
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
    new_Year <- goose_data[rows, 'Year'] + 1
    
    # COUNT: New population count is the number projected by GMSE minus the number proposed culled 
    #  (starting point for population projection for following year):
    new_Count <-  gmse_pop - gmse_cul
    
    # ICELANDCULL: Set to NA as we are not explicitly separating numbers culled on either Iceland 
    #  or Greenland (See HB below).
    new_IcelandCull <- NA
    # ISLAYCULL: Number proposed culled on Islay in projected year./
    new_IslayCull  <- gmse_cul
    # GREENLANDCULL: Set to NA as we are not explicitly separating numbers culled on either Iceland 
    #  or Greenland.
    new_GreenlandCull  <- NA
    # Future AIG, IslayTemp, AugRain and AugTemp are sampled randomly from previous years' data:
    new_AIG <- sample_trended(goose_data[,'AIG'])
    new_IslayTemp <- sample_trended(goose_data[,'IslayTemp'])
    new_AugRain <- sample_trended(goose_data[,'AugRain'])
    new_AugTemp <- sample_trended(goose_data[,'AugTemp'])
    
    # y: In observed years this was count + no. culled on Islay, in projected years this is GMSE 
    #  projection MINUS numbers proposed culled on Islay (i.e. the number culled on Islay is assumed 
    #  to be exactly what was set as the target cull). This is the population number used as the 
    #  starting point for next year's projection.
    new_y <- gmse_pop - gmse_cul; 
    
    # Future AIG.sc and HB (total hunting bag on Iceland and Greenland) are sampled randomly from 
    #  previous years' data.
    new_AIG.sc <- sample_trended(goose_data[,'AIG.sc']);
    new_HB <- sample_trended(goose_data[,'HB']);
    
    # Fill in values for monthly counts (all NA for projections)
    new_Monthly <- rep(NA,5)
    
    new_r <- c(new_Year, 
               new_Monthly, 
               new_Count, 
               new_IcelandCull, 
               new_IslayCull, 
               new_GreenlandCull, 
               new_AIG, 
               new_IslayTemp, 
               new_AugRain, 
               new_AugTemp, 
               new_y, 
               new_AIG.sc, 
               new_HB,
               gmse_pop=gmse_pop)
      
    # Add new projected data to existing data, and return:
    new_dat   <- rbind(goose_data, new_r);
    return(new_dat);
}

##### Uncertainty simulations #####

### Functions for sampling error distributions

# Sample error around count
count_ER <- function(rep=1){
  x <- rnorm(rep,0,2846)
  x
}

# Sample Iceland/Greenland cull error
cull_ER <- function(rep=1){
  x <- rnorm(rep,0.044,0.015)
  x
}

### Function to generate new data using error resampling of y and HB
y_HB_ER <- function(dat) {
  dat$y <- dat$y+count_ER(nrow(dat))
  dat$HB <- dat$HB*cull_ER(nrow(dat))
  return(dat)
}

### Helper function to draw normal samples from given vectors of parameters and SEs
ER <- function(paras,ses) {
  c(rnorm(1,paras[1],ses[1]),
    rnorm(1,paras[2],ses[2]),
    rnorm(1,paras[3],ses[3]),
    rnorm(1,paras[4],ses[4]),
    rnorm(1,paras[5],ses[5]),
    rnorm(1,paras[6],ses[6])
  )
}


gmse_goose <- function(data_file, manage_target, max_HB, years, obs_error, 
                       use_est = "normal",
                       plot = FALSE){
  
    # -- Initialise ------------------------------------------------------------

    proj_yrs   <- years

    goose_data <- goose_clean_data(file = data_file)

    last_year  <- goose_data[dim(goose_data)[1], 1]
    
    # use_est    <- 0
    # if(use_est == "cautious"){
    #     use_est <- -1
    # }
    # if(use_est == "aggressive"){
    #     use_est <- 1
    # }
    # assign("goose_data", goose_data, envir = globalenv() )
    # assign("target", manage_target, envir = globalenv() )
    # assign("max_HB", max_HB, envir = globalenv() )
    # assign("obs_error", obs_error, envir = globalenv() )
    # assign("use_est", use_est, envir = globalenv() )
    
    # goose_data$Npred_mn <- NA
    # goose_data$Npred_lo <- NA
    # goose_data$Npred_hi <- NA
    
    print('before gmse_apply 1')

    print(goose_data)
    print(environment(goose_data))
    
    gmse_res   <- gmse_apply(res_mod = goose_gmse_popmod, 
                             obs_mod = goose_gmse_obsmod,
                             man_mod = goose_gmse_manmod,
                             use_mod = goose_gmse_usrmod,
                             dat = goose_data, obs_error = obs_error,
                             manage_target = manage_target, max_HB = max_HB,
                             use_est = 0, stakeholders = 1, 
                             get_res = "full")
    
    print('after gmse_apply 1')
    
    goose_data <- sim_goose_data(gmse_results = gmse_res$basic,
                                 goose_data = goose_data)
    
    goose_data$Npred_mn[nrow(goose_data)] <- Npred_mn-gmse_res$basic$user_results
    goose_data$Npred_lo[nrow(goose_data)] <- Npred_lo-gmse_res$basic$user_results
    goose_data$Npred_hi[nrow(goose_data)] <- Npred_hi-gmse_res$basic$user_results
    
    # Start 'while' loop
   
    while(years > 1){
      
      print(paste('while loop', years))
      
      if(goose_data$y[nrow(goose_data)]<1) {
        print('EXTINCTION')
        extinct <- TRUE
        goose_data$y[nrow(goose_data)] <- 0
        goose_data$Count[nrow(goose_data)] <- 0
        goose_data$Npred_mn[nrow(goose_data)] <- 0
        goose_data$Npred_lo[nrow(goose_data)] <- 0
        goose_data$Npred_hi[nrow(goose_data)] <- 0
      }
      
      if(extinct==FALSE) {
        
        goose_data$y[goose_data$y<0] <- 0
        goose_data$Npred_mn[goose_data$Npred_mn<0] <- 0
        #goose_data$Npred_lo[goose_data$Npred_lo<0] <- 0
        #goose_data$Npred_hi[goose_data$Npred_hi<0] <- 0
        
        gmse_res_new   <- gmse_apply(res_mod = goose_gmse_popmod, 
                                     obs_mod = goose_gmse_obsmod,
                                     man_mod = goose_gmse_manmod,
                                     use_mod = goose_gmse_usrmod,
                                     dat = goose_data,
                                     manage_target = manage_target, use_est = 0 ,
                                     max_HB = max_HB, obs_error = obs_error,
                                     stakeholders = 1, get_res = "full");
        
        gmse_res   <- gmse_res_new;
        
        goose_data <- sim_goose_data(gmse_results = gmse_res$basic, 
                                     goose_data = goose_data);
        
        goose_data$Npred_mn[nrow(goose_data)] <- Npred_mn-gmse_res$basic$user_results
        goose_data$Npred_lo[nrow(goose_data)] <- Npred_lo-gmse_res$basic$user_results
        goose_data$Npred_hi[nrow(goose_data)] <- Npred_hi-gmse_res$basic$user_results
      } else {
        cur_yr <- goose_data$Year[length(goose_data$Year)]
        rem_yrs <- (last_year+proj_yrs)-cur_yr
        
        adds <- data.frame(Year=(cur_yr+1):(cur_yr+rem_yrs),
                           November=NA,
                           December=NA,
                           January=NA,
                           February=NA,
                           March=NA,
                           Count=0,
                           IcelandCull=NA,
                           IslayCull=NA,
                           GreenlandCull=NA,
                           AIG=NA,
                           IslayTemp=NA,
                           AugRain=NA,
                           AugTemp=NA,
                           y=0,
                           AIG.sc=NA,
                           HB=NA,
                           Npred_mn=0,
                           Npred_lo=NA,
                           Npred_hi=NA
        )
        goose_data <- rbind(goose_data, adds)
        years <- 1
      }

      years <- years - 1
    }
    
    
    return(goose_data)
}


gmse_goose_multiplot <- function(data_file, proj_yrs, 
                                 obs_error = 1438.614, manage_target, 
                                 max_HB, iterations, 
                                 use_est = "normal"){
    
  goose_multidata <- NULL
  
  for(i in 1:iterations){
    
    goose_multidata[[i]] <- gmse_goose(data_file = data_file,
                                       obs_error = obs_error,
                                       years = proj_yrs,
                                       manage_target = manage_target,
                                       max_HB = max_HB, plot = FALSE,
                                       use_est = 0)
    }
    
    goose_data <- goose_multidata[[1]]
    dat        <- goose_data[-1,]
    last_year  <- dat[dim(dat)[1], 1]
    yrs        <- dat[,1]
    NN         <- dat[,10]
    HB         <- dat[,3]
    pry        <- (last_year - proj_yrs):last_year
    obsrvd     <- 1:(dim(dat)[1] - proj_yrs - 1)
    par(mar = c(5, 5, 1, 1))
    plot(x = yrs, y = NN, xlab = "Year", ylab = "Population size",
         cex = 1.25, pch = 20, type = "n", ylim = c(0, max(NN+20)), 
         cex.lab = 1.1, cex.axis = 1.1, lwd = 2)
    polygon(x = c(pry, 2*last_year, 2*last_year, rev(pry)), 
            y = c(rep(x = -10000, times = length(pry) + 1), 
                  rep(x = 2*max(NN), times = length(pry) + 1)), 
            col = "grey", border = NA)
    box()
    points(x = yrs[obsrvd], y = NN[obsrvd], cex = 1.25, pch = 20, type = "b")
    abline(h = manage_target, lwd = 0.8, lty = "dotted")
    text(x = dat[5,1], y = 50000, labels = "Observed", cex = 1.75)
    text(x = pry[length(pry)], y = 50000, labels = "Projected", cex = 1.75, pos = 2)
    
    for(i in 1:length(goose_multidata)){
      goose_data <- goose_multidata[[i]]
      dat <- goose_data[-1,]
      yrs <- dat[,1]
      NN  <- dat[,10]
      HB  <- dat[,3]
      pry <- (last_year):(yrs[length(yrs)]-2+20)
      points(x = yrs, y = NN, pch = 20, type = "l", lwd = 0.6)
    }
    
    dev.copy(png,file="mainPlot.png", width=800, height=800)
    dev.off()
    
    return(goose_multidata)
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


