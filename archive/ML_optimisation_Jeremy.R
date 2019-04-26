### J. Cusack's attempt at resolving the optimisation and SE derivation problem, based on Jeroen's work

rm(list=ls())

##### Required packages #####

library(HelpersMG)

##### Useful functions #####

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

##### Import data #####

# setwd("/Users/jc130/Dropbox/SNH report code work")        # NEEDS TO BE CHANGED

data <- read.csv("example_data_UPDATED.csv")              # This is the same dataset, just with a different name

##### Rescale AIG function (unmodified) #####

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

##### Clean data function #####
# MODIFICATIONS
# 1) load_input function not used
# 2) following lines not implemented:
     #>data$HB[data$HB==0] <- NA
     #>data$IcelandCull[data$IcelandCull==0] <- NA
     #>data$GreenlandCull[data$GreenlandCull==0] <- NA

goose_clean_data <- function(file){
  
  ### goose_clean_data()
  ### 
  ### Single argument, which is a character string of the input file to be loaded.
  ### - Calls load_input()
  ### - Calculates 'working' data, i.e. total number of geese, scales/standardises 
  ###    input variables for model fit.
  ### - Calculates total number culled on Iceland and Greenland as variable HB, 
  ###    ignoring any NA's.
  
  #data   <- load_input(file);              # Load dataset
  
  # data$y is the observed count plus the number culled on Islay:
  data$y <- data$Count+data$IslayCull
  
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

##### Clean data #####

data <- goose_clean_data(data) ###

##### Goose growth function (maximising Poisson likelihood) #####

goose_growth <- function(para, data) {
  data_rows <- dim(data)[1];
  N_pred <- goose_pred(para=para, data = data);
  return(-sum(dpois(data$y, N_pred, log=T),na.rm=T))
}

##### Different parameterisation of goose_pred() to allow for ML #####

goose_pred <- function(para, data){  
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
  data_rows <- nrow(data)
  N_pred    <- rep(NA, data_rows)
  
  # Starting at year 3 (we need data from at least 2 years ago), run through each input line.
  for(time in 3:data_rows){
    # Reproduction rate is max growth rate times previous years' population size
    goose_repr   <- r_val * data$y[time - 1];
    # Goose density/carrying capacity term is function of numbers in previous year, K and AIG
    goose_dens   <- 1 - (data$y[time -1] / (K_val * data$AIG[time - 1]));
    # 'Current' population (previous year)
    goose_now    <- data$y[time - 1];
    # Effect of rainfall in Greenland in previous year
    G_rain_adj   <- G_rain_coeff * data$AugRain[time - 1];
    # Effect of temperature of Greenland in previous year
    G_temp_adj   <- G_temp_coeff * data$AugTemp[time - 1];
    # Effect of temperature on Islay in previous year
    I_temp_adj   <- I_temp_coeff * data$IslayTemp[time - 1];
    # Effect of AIG on Islay in the year before last
    AIG_2_adj    <- AIG_2_yrs    * data$AIG.sc[time - 2];
    # Sum the combined effect of above effects for convenience
    adjusted     <- G_rain_adj + G_temp_adj + I_temp_adj + AIG_2_adj
    # Next years' population size is reproduction rate times the density term, adjusted by 
    #  environmental effects, minus the number taken on Greenland and Iceland, which is taken 
    #  as the mean of the previous years' take there.
    N_pred[time] <- goose_repr * (goose_dens + adjusted) + goose_now - mean(data$HB/data$y, na.rm=T)    
    
    ### So, the prediction N_pred[time] here is the projected population size on Islay AFTER 
    ###   culling on G'land and Iceland, but EXCLUDING anything 'to be' culled on Islay at [time].
    ### data$HB for the input file is the sum of the numbers culled on G'land and Iceland (treating 
    ###  an NA in one or the other as a zero but keeps NA if both values are NA.
    ### By substracting mean(data$HB) here, the number removed due to culling in G'land and Iceland 
    ###  becomes a 'running mean' (i.e. changed as new data become available) and will be sampled 
    ###  from randomly for future projections.
  }
  
  return(N_pred);
}

##### Get parameters function (based on goose_growth2) #####

get_goose_paras <- function(data, init_params = NULL){
  
  ### get_goose_paras()
  ###
  ### Runs optimisation for population model parameters.
  ### Takes goose_data and initial parameters as input (latter not required)
  
  # Set init parameters if not provided
  if( is.null(init_params) == TRUE ){
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
  get_parameters <- optim(par = init_params, fn = goose_growth, data = data,
                          hessian = TRUE);
  
  # Updates progress bar when running as Shiny app
  if(exists("progress_i")) {
    progress_i <- progress_i+1
    assign("progress_i", progress_i, envir = globalenv())
    progress$set(value = progress_i)
  }
  
  # Return optimised parameters
  return(get_parameters);
}

##### Get parameters and Hessian matrix #####

ggp <- get_goose_paras(data)
Params <- ggp$par
hess <- ggp$hessian

##### Derive SEs (uses the SEfromHessian function in package HelperMG) #####

SEs <- SEfromHessian(ggp$hessian)

##### Get predictions function #####

goose_plot_pred <- function(data, year_start = 1987, ylim = c(10000, 60000),
                            plot = TRUE){
  
  ### goose_plot_pred()
  ###
  ### Called by goose_gmse_popmod, produces population prediction.
  ### - Calls get_goose_paras() to obtain optimised parameter estimates
  ### - Calls goose_pred() to obtain population prediction using optimised parameter estimates
  ### - May produce a plot of predictions if requested (plot= argument)
  ### - Returns vector of population predictions (Npred)  
  
  
  params <- get_goose_paras(data = data);
  
  Npred  <- goose_pred(para = params$par,
                        data = data);
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

##### Get predictions #####

res <- goose_plot_pred(data=data, plot=F)

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


ER <- function(paras,ses) {
  c(rnorm(1,paras[1],ses[1]),
    rnorm(1,paras[2],ses[2]),
    rnorm(1,paras[3],ses[3]),
    rnorm(1,paras[4],ses[4]),
    rnorm(1,paras[5],ses[5]),
    rnorm(1,paras[6],ses[6])
    )
}


### Implement simulations

reps=10000

res_sim <- matrix(nrow=reps,ncol=nrow(data))

datar <- list(y_HB_ER(data))
for (r in 1:(reps)){
  datar[[length(datar)+1]] <- y_HB_ER(data)
}
datar[[length(datar)]] <- NULL

pred_with_new <- function(z) {
  goose_pred(ER(Params,SEs), z)
}
res_sim_list <- lapply(datar, pred_with_new)

res_sim <- as.data.frame(NULL)
for(i in 1:length(res_sim_list)) {
  res_sim <- rbind(res_sim, res_sim_list[[i]])
}



### Plot

quartz()
par(pty="s")
plot(c(data$y/1000)~data$Year,type="l",lty=2,ylim=c(15,55),xlab="Year",ylab="Estimated population size (x1000)",cex.lab=1.2,cex.axis=1.2)
points(c(data$y/1000)~data$Year)
lines(c(res/1000)~data$Year,lty=2,col="red")
points(c(res/1000)~data$Year,pch=21,bg="red")
ucl <- apply(res_sim,2,function(x){quantile(x,probs=0.975,na.rm=T)})
lcl <- apply(res_sim,2,function(x){quantile(x,probs=0.025,na.rm=T)})
arrows(data$Year,c(data$y/1000),data$Year,lcl/1000,length=0)
arrows(data$Year,c(data$y/1000),data$Year,ucl/1000,length=0)
legend("topleft",legend=c("Observed","Logisitc model predictions"),lty=2,col=c("black","red"),bty="n")




