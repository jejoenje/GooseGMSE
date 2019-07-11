### Code to produce Figures 4 and 5 in the SNH report ###

rm(list=ls())

##### Set working directory #####

setwd("/Users/jeremycusack/Dropbox/SNH report code work/Latest revision")

##### Load packages #####

library(GMSE)
library(tools)
library(readxl)
library(HelpersMG)
library(parallel)
library(foreach)
library(scales)
library(maxLik)

##### Functions (unnecessary ones have been removed) #####

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

goose_clean_data <- function(data){
  
  ### goose_clean_data()
  ### 
  ### Single argument, which is a character string of the input file to be loaded.
  ### - Calls load_input()
  ### - Calculates 'working' data, i.e. total number of geese, scales/standardises 
  ###    input variables for model fit.
  ### - Calculates total number culled on Iceland and Greenland as variable HB, 
  ###    ignoring any NA's.
  
  #data   <- load_input(file);              # Load dataset
  
  av.cull.per.month <- round(data$IslayCull/5)
  
  # Add to count
  monthly.count.plu.av.cull <- data[1:nrow(data),c("November","December","January","February","March")]+av.cull.per.month
  
  # data$y is the observed count plus the number culled on Islay:
  #data$y <- data$Count+data$IslayCull
  data$y <- round(apply(monthly.count.plu.av.cull,1,mean,na.rm=T))
  
  # Rescale AIG ensuring consistency in measurementS:
  data   <- goose_rescale_AIG(data = data, years = 22);
  
  # Scale/standardise environmental variables:
  #data$AugTemp   <- as.numeric( scale(data$AugTemp) )
  #data$IslayTemp <- as.numeric( scale(data$IslayTemp) )
  #data$AugRain   <- as.numeric( scale(data$AugRain) )
  #data$AIG.sc    <- as.numeric( scale(data$AIG) )
  
  # The following ensures that if either G'land or Iceland culls were unavailable 
  #  (NA) but the other was, the missing number is treated as zero, i.e. the total 
  #  culled on G'land and Icelan#d (HB) is always at least the number available for 
  #  one of them (i.e. avoid NA's in a sum including NA's):
  
  #data$IcelandCull[is.na(data$IcelandCull)] <- 0
  #data$GreenlandCull[is.na(data$GreenlandCull)] <- 0
  data$HB <- data$IcelandCull+data$GreenlandCull
  data$HB[data$HB==0] <- NA
  #data$IcelandCull[data$IcelandCull==0] <- NA
  #data$GreenlandCull[data$GreenlandCull==0] <- NA
  
  return(data);
} 

goose_growth <- function(para, dat) {
  data_rows <- dim(dat)[1];
  N_pred <- goose_pred(para = para, dat = dat);
  return(-sum(dpois(dat$y, N_pred, log=T),na.rm=T))
}

goose_pred <- function(para, dat, k0=673, e=0.044){  
  ### goose_pred()
  ### 
  ### MAIN POPULATION MODEL PREDICTION FUNCTION
  ###
  ### Takes optimised parameters and data as input
  ###  - Extract input parameters from para vector.
  ###  - Produce a prediction for each line in the data, given environmental variables.
  ###  - Returns vector of predictions.
  
  r_val        <- para[1]              # Maximum growth rate
  K_z          <- k0
  K_val        <- para[2]              # Carrying capacity
  G_rain_coeff <- para[3]              # Effect of precipitation on Greenland in August
  G_temp_coeff <- para[4]              # Effect of temperature on Greenland in August
  I_temp_coeff <- para[5]              # Effect of temperature on Islay the previous winter
  AIG_2_yrs    <- para[6]              # Effect of area of improved grassland 2 years prior
  AddCull      <- e
  
  # Make as many predictions as there are lines in data:
  data_rows <- length(dat[,1])
  N_pred    <- rep(NA, data_rows)
  
  # Starting at year 3 (we need data from at least 2 years ago), run through each input line.
  for(time in 3:data_rows) {
    # 'Current' population (previous year)
    goose_now    <- dat$y[time - 1];
    # Reproduction rate is max growth rate times previous years' population size
    goose_repr   <- r_val * goose_now;
    # Effect of rainfall in Greenland in previous year
    G_rain_adj   <- G_rain_coeff * dat$AugRain[time - 1];
    # Effect of temperature of Greenland in previous year
    G_temp_adj   <- G_temp_coeff * dat$AugTemp[time - 1];
    # Effect of temperature on Islay in previous year
    I_temp_adj   <- I_temp_coeff * dat$IslayTemp[time - 1];
    # Effect of AIG on Islay in the year before last
    AIG_2_adj    <- AIG_2_yrs    * dat$AIG[time - 2];
    # Sum the combined effect of above effects for convenience
    adjusted     <- G_rain_adj + G_temp_adj + I_temp_adj + AIG_2_adj;
    # Goose density/carrying capacity term is function of numbers in previous year, K and AIG
    goose_dens   <- 1 - (goose_now / (K_z + K_val * dat$AIG[time - 1]));
    # Next years' population size is reproduction rate times the density term, adjusted by 
    #  environmental effects, minus the number taken on Greenland and Iceland, which is taken 
    #  as the mean of the previous years' take there.
    N_pred[time] <- ((goose_repr * adjusted * goose_dens) + goose_now) - AddCull*goose_now    
    
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
    init_params    <- c(0.128,6,0,0,0,0);                
  }
  
  # Run optimisation routine, using the goose_growth() function, goose_data and init_params
  ## PREVIOUS:
  # Set control parameters for optim() function:
  # contr_paras    <- list(trace = 1, fnscale = -1, maxit = 1000, factr = 1e-8,
  #                      pgtol = 0);
  # get_parameters <- optim(par = init_params, fn = goose_growth, dat = dat,
  #                         method = "BFGS", control = contr_paras, 
  #                         hessian = TRUE);
  
  ## NEW ATTEMPT (LL)
  ## Note I'm currently not using any of the control pars; didn't seem to be necessary for now...
  dat$y <- floor(dat$y)
  #dat$y[dat$y<0] <- 0
  contr_paras    <- list(maxit = 500);
  # get_parameters <- optim(par = init_params, fn = goose_growth, dat = dat, method='L-BFGS-B', control = contr_paras,
  #                         hessian = TRUE);
  
  get_parameters <- optim(par = init_params, fn = goose_growth, dat = dat, 
                          hessian = TRUE);
  
  #ophess <- optimHess(par = init_params, fn = goose_growth, dat=dat)
  
  #get_parameters <- maxLik(logLik=goose_growth2,start=init_params,dat=dat)
  
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

goose_plot_pred <- function(data, year_start = 1987, ylim = c(20000, 50000),
                             plot = TRUE, para){
  
  ### goose_plot_pred()
  ###
  ### Called by goose_gmse_popmod, produces population prediction.
  ### - Calls get_goose_paras() to obtain optimised parameter estimates
  ### - Calls goose_pred() to obtain population prediction using optimised parameter estimates
  ### - May produce a plot of predictions if requested (plot= argument)
  ### - Returns vector of population predictions (Npred)  
  
  
  params <- get_goose_paras(dat = data);
  
  Npred  <- goose_pred(para=para,
                       dat = data)/1000;
  yrs    <- year_start:(year_start + length(data$y) - 1);
  if(plot == TRUE){
    quartz()
    par(mar = c(5, 5, 1, 1),pty="s");
    plot(x =  yrs, y = data$y/1000, pch = 1, ylim = ylim/1000, cex.lab = 1.5,
         xlab="Year", ylab="Population size (x1000)")         # Observed time series
    points(x = yrs, y = Npred, pch = 19, col = "red") # Predict time series
    oend <- length(data$y);
    points(x = yrs[3:oend], y = data$y[2:(oend - 1)]/1000, pch = 19,
           col = "blue");
    lines(x =  yrs, y = data$y/1000,lty=2)
    lines(x = yrs, y = Npred,lty=2,col="red")
    lines(x = yrs[3:oend], y = data$y[2:(oend - 1)]/1000,lty=2,col="blue")
    legend("topleft",legend=c("Observed","Logistic Model Predictions","Previous Year Count"),
           lty=2,col=c("black","red","blue"),bty="n")
  }
  return(Npred);
}

count_ER <- function(rep=1){
  x <- rnorm(rep,0,(2846/sqrt(5)))
  x
}

cull_ER <- function(rep=1){
  x <- rnorm(rep,0.044,0.015)
  x[x<0] <- 0
  return(x)
}

k0_ER <- function(){
  x <- rpois(1,673)
  x
}

gr_ER <- function(){
  x <- rnorm(1,Params[1],SEs[1])
  x
}

k_ER <- function(){
  x <- rnorm(1,Params[2],SEs[2])
  x
}

a_ER <- function(){
  x <- rnorm(1,Params[3],SEs[3])
  x
}

b_ER <- function(){
  x <- rnorm(1,Params[4],SEs[4])
  x
}

c_ER <- function(){
  x <- rnorm(1,Params[5],SEs[5])
  x
}

d_ER <- function(){
  x <- rnorm(1,Params[6],SEs[6])
  x
}

##### FIGURE 4 #####

DATA <- read.csv("Standardised_dataset_IslayGBG.csv") # Import data

data <- goose_clean_data(data=DATA) # Clean data

paras_ori <- get_goose_paras(dat=data) # Optimise parameter values

res <- goose_plot_pred(data=data,para=paras_ori$par)

##### FIGURE 5 #####

Params <- paras_ori$par # Extract parameters        

SEs <- SEfromHessian(paras_ori$hessian) # Extract SEs

reps=1000 # Number of iterations to produce errors

res_sim <- matrix(nrow=reps,ncol=29) # Object to collect results

for (r in 1:reps){
  
  datar <- data
  for (i in 1:nrow(datar)){
    datar[i,"y"] <- datar[i,"y"]+count_ER()
  }
  
  r.ER <- gr_ER()
  k0.ER <- k0_ER()
  k.ER <- k_ER()
  a.ER <- a_ER()
  b.ER <- b_ER()
  c.ER <- c_ER()
  d.ER <- d_ER()
  cull.ER <- cull_ER()
  
  res_sim[r,] <- goose_pred(para=c(r.ER,k.ER,a.ER,b.ER,c.ER,d.ER),dat=datar,k0=k0.ER,e=cull.ER)
  
  print(r)
  
} # Sample from distributions to produce conf intervals

quartz()
par(pty="s")
plot(c(data$y/1000)~data$Year,type="l",lty=2,ylim=c(15,55),xlab="Year",ylab="Estimated population size (x1000)",cex.lab=1.2,cex.axis=1.2)
points(c(data$y/1000)~data$Year)
lines(c(res)~data$Year,lty=2,col="red")
points(c(res)~data$Year,pch=21,bg="red")
ucl <- apply(res_sim,2,function(x){quantile(x,probs=0.975,na.rm=T)})
lcl <- apply(res_sim,2,function(x){quantile(x,probs=0.025,na.rm=T)})
arrows(data$Year,c(data$y/1000),data$Year,lcl/1000,length=0)
arrows(data$Year,c(data$y/1000),data$Year,ucl/1000,length=0)
legend("topleft",legend=c("Observed","Logisitc model predictions"),lty=2,col=c("black","red"),bty="n")
