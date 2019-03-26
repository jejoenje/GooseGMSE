rm(list=ls())

source('goose_predict_gui.R')

input <- list(input_name=data.frame(
  datapath=as.vector('/home/jm241/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/Islay_Stirling_2017.xls')),
  sims_in=5, yrs_in=1, maxHB_in=2000, target_in=32000)
input$input_name$datapath <- as.vector(input$input_name$datapath)
iterations <- input$sims_in
years <- input$yrs_in
proj_yrs <- years
manage_target <- input$target_in
max_HB <- input$maxHB_in
data_file <- as.vector(input$input_name$datapath)
obs_error = 1438.614
plot = TRUE

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
text(x = dat[5,1], y = 50000, labels = "Observed", cex = 1.75);
text(x = pry[length(pry)], y = 50000, labels = "Projected", cex = 1.75, pos = 2);

for(i in 1:length(goose_multidata)){
  goose_data <- goose_multidata[[i]];
  dat <- goose_data[-1,];
  yrs <- dat[,1];
  NN  <- dat[,10];
  HB  <- dat[,3];
  pry <- (last_year):(yrs[length(yrs)]-2+20);
  points(x = yrs, y = NN, pch = 20, type = "l", lwd = 0.6);
}

x <- goose_multidata[[1]][31:34,'Count']

goose_multidata



paras <- optim(par = init_params, fn = goose_growth, data = goose_data,
                        method = "BFGS", control = contr_paras, 
                        hessian = TRUE);
par <- paras$par
par_se <- sqrt(diag(0.5*solve(-paras$hessian)))


