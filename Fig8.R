rm(list=ls())

source('goose_predict_gui.R')

library(scales)

hb_range <- seq(0, 20000, 1000)

input <- list(input_name=data.frame(
  datapath=as.vector('~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/example_data_UPDATED_April2019.csv')),
  sims_in=5, yrs_in=3, maxHB_in=hb_range[1], target_in=29000)
input$input_name$datapath <- as.vector(input$input_name$datapath)
iterations <- input$sims_in
years <- input$yrs_in
proj_yrs <- years
manage_target <- input$target_in
max_HB <- input$maxHB_in
data_file <- as.vector(input$input_name$datapath)
obs_error = 1438.614

plot = TRUE
past = FALSE

resamp = TRUE

extinct = FALSE  

prev_params <- NULL

goose_multidata2 <- NULL


for (i in 1:length(hb_range)) {
  prev_params <- NULL
  goose_multidata <- NULL
    for(j in 1:iterations) {
    prev_params <- NULL
    goose_multidata[[j]] <- gmse_goose(data_file = data_file,
                                       obs_error = obs_error,
                                       years = proj_yrs,
                                       manage_target = manage_target,
                                       max_HB = hb_range[[i]], plot = FALSE,
                                       use_est = 0)
    
  }
  goose_multidata2[[i]] <- data.frame(Npred_mn = unlist(lapply(goose_multidata, function(x) tail(x$Npred_mn,1))), 
                                 Npred_lo = unlist(lapply(goose_multidata, function(x) tail(x$Npred_lo,1))),
                                 Npred_hi = unlist(lapply(goose_multidata, function(x) tail(x$Npred_hi,1))))
}

temp <- lapply(goose_multidata2, function(x) c(mean(x$Npred_mn), mean(x$Npred_lo), mean(x$Npred_hi)))
f8dat <- as.data.frame(matrix(unlist(temp), ncol=3, byrow=T))
names(f8dat) <- c('Npred_mn','Npred_lo','Npred_hi')
f8dat$hb <- hb_range

tiff('Figure8.tiff', width=600, height=600, pointsize=18)
plot(f8dat$hb, f8dat$Npred_mn, ylim=c(15000, 45000), type='n', xlab = 'Maximum cull level in year t', 
     ylab = 'Estimated population size at start of year t+1', xaxt='n')
polygon(c(f8dat$hb, rev(f8dat$hb)), c(f8dat$Npred_hi, rev(f8dat$Npred_lo)),
        col = "lightgrey", border = NA) 
lines(f8dat$hb, f8dat$Npred_mn, lwd=2)
axis(1, at= seq(0,20000,2500), labels = seq(0,20000,2500))

abline(h=goose_data[goose_data$Year=='2015','Count'],col='blue',lty='dashed',lwd=1)
abline(h=manage_target,col='red',lty='dashed',lwd=1)

coef(lm(Npred_lo ~ hb, data=f8dat))

cross_lo_hb <- (manage_target-as.numeric(coef(lm(Npred_lo ~ hb, data=f8dat))[1]))/as.numeric(coef(lm(Npred_lo ~ hb, data=f8dat))[2])

abline(v=cross_lo_hb, lty='dotted')
dev.off()
