source('goose_predict_gui.R')

goose_data <- goose_clean_data('~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/example_data_UPDATED_April2019.csv')

plot(goose_data$AIG, goose_data$Count, xlim=c(0,9000), ylim=c(0,55000))

goose_data$AIG2 <- goose_data$AIG^2

### Simple models relating counts to to AIG^2 (i.e avoid a purely linear relationship)

### First model is with counts modelled as Normally distributed (i.e. not right but gets ~3000 at intercept)

m1 <- glm(Count ~ AIG2, data=goose_data, family='gaussian')
summary(m1)

xval1 <- seq(0,max(goose_data$AIG),100)
xval2 <- xval1^2
ypred1 <- as.vector(predict(m1, newdata = data.frame(AIG2=xval2), type='response') )
lines(xval1, ypred1)

### Second attempt uses Poisson distribution for the same model:

m2 <- update(m1, .~., family='poisson')
ypred2 <- as.vector(predict(m2, newdata = data.frame(AIG2=xval2), type='response') )
lines(xval1, ypred2, col='red')

### Third attempt adds a linear predictor for AIG in addition to AIG^2

m3 <- update(m2, .~. +AIG)
ypred3 <- as.vector(predict(m3, newdata = data.frame(AIG2=xval2, AIG=xval1), type='response') )
lines(xval1, ypred3, col='blue')

### Predicted value at intercept: 

# Gaussian model (AIG^2 only)
coef(m1)[1]
# Poisson model (AIG^2 only)
exp(coef(m2)[1])
# Poisson model (AIG+AIG^2)
exp(coef(m3)[1])


