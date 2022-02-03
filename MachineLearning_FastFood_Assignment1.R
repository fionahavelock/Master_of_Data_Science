#Machine Learning Assignment 1. Fast Food

#Install packages
install.packages("openintro")
install.packages("glmnet")
install.packages("pls")
install.packages("gam")
library("openintro")
?fastfood

#We are going to be interested in a subset of this data. 
#Start by generating a new dataframe called ffood

fries <- fastfood[,-c(1,2,4,7,17)] #restaurant, item, cal_fat, trans_fat, salad
burger <- which( rowSums( is.na( fries ) ) > 0 )
ffood <- fries[-burger,]
#Check there are no missing values
sum( is.na( ffood ) )

View(fastfood)
View(ffood)
summary(fastfood)
summary(ffood)

#Produce a 6 by 6 frame of scatter plots of the response variable calories, 
#and the predictors total_fat, sat_fat, cholesterol, fiber and sugar

pairs( ffood[, c("calories", "total_fat", "sat_fat", "cholesterol", "fiber", "sugar")] )

#Perform the following methods for predicting calories, 
#taking all of the other variables in ffood as possible predictors;
# - lasso regression with min-CV lambda and 20 folds.
# - backward subset selection with Cp.

#################
#Lasso Regression
#################

library(glmnet)
#In this part again we call the response y 
#and extract the matrix of predictors, 
#which we call x using the command model.matrix().
#lasso regression with min-CV lambda and 20 folds.
y = ffood$calories
x = model.matrix(calories~., ffood)[,-1] # Here we exclude the first column 

set.seed(1)
lasso.cv = cv.glmnet(x, y)
lasso.cv$lambda.min

coef(lasso.cv, s = 'lambda.min')

##################################
#backward subset selection with Cp.
##################################
library(leaps)
best = regsubsets(calories~., data = ffood, nvmax = 11)
bwd = regsubsets(calories~., data = ffood, method = 'backward', nvmax = 11)
which.min(summary(best)$cp)
coef(bwd,4)
#total_fat cholesterol  total_carb  protein 


#Use 200 replications of data-splitting 
#with 230 samples for training to compare 
#the predictive performance of the following models 
#with the remaining possible predictors for calories.
###############################
#Principal component regression
###############################
library("pls")
pcr.fit = pcr(calories~., data=ffood, scale = TRUE, validation = "CV" )
summary(pcr.fit)
validationplot( pcr.fit, val.type = 'MSEP' )

min.pcr = which.min(MSEP(pcr.fit)$val[1,1, ]) -1
min.pcr

#######################
#Predictive Performance
######################
# Load R libraries.
library(leaps)

# Predict function for regsubsets
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

repetitions = 200
cor.bss = c()
cor.ridge = c()
cor.pcr = c()

set.seed(3)                
for(i in 1:repetitions){
  # Step (i) data splitting
  training.obs = sample(1:301,  230)
  y.train = ffood$calories[training.obs]
  x.train = model.matrix(calories~., ffood[training.obs, ])[,-1]
  y.test = ffood$calories[-training.obs]
  x.test = model.matrix(calories~., ffood[-training.obs, ])[,-1]
  
  # Step (ii) training phase
  bss.train = regsubsets(calories~., data=ffood[training.obs,], nvmax=8)
  min.cp = which.min(summary(bss.train)$cp)
  ridge.train = cv.glmnet(x.train, y.train, alpha = 0, nfolds = 5)
  pcr.train = pcr(calories~., data =ffood[training.obs,], 
                  scale = TRUE, validation="CV")
  min.pcr = which.min(MSEP(pcr.train)$val[1,1, ] ) - 1
  
  # Step (iii) generating predictions
  predict.bss = predict.regsubsets(bss.train, ffood[-training.obs, ], min.cp)
  predict.ridge = predict(ridge.train, x.test, s = 'lambda.min')
  predict.pcr = predict(pcr.train,ffood[-training.obs, ], ncomp = min.pcr )
  
  # Step (iv) evaluating predictive performance
  cor.bss[i] = cor(y.test, predict.bss)
  cor.ridge[i] = cor(y.test, predict.ridge)
  cor.pcr[i] = cor(y.test, predict.pcr)
}

# Plot the resulting correlations as boxplots.
boxplot(cor.bss, cor.ridge, cor.pcr, 
        names = c('BSS','Ridge', 'PCR'), 
        ylab = 'Test correlation', col = 2:5)

########################
#Best Histogram
########################
min.valid = c()

for(n in 1:200){
  set.seed(n)          
  training.obs = sample(1:301, 230)
  ffood.train = ffood[training.obs,  ]
  ffood.test = ffood[-training.obs,  ]
  
  best = regsubsets(calories~., data = ffood.train, nvmax = 11)
  
  val.error<-c()
  for(i in 1:11){
    pred = predict.regsubsets(best, ffood.test, i)
    val.error[i] = mean((ffood.test$calories - pred)^2)
  }
  val.error
  min.valid[n] = which.min(val.error)
}

hist(min.valid, col = 3, breaks = seq( from = 0.5, to = 14.5, length = 15 ), 
     xlab = 'Number of Predictors', main = 'BSS with validation')
abline(v = mean(min.valid), col = 2, lwd = 4)
legend('topright', legend=c('Average selection'),bty = 'n', lty = 1, 
       lwd = 4, col = 2)


#################################################
#Fit a Generalised Additive Model for calories that consists of three terms:
#a smoothing spline with 6 degrees of freedom for total_fat,
#a natural spline with 3 degrees of freedom for cholesterol, and
#a simple linear model term for vit_a.
######################################

library(gam)
# Fit a GAM.
gam = gam( calories ~ ns( cholesterol, df = 3 ) + s( total_fat, df = 6 ) + vit_a, 
           data = ffood )

# Plot the contributions.
par( mfrow = c(2,3) )
plot( gam,  se = TRUE, col = "blue" )

# Compare with the following plots.
plot( ffood$cholesterol, ffood$calories, pch = 16, col = 2, 
      ylab ="calories", xlab = "cholesterol" )

plot( ffood$total_fat, ffood$calories, pch = 16, col = 2, 
      ylab = "calories", xlab = "total fat" )

plot( ffood$vit_a, ffood$calories, pch = 16, col = 2, 
      ylab = "calories", xlab = "vitamin a" )



