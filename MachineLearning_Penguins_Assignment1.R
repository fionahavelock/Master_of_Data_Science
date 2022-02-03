#Assignment 1, Machine Learning. Penguins

#install packages
install.packages("palmerpenguins")
install.packages("splines")
library("splines")
library("palmerpenguins")
?penguins
View(penguins)

#We create a new dataset called `palmer` that removes any missing data.
polar_bear <- which( rowSums( is.na( penguins ) ) > 0 )
palmer <- penguins[-polar_bear,]
dim(palmer)


#We seek to predict penguin body mass from flipper length. Thus we run the code:
y = palmer$body_mass_g
x = palmer$flipper_length_mm
y.lab = "Body mass"
x.lab = "Flipper Length"
#such that now we are interested in predicting y from x.
#What are the 25th, 50th and 75th percentiles of x?
quantile(x, c(.25, .50, .75))
summary(x)

#linear spline model for penguin mass as a function of flipper length, 
#taking knots at the lower quartile, median and upper quartile
#we specify degree = 1 for a linear spline
#response is y and predictor is x
#y is a function of x" (denoted y = y(x)) means that y varies according to whatever value x takes on
#spline1 = lm( y ~ bs (x, degree =1, knots = summmary (x) [c(2, 3, 5)]))

#modelling y based on x, Fit a first, second, third, fourth 
#and fifth order polynomial to the data using the commands lm and poly
plot( x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
      main = "", bty = 'l' )

#Fit a first, second, third, fourth and fifth order polynomial to the data 
#using the commands lm and poly.

poly1 = lm(y ~ poly(x,  1))
poly2 = lm(y ~ poly(x,  2))
poly3 = lm(y ~ poly(x,  3))
poly4 = lm(y ~ poly(x,  4))
poly5 = lm(y ~ poly(x,  5))

# sorted values of x.
sort.x = sort(x) 

# Predicted values.
pred1 = predict(poly1, newdata = list(x = sort.x), se = TRUE)
pred2 = predict(poly2, newdata = list(x = sort.x), se = TRUE)
pred3 = predict(poly3, newdata = list(x = sort.x), se = TRUE)
pred4 = predict(poly4, newdata = list(x = sort.x), se = TRUE)
pred5 = predict(poly5, newdata = list(x = sort.x), se = TRUE)

# Confidence interval bands.
se.bands1 = cbind( pred1$fit - 2*pred1$se.fit, pred1$fit + 2*pred1$se.fit )
se.bands2 = cbind( pred2$fit - 2*pred2$se.fit, pred2$fit + 2*pred2$se.fit )
se.bands3 = cbind( pred3$fit - 2*pred3$se.fit, pred3$fit + 2*pred3$se.fit )
se.bands4 = cbind( pred4$fit - 2*pred4$se.fit, pred4$fit + 2*pred4$se.fit )
se.bands5 = cbind( pred5$fit - 2*pred5$se.fit, pred5$fit + 2*pred5$se.fit )

par(mfrow = c(2,3))

# Degree-1 polynomial plot.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-1 polynomial", bty = 'l')
lines(sort.x, pred1$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands1, lwd = 2, col = "red", lty = 3)

# Degree-2 polynomial plot.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-2 polynomial", bty = 'l')
lines(sort.x, pred2$fit, lwd = 2, col = "blue")
matlines(sort.x, se.bands2, lwd = 2, col = "blue", lty = 3)

# Degree-3 polynomial plot.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-3 polynomial", bty = 'l')
lines(sort.x, pred3$fit, lwd = 2, col = "gold")
matlines(sort.x, se.bands3, lwd = 2, col = "gold", lty = 3)

# Degree-4 polynomial plot.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-4 polynomial", bty = 'l')
lines(sort.x, pred4$fit, lwd = 2, col = "purple")
matlines(sort.x, se.bands4, lwd = 2, col = "purple", lty = 3)

# Degree-5 polynomial plot.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-5 polynomial", bty = 'l')
lines(sort.x, pred5$fit, lwd = 2, col = "black")
matlines(sort.x, se.bands5, lwd = 2, col = "black", lty = 3)

#Perform an analysis of variance to confirm 
#whether higher order degrees of polynomial are 
#useful for modelling y based on x. 
anova(poly1, poly2, poly3, poly4, poly5)
##################
#Console response:
#4    328 46125249  1    422040  2.9920  0.084619 .
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

###############################

#step for Which command would effectively build a step function 
#regression model with 7 cut-points for y based on x?
# Remember to define the number of intervals (one more than the number of 
# cut-points).
step8 = lm(y ~ cut(x, 8))

pred8 = predict(step8, newdata = list(x = sort(x)), se = TRUE)
se.bands8 = cbind(pred8$fit + 2*pred8$se.fit, pred8$fit-2*pred8$se.fit)

plot(x, y, cex.lab = 1.1, col="darkgrey", bty = 'l')

#lines(sort(x), pred6$fit, lwd = 2, col = "red")
lines(x = sort(x), y = pred8$fit, lwd = 2, col = "red")
matlines(sort(x), se.bands8, lwd = 1.4, col = "red", lty = 3)
table(cut(x, 2))


###############
#Last section of the practical
summary(step8)
newx <- seq(from = min(x), to = max(x), length = 100)
pred8 = predict(step8, newdata = list(x = newx), se = TRUE)
se.bands8 = cbind(pred8$fit + 2*pred8$se.fit, pred8$fit-2*pred8$se.fit)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "7 cutpoints", bty = 'l')

lines(newx, pred8$fit, lwd = 2, col = "red")
matlines(newx, se.bands8, lwd = 1.4, col = "red", lty = 3)
