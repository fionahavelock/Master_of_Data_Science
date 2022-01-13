data(infert)

#we take away the misfitting data, the article mentioned that one patient did not have two control matches infer is our new data set
infer <- infert[(infert$stratum != 74),]
dim(infer)


#less educated implies more likely to get induced abortion, however this does not result in infertility
library(ggplot2)
ggplot(infer, aes(x=education, y=induced))+ geom_boxplot(fill='#F0ADBC')

library(ggplot2)
ggplot(infer, aes(x=education, y=spontaneous))+ geom_boxplot(fill='#509491')

library(ggplot2)
ggplot(infer, aes(x=case, y=induced))+ geom_boxplot(fill='#E6FFFB')

library(ggplot2)
ggplot(infer, aes(x=case, y=spontaneous))+ geom_boxplot(fill='#EDCCBE')

library(ggplot2)
ggplot(infer, aes(x=case, y=parity))+ geom_boxplot(fill='#EDCCBE')

str(infer)
summary(infer)
#parity = number of previous preganancies
View(infer)
pairs(infer)

#we make the dependent variable a factor
infer$case = factor(infer$case)
levels(infer$case) = c('Control', 'infertile') 
summary(infer$case)

#here we see for each recorded case of infertility there are two control patients
#This is reflected in the stratum
infer$stratum = factor(infer$stratum)
infer$pooled.stratum = factor(infer$pooled.stratum)
summary(infer)
# use table to perform a baseline method for predicting the average outcome for all our data points
table(infer$case)

#separate training set and validation set
install.packages("caTools")
library(caTools)
#now randomly spilt the data into a training set and a testing set
set.seed(88)
split = sample.split(infer$case, SplitRatio = 0.50)
split

inferTrain = subset (infer, split == TRUE)
inferTest = subset (infer, split == FALSE)

nrow(inferTrain)
nrow(inferTest)

#########################################
#Model 1
#########################################

infertilityLog = glm(case ~ induced + spontaneous + parity + education, data = inferTrain, family = binomial())
summary(infertilityLog)

plot(infertilityLog)


#check for colinearity
cor(inferTrain[c("induced", "spontaneous","parity")])

# look at Preliminary Correlations
cor.test(as.numeric(infer$case=="Infertile"), infer$parity, alternative = "two.sided", method = "pearson")
cor.test(as.numeric(infer$case=="Infertile"), infer$parity, alternative = "two.sided", method = "spearman")


glm.fit = glm(as.numeric(infer$case=="Infertile") ~ induced, data = infer, family = "binomial")
summary(glm.fit)
summary(glm.fit$fitted.values)

plot(infer$induced, as.numeric(infer$case=="Infertile"),col="red",xlab="induced",ylab="case")
points(glm.fit$data$induced,glm.fit$fitted.values, col = "black", pch = 4)
curve(predict(glm.fit,data.frame(induced = x),type="resp"),col="blue",lwd=2,add=TRUE)

#Analyse predictions--------case ~ induced + spontaneous + parity + education

residuals.glm(infertilityLog,type="pearson")
plot(residuals.glm(infertilityLog))

residuals.glmD(infertilityLog,type="deviance")
plot(residuals.glmD(infertilityLog))


predictTrain = predict(infertilityLog, type ="response")
summary(predictTrain)
tapply(predictTrain, inferTrain$case, mean)

table(inferTrain$case, predictTrain > 0.3)

install.packages("ROCR")
library(ROCR)

ROCRpred = prediction(predictTrain, inferTrain$case)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cuttoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))


#####################################
#Model 2 
####################################
infertilityLog0 = glm(case ~ induced + spontaneous + parity + age, data = inferTrain, family = binomial())
summary(infertilityLog0)

plot(infertilityLog0)

#check for colinearity
cor(inferTrain[c("induced", "spontaneous","parity", "age")])

#Analyse predictions--------case ~ induced + spontaneous + parity + age
predictTrain0 = predict(infertilityLog0, type ="response")
summary(predictTrain0)
tapply(predictTrain0, inferTrain$case, mean)

table(inferTrain$case, predictTrain0 > 0.3)

ROCRpred0 = prediction(predictTrain0, inferTrain$case)
ROCRperf0 = performance(ROCRpred0, "tpr", "fpr")
plot(ROCRperf0, colorize = TRUE, print.cuttoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))

#######################################
#Resampling
#######################################
library (boot)
set.seed (17)
cv.error.5 <- rep (0, 5)
for (i in 1:5) {
glm.fit <- glm (case ~ induced + spontaneous + parity, data = infer, family = binomial())
cv.error.5[i] <- cv.glm (infer, glm.fit , K = 5)$delta[1]
}
cv.error.5
plot(cv.error.5,type="b",col=2,pch=18,lwd=2, main="case ~ induced + spontaneous + parity")

####################################










