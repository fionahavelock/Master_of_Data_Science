data = read.csv(file="C:\\Users\\fgene\\Downloads\\Wholesale_customers_miss1.csv")
View(data)

df = read.csv(file="C:\\Users\\fgene\\OneDrive\\Documents\\imputed_data.csv")
View(df)


df <- cbind(ID = 1:nrow(data), data)    # Applying cbind function
View(df) 
##########################
#Initial Data Visualisation
#########################
install.packages('psych')
library(psych)
pairs.panels(data,
             method = "pearson", # correlation method
             hist.col = "#4E7ACC",
             col = "red",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             main="Scatter Plot Matrix of Spending on Wholesale Products"
)


# Plotting Outliers
library(ggplot2)
boxplot(data, main="Spending on Wholesale Products", 
        ylab="Annual Spending in Monetary Units (m.u.)", 
        xlab="Product Categories", col="#4E7ACC")
summary(data)

par(mfrow = c(2, 3))

qqnorm(data$Fresh, pch = 1, frame = FALSE, main = "Fresh")
qqline(data$Fresh, col = "#4E7ACC", lwd = 2)

qqnorm(data$Milk, pch = 1, frame = FALSE, main ="Milk")
qqline(data$Milk, col = "#e13342", lwd = 2)

qqnorm(data$Grocery, pch = 1, frame = FALSE, main ="Grocery")
qqline(data$Grocery, col = "#e13342", lwd = 2)

qqnorm(data$Frozen, pch = 1, frame = FALSE, main ="Frozen")
qqline(data$Frozen, col = "#e13342", lwd = 2)

qqnorm(data$Detergents_Paper, pch = 1, frame = FALSE, main ="Detergents_Paper")
qqline(data$Detergents_Paper, col = "#e13342", lwd = 2)

qqnorm(data$Delicatessen, pch = 1, frame = FALSE, main ="Delicatessen")
qqline(data$Delicatessen, col = "#e13342", lwd = 2)

#summary
summary(data$Frozen)

#Finding outliers

boxplot(df$Fresh, plot=FALSE)$out
outliers <- boxplot(df$Fresh, plot=FALSE)$out
outliers

boxplot(df$Milk, plot=FALSE)$out
outliers <- boxplot(df$Milk, plot=FALSE)$out
outliers

boxplot(df$Grocery, plot=FALSE)$out
outliers <- boxplot(df$Grocery, plot=FALSE)$out
outliers

boxplot(df$Froz, plot=FALSE)$out
outliers <- boxplot(df$Froz, plot=FALSE)$out
outliers

boxplot(data$DetP, plot=FALSE)$out
outliers <- boxplot(data$DetP, plot=FALSE)$out
outliers

boxplot(df$DelicE, plot=FALSE)$out
outliers <- boxplot(df$DelicE, plot=FALSE)$out
outliers

#################
#impute the data
#Multiple imputation (MI)

library("mice")

mice_data <- mice(data, m=1, seed=123)
summary(mice_data)

mice_data2 <- mice(data, m=1, seed=123, method =c("sample","sample","", "", "","") )
mice_data2$method

pred <- mice_data$predictorMatrix
pred

mice_data3 <- mice(data, m=1, predictorMatrix = pred,seed=123, 
                  method =c("rf","rf","rf", "rf", "rf","rf") )

mice_data3 <- mice(data, m=1, predictorMatrix = pred,seed=123, 
                   method =c("pmm","pmm","pmm", "pmm", "pmm","pmm") )

mice_data3 <- mice(data, m=1, predictorMatrix = pred,seed=123, 
                   method =c("midastouch","midastouch",
                             "midastouch",
                             "midastouch", 
                             "midastouch","midastouch") )


completedata <- complete(mice_data3)# this is the first completed (imputed) data set
head(completedata)

View(completedata)
densityplot(mice_data3, main = "Imputation with Predictive Mean Matching Method")



write.csv(completedata,'imputed_data.csv')

#Model fitting
model_final <- with(mice_data3,lm(Fresh ~ Grocery + Frozen + Milk + Detergent_Paper
                                  + Delicatessen))
model_final

summary(pool(model_final))


##############Single Imputation
imp_reg <- mice(data, method = "midastouch", m = 1, maxit = 1,
                seed = 123, print = FALSE)
model_reg <- with(imp_reg,lm(Fresh ~ Grocery + Frozen + Milk + Detergent_Paper
                             + Delicatessen))
summary(pool(model_reg)) 


reg_dat <- complete(imp_reg )# extract the completed data set
summary(lm(Fresh ~ Grocery + Frozen + Milk + Detergent_Paper
           + Delicatessen, data = reg_dat))# model fitting and summary


densityplot(imp_reg)

############################
#Missing data Visualisation

md.pattern(data)

md.pairs(data)
library(VIM)
aggr(data, col=c("#4E7ACC","#e13342"), numbers=TRUE, sortVars=TRUE, labels=names(data),
     cex.axis=.7, gap=3, ylab=c("Proportion of Missingness","Missingness Pattern"))

marginplot(data[, c( "Delicatessen", "Milk")], col=c("#4E7ACC","#e13342"), ylab = "Milk", 
           xlab = "Delicatessen", main = "Margin Plot for Missing Values",
           cex.numbers = 1.2, pch = 20)
#####################
#PCA
#Use imputed data
df = read.csv(file="C:\\Users\\fgene\\OneDrive\\Documents\\imputed_data.csv")
View(df)

summary(df)

sd <- apply(df, 2, sd)
sd
#Spectral decomposition can be obtained using the eigen() function
S <- cor(df)
S
eigdec <- eigen(S)
eigdec


eig <-  eigdec$values
eig

sum(eig)

#The eigenvectors can be obtained as
pc_loading <-  eigdec$vectors
rownames(pc_loading) <- colnames(df)
pc_loading

#We can obtain the proportion of variance explained as follows:
# Variances in percentage
eig <-  eigdec$values
variance <- eig*100/sum(eig)
# Cumulative variances
cumvar <- cumsum(variance)
eig2<- data.frame(eig = eig, variance = variance,
                  cumvariance = cumvar)
eig2 


#[(i)] Use prcomp() to fit the model as follows:
pr.out <- prcomp(df, scale =TRUE)
names(pr.out)
summary(pr.out)


#Scree plot 
library("factoextra")
fviz_screeplot(pr.out, addlabels = TRUE, barfill = "#4E7ACC",
               linecolor = "black", barcolor = "#4E7ACC", 
               title = "Scaled Data Scree Plot")

#The PC loadings and scores can be extracted using the codes below:
pr.out <- prcomp(df,scale =TRUE )


#pc loadings
pr.out$rotation

#pc score
pr.out$x



#[(ii)] Correlation between variables and PCs, 
#quality of representation (cos2) and contributions 
#can be obtained respectively as
var <- get_pca_var(pr.out)
var$cor
var$cos2
var$contrib

#plot the contribution of the variables to PC1 as follows:


fviz_contrib(pr.out, choice = "var", axes = 1:3, top = 10, fill = "#4E7ACC",
             color = "#4E7ACC", linecolor = "black")

fviz_contrib(pr.out, choice = "var", axes = 1:4, top = 10, fill = "#4E7ACC",
             color = "#4E7ACC", linecolor = "black")


fviz_contrib(pr.out, choice = "var", axes = 1, top = 10, fill = "#4E7ACC",
             color = "#4E7ACC", linecolor = "black")

fviz_contrib(pr.out, choice = "var", axes = 2, top = 10, fill = "#4E7ACC",
             color = "#4E7ACC", linecolor = "black")

fviz_contrib(pr.out, choice = "var", axes = 3, top = 10, fill = "#4E7ACC",
             color = "#4E7ACC", linecolor = "black")


fviz_contrib(pr.out, choice = "var", axes = 4, top = 10, fill = "#4E7ACC",
             color = "#4E7ACC", linecolor = "black")

#bipolot
fviz_pca_biplot(pr.out, 
                addEllipses = TRUE, label = "var", axes = 3:4,
                title = "Principal Component Biplot For PC3 and PC4",
                col.var = "black", col = "#4E7ACC", repel = TRUE)
#bipolot
fviz_pca_biplot(pr.out, 
                addEllipses = TRUE, label = "var", axes = 1:2,
                title = "Principal Component Biplot For PC1 and PC2",
                col.var = "black", col = "#4E7ACC", repel = TRUE)


####################
#Clustering

#K medoids

################### With dimensional reduction ############
library(cluster)
library(factoextra)
library(ggplot2)




############################
#find optimum clusters with dimension reduction

library(cluster)
library(factoextra)
df.scaled <- scale(df)
pr.out = prcomp(df.scaled)
fviz_nbclust(pr.out$x[,1:4], pam, method = "silhouette")+
  theme_classic() + 
  labs(title= "Optimal Number of Clusters With Dimensional Reduction")


############
#Silhoute anayliss

library(cluster)
df.scaled <- scale(df)
pr.out = prcomp(df.scaled) 
pam.res <- pam(pr.out$x[, 1:4], k =2)
sil <- silhouette(pam.res$cluster, dist(df.scaled))

library(factoextra)
fviz_silhouette(sil, palette = c("#4E7ACC", "#FA806E"),
                ggtheme = theme_classic(), main = "Cluster Silhouette Plot With Dimension Reduction
                Average Silhouette Width: 0.26 "
)

#############
#create clusters with dimension reduction


x= df
k=2
pam(x, k, metric="euclidean", stand = FALSE)


library(cluster)
df.scaled <- scale(df)
pr.out = prcomp(df.scaled) 
pam.res <- pam(pr.out$x[,c (1,2)], k=2)



names(pam.res)

pam.res$medoids

pam.res$i.med

pam.res$clustering
head(pam.res)

pr.out = prcomp(df) 
library(factoextra)
fviz_cluster(pam.res, pr.out, ellipse.type = "norm", 
             palette = c("#4E7ACC", "#FA806E"), 
             main = "K-Medoid Clusters with Dimensional Reduction",
             xlab="Dim 1 (48.5%)", ylab ="Dim 2 (17.2%)"
             )

############K means

# Compute k-means with k = 3
df.scaled <- scale(df)
pr.out = prcomp(df.scaled) 

set.seed(123)

km.res <- kmeans(pr.out$x[,c (1,4)], 2, nstart = 25)
names(km.res)

km.res

aggregate(pr.out$x[,c (1,4)], by=list(cluster=km.res$cluster), mean)

fviz_cluster(km.res, pr.out$x[,c (1,4)], ellipse.type = "norm", 
             palette = c("#4E7ACC", "#FA806E"), 
             main = "K-Means Clusters with Dimensional Reduction", 
             xlab="Dim 1 (48.5%)", ylab ="Dim 4 (14.7%)")

################### Without dimensional reduction ############
library(cluster)
library(factoextra)
library(ggplot2)

############################
#find optimum clusters without dimension reduction

library(cluster)
library(factoextra)
df.scaled <- scale(df)
fviz_nbclust(df, pam, method = "silhouette")+
  theme_classic() + 
  labs(title= "Optimal Number of Clusters Without Dimensional Reduction")

############
#Silhoute anayliss

library(cluster)
df.scaled <- scale(df)
pam.res <- pam(df, k =3)
sil <- silhouette(pam.res$cluster, dist(df.scaled))

library(factoextra)
fviz_silhouette(sil, palette = c("#4E7ACC", "#FA806E", "#63E6AC"),
                ggtheme = theme_classic(), main = "Cluster Silhouette Plot Without Dimension Reduction
                Average Silhouette Width: 0.13 "

)

########create clusters without dimensional reduction

x= df
k=3
pam(x, k, metric="euclidean", stand = FALSE)

library(cluster)
df.scaled <- scale(df)
pam.res <- pam(df, k=3)

names(pam.res)

pam.res$medoids

pam.res$i.med

pam.res$clustering
head(pam.res)

library(factoextra)
fviz_cluster(pam.res, df, ellipse.type = "norm",
             palette = c("#4E7ACC", "#FA806E","#54C494"), 
             main = "K-Medoid Clusters without Dimensional Reduction"
            
             )


########Hierachal clustering with Dimensional Reduction #############

x= df
#k=3

df.scaled <- scale(df)
pr.out = prcomp(df.scaled)


# Compute distances and hierarchical clustering
dd <- dist(pr.out$x[, 1:4], method = "euclidean")
dd
hc <- hclust(dd, method = "complete")
hc
hc1 <- hclust(dd, method = "single")
hc2 <- hclust(dd, method = "average")

 par (mfrow = c(1, 3))
dendros <- as.dendrogram(hc1)
plot(dendros, main = "Single linkage",
     ylab = "Height")

dendros <- as.dendrogram(hc2)
plot(dendros, main = "Average linkage",
     ylab = "Height")

dendros <- as.dendrogram(hc)
plot(dendros, main = "Complete linkage",
     ylab = "Height")
abline(h=0.2, lty = 2, col="red")
abline(h=0.428, lty = 2, col="blue")#point 116 and 142 are merged

library(factoextra)
fviz_dend(hc,k = 2,  # Cut in three groups
          cex = 0.5, # label size
          k_colors = c("#4E7ACC", "#FA806E"),
          color_labels_by_k = TRUE, # color labels by groups
          ggtheme = theme_gray(), # Change theme
          main = "Hierarchical Clustering on the First Four Principal Components"
)


hcut <- cutree(hc, k = 2)
table(hcut)
hc$height

#The aggregate() function can be used to compute 
#the mean of each variables by clusters using original data.
aggregate(pr.out$x[, 1:4], by=list(cluster=hcut), mean)


library(cluster)
m <- c("average","single","complete")
names(m) <- c("average","single","complete")
# function to compute coefficient
ac <- function(x){
  agnes(pr.out$x[, 1:4], method = x)$ac
}

library(tidyverse)
map_dbl(m,ac)



library(igraph)
fviz_dend(hc, k = 2, k_colors =c("#4E7ACC", "#FA806E"),
          type = "phylogenic", repel = TRUE, 
          title ="Phylogenic Plot - Hierarchical Clustering on the First Four Principal Components")


########Hierachal clustering without Dimensional Reduction #############

x= df
k=2

df.scaled <- scale(df)

# Compute distances and hierarchical clustering
dd <- dist(df.scaled, method = "euclidean")

dd
hc <- hclust(dd, method = "complete")
hc
library(factoextra)
fviz_dend(hc, k = 2,# Cut in three groups
          cex = 0.5, # label size
          k_colors = c("#4E7ACC", "#FA806E"),
          color_labels_by_k = TRUE, # color labels by groups
          ggtheme = theme_gray(),
          main = "Hierarchical Clustering without Dimension Reduction"# Change theme
)
hc$height

hcut <- cutree(hc, k = 2)
table(hcut)
k = 2

library(igraph)
fviz_dend(hc, k = 2, k_colors =c("#4E7ACC", "#FA806E"),
          type = "phylogenic", repel = TRUE, 
          title ="Phylogenic Plot - Hierarchical Clustering on the Whole Data Set")

