#####################################################################
################ Reducing data Complexity  ##########################
#####################################################################
############## Principal component analysis #########################
######################### P C A #####################################
#####################################################################

# PCA: Principal component analysis:
#      attempts to find uncorrelated linear dimensions 
#      that capture maximal variance in the data.
# EFA: Exploratory factor analysis
#      attempts to capture variance with a small number
#      of dimensions while seeking to make the dimensions
#      interpretable interms of the original variables.
# MDS: Multidimensional scaling
#      maps similarities among observations in terms 
#      of a low-dimension space such as a two-dimensionalplot.
#      MDS can work with metric data and with non-metric data
#      such as categoricalor ordinal data.

brand.ratings <- read.csv("http://goo.gl/IQl8nc")
brand.ratings <- read.csv("D:/Shiva/R docs/rintro-chapter8.csv")
str(brand.ratings)
summary(brand.ratings)

brand.sc<-brand.ratings[,1:9] 
brand.sc[,1:9] <- scale(brand.ratings[,1:9])
brand.sc$brand <-brand.ratings$brand
str(brand.sc)
summary(brand.sc)
hist(brand.sc$perform)
gpairs::gpairs(brand.sc)

# * platikurtic: that means and sd are flatter than standard normal
#     this is common property of survey data

# find the correlation
library(corrplot)
corrplot(cor(brand.sc[,1:9])
         ,order="hclust") # orders by clusters


# What is the avg mean of Brand on  each adjectiv?
brand.mean <- aggregate(.~brand , data = brand.sc,mean)
rownames(brand.mean)<-brand.mean[,1]
brand.mean <-brand.mean[,-1]
brand.mean

#Heatmap
library(ggplot2)
library(RColorBrewer)
heatmap(as.matrix(brand.mean)
        ,col=brewer.pal(9,"GnBu"),trace="none",key=FALSE
        ,dend="none")






set.seed(98286)
xvar<-sample(1:10, 100, replace = TRUE)
yvar <- xvar
yvar[sample(1:length(yvar),50)] <- sample(1:10,50,replace = TRUE)
#var[sample(1:length(yvar),50)] <-
zvar <- yvar
zvar[sample(1:length(zvar),50)] <- sample(1:10,50,replace = TRUE)
my.vars<-cbind(xvar,yvar,zvar)
my.vars
plot(yvar~xvar,data = jitter(my.vars))
cor(my.vars)

my.pca<-prcomp(my.vars)
my.pca
summary(my.pca)


cor(my.pca$x)
cor(my.pca$rotation)

#----- Visualizing PCA
biplot(my.pca)



#do PCA for Brand Rating
brand.pca <- prcomp(brand.sc[,1:9])
brand.pca
summary(brand.pca)

plot(brand.pca,type="l")
# when and elbow is seen(comp 3 or 4) that means 
# first 2 or 3 explain most of the variance

biplot(brand.pca)
# (serious-leader-perform)  <> (fun)
# (rebuy-value-bargain) <>  (trendy-latest)   

brand.mu.pca <- prcomp(brand.mean,scale=TRUE)
summary(brand.mu.pca)
#PC1 and PC2 together form 84% of variation

biplot(brand.mu.pca,cex=c(1.5,1))


# how to change e, to reach brand c:
brand.mean["c",]-brand.mean["e",]
#strengthen "serius" and "prrform" weeken "fun" and "value"


# how to put e betwwn "c-b" and "f-g":
colMeans(brand.mean[c("b","c","f","g"),]) - brand.mean["e",]
#strengthen "prforme" and weeken "latest" and "fun"

#  * scree plot shows homany main component should you focus on

################# Usage:
#
# PCA works on servey  or objective data price oan phisical measurment
# or with combination of the 2.
#
# in multidimentionlal cases, PCA is a good tool to understand diffrences 
# in the market
#
# 1. product rating
# 2. position of consumer segments
# 3. ratings of political candidates
# 4. evaluations of advertisements
# or any other area where you have metric data on
# multiple dimensions that is aggregated for a modest
# number of discreteentities of interest.




################## Cautions:
# 1. Choosing right aggregate:
# median for ordina   and modal response for categorical
# You should check weather biplots are the same 
# for aggregate and raw data.
#
# 2. Stability of adjectives:
# adding or removing a brand can change the biplot
# you should asses them all, firts:
# take 80% of all observations,and each time drop an adjective
# if maps are the same, then they are stable
#
# 3. position on the map dependson reletive position of 
# principal components (similarities) and not differences
# so you might thing b and ar week in "fun" but b got the 
# strongest "fun" of all!
#


####################################################################
############## Exploratory Factor Analysis #########################
######################### E F A ####################################
####################################################################
# There are some constructs that can not be mesured like 
# price sensibility - category involvment - purchase intent , etc
# but we can observe number of variebles we believe they should relate to
# plus:
# we can assess weather or not we should take all items
# to assesa single consept or not. maybe some are irrelevant
#
#
############# Usage:
# 1. Dimension reduction: use factor score instead of 10 items
# 2. Reduce uncertainity: if satsisfaction is shown imperfecltly
#    in several items, then combination of them will have 
#    less noise.
# 3. Distcarding some non imoirtant items









########### 1. determine number of factors to esstimate:
#    a.use screeplot
plot(brand.pca,type='l')
#    b. retain factor
install.packages("nFactors")
library(nFactors)
nScree(brand.sc[,1:9])
# 3 of methods suggest that 3 is number of the factors
# examine #eigen value now:
eigen(cor(brand.sc[,1:9]))
# check $values: first 3 are > 1
#
########### 2.Run:
factanal(brand.sc[,1:9], factors = 2)
# factor1:value: loads on bargain and value
# factor2:fun: loads on leader and fun
factanal(brand.sc[,1:9], factors = 3)
# adds factor3: loads on latest and trendy
#--------roration
# oblique rotation: if you need toconsider corellatoins:
install.packages("GPArotation")
library(GPArotation)
brand.fa.ob <-factanal(brand.sc[,1:9],factors = 3,rotation = "oblimin")
brand.fa.ob
# 1.factor numbers are slightly different
# 2.factor1 and 2 are negetively correlated as supposed to be
########### 3.visualize
library(gplots)
library(RColorBrewer)
heatmap.2(brand.fa.ob$loadings
          ,col = brewer.pal(9,"Greens")
          ,trace = "none", key = "FALSE" , dend = "none"
          ,colv=FALSE, cexCol = 1.2
          ,main="\n\n\n\n\nFactor loadings for brand adjectives")
# or pth diagram
install.packages("semPlot")
library(semPlot)
semPaths(brand.fa.ob,what = "est",residuals = FALSE
         ,cut=0.3
         ,posCol=c("white", "darkgreen"), negCol=c("white", "red")
         ,edge.label.cex = 0.75,ncharNodes=7)
# instead of using 9 items, use 3 underlying latent factor
########### 4.scors
library(GPArotation)
brand.fa.ob <- factanal(brand.sc[, 1:9], factors=3, rotation = "oblimin",scores="Bartlett")
brand.scores <-data.frame(brand.fa.ob$scores)
brand.scores
brand.scores$brand <- brand.sc$brand
str(brand.scores )
plot(brand.scores$Factor1~brand.scores$Factor2,col = brand.scores$brand)


brand.fa.ob.mean <- aggregate(.~brand,data = brand.scores,mean)
brand.fa.ob.mean
rownames(brand.fa.ob.mean) <- brand.fa.ob.mean[,1]
brand.fa.ob.mean <- brand.fa.ob.mean[,-1]
names(brand.fa.ob.mean) <- c("Leader", "Value", "Latest")
brand.fa.ob.mean

heatmap.2( as.matrix(brand.fa.ob.mean)
          ,col=brewer.pal(9, "GnBu"), trace="none", key=FALSE
          , dend="none" 
,cexCol=1.2, main="\n\n\n\n\n\nMean factor score by brand"
)









####################################################################
################### Multidimensional Scaling #######################
############################ MDS ###################################
####################################################################
# works with distance
brand.dist<-dist(brand.mean)
brand.dist
brand.dist*-1

# use -1
heatmap.2( as.matrix(-1*brand.dist)
           ,col=brewer.pal(9, "BuGn"), trace="none", key=FALSE
           , dend="none" 
           ,cexCol=1.2, main="\n\n\n\n\n\nMean factor score by brand"
)

brand.mds <-cmdscale(brand.dist)
brand.mds
plot(brand.mds)
text(brand.mds,row.names(brand.mds),cex=2) #--- point name in plot


#--------------- ordinal factor
# uses when we have text

#--change numbers to rank
brand.mean
# orderd code ans as ordinal factor
ordered(rank(brand.mean$perform))

brand.rank <- data.frame(lapply(brand.mean,function(x) ordered(rank(x))))
str(brand.rank)

#install.packages("cluster")
library(cluster)
brand.dist.factor<-daisy(brand.rank,metric = "gower")
brand.dist.factor

library(gplots)
library(RColorBrewer)
heatmap.2( as.matrix(-1*brand.dist.factor)
           ,col=brewer.pal(9, "BuGn"), trace="none", key=FALSE
           , dend="none" 
           ,cexCol=1.2, main="\n\n\n\n\n\nMean factor score by brand"
)

library(MASS)
brand.mds.factor<-isoMDS(brand.dist.factor)

plot(brand.mds.factor$points , type = "n")
text(brand.mds.factor$points , levels(brand.sc$brand),cex=2)

