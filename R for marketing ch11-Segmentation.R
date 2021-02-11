###############################################################
#####################                 #########################
#####################   Segmentation  #########################
#####################                 #########################
###############################################################
#--------------------------------------------------------------
# Structoral MOdels:
#   Customer relationship management (CRM)records, attitudinal surveys
#   ,product purchase and usage, and more generally, anydata set with
#   observations about customers
# hclust() models data in tree
# kmeans() models data in group centroids




seg.raw <- read.csv("D:/Shiva/R docs/rintro-chapter5.csv")
summary(seg.raw)
str(seg.raw)

seg.df<-seg.raw[,-7]
str(seg.df)


###### Steps:
#  0. preanalysis
#  1. transform data . some methods require all numeric (kmeans - mclust0)
#     or categorical data (poLCA)
#  2. Compute distance Matrix if needed
#  3. Apply the method. add K if needed
#  4. further parse the object to obtain a solution withKgroups(e.g.,hclust()).
#  5. Examine the solouthion


#####  pre analysis
seg.sum <- function(data , groups){
  aggregate(data , list(groups), function(x) mean(as.numeric(x)))
}

seg.sum(seg.df,seg.raw$Segment)


##################################### hirarchical clustering: hclust()
# based on distance
# needs dissimilarity matrix

#---- find the distance matrix:
d <- dist(seg.df[,c("age","income","kids")])
as.matrix(d)[1:5,1:5]
seg.df

#---- It s better to use a function to find the dist and rescale it in the same time:
library(cluster)
seg.dist<-daisy(seg.df)
seg.dist
as.matrix(seg.dist)[1:5,1:5]


#--- run the cluster method:
seg.hc <- hclust(seg.dist,method="complete")
# complete: evaluates the distance between every member when proceding

#--- Visualize:
plot(seg.hc)

# cut the image:
par(mfrow=c(2,2))
plot(cut(as.dendrogram(seg.hc),h=0.5)$lower[[1]])
plot(cut(as.dendrogram(seg.hc),h=0.5)$lower[[2]])
plot(cut(as.dendrogram(seg.hc),h=0.5)$lower[[3]])
plot(seg.hc)

seg.df[c(128, 137), ]
#---- assesment: Cophentic correlation coeficient (CPCC)
cor(cophenetic(seg.hc),seg.dist)
# > 0.7 indicates a relativly  strong fit

#---- take segment:
# height= 0.7    =>   k=2
# height = 0.4   =>   k=7
plot(seg.hc)
rect.hclust(seg.hc,k=3,border = "red")

#---membership vector
seg.hc.segment <- cutree(seg.hc,k=4)
seg.hc.segment
table(seg.hc.segment)
seg.sum(seg.df,seg.hc.segment)
str(seg.df)


#---- insight
#plot of gender by subscribe with all of the obser-vations colored by segment membership.
plot( jitter(as.numeric(seg.df$gender)) ~ jitter(as.numeric(seg.df$subscribe))
      ,col=seg.hc.segment,yax="n", xaxt="n", ylab="", xlab="")
axis(1, at=c(1, 2), labels=c("Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1, 2), labels=levels(seg.df$gender))
# in general, answers are so UNinteresting    :
# “Our advanced hierarchical analysis in R examinedconsumers
#  who don’t yet subscribe and found two segments to target!
#  The segmentsare known as ‘Men’ and ‘Women.”’
# * a reason of being so untineteresting is: by rescaling, categorical features
#   like ownHomw and gender became so influencive


######################################## Mean based Clustering
#based on mean sum-of-squared deviation of each observation from centroid
# -Eculidian Distance  => needs numeric features ==> not optimized for binary features but we try it

#--- recode to numeric
seg.df
seg.df.num <- seg.df#[seg.df$income > 0]
seg.df.num$gender <- ifelse(seg.df$gender =="male" , 0 ,1)
seg.df.num$ownHome <- ifelse(seg.df$ownHome =="ownNo" , 0 ,1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe =="subNo" , 0 ,1)

summary(seg.df.num  )

#-----  methodfit with k
set.seed(96743)
seg.k <- kmeans(seg.df.num,centers = 4)
seg.k

seg.sum(seg.df,seg.k$cluster)

#------ visualize
par(mfrow = c(1,2))
boxplot(seg.df.num$income ~seg.k$cluster , ylab="Income", xlab="Cluster")
boxplot(seg.df.num$income ~seg.hc.segment , ylab="Income", xlab="Cluster")


str(seg.df.num)
par(mfrow = c(1,2))
boxplot(seg.df.num$age ~seg.k$cluster , ylab="age", xlab="Cluster")
boxplot(seg.df.num$age ~seg.hc.segment , ylab="age", xlab="Cluster")


par(mfrow = c(1,2))
boxplot(seg.df.num$kids ~seg.k$cluster , ylab="kids", xlab="Cluster")
boxplot(seg.df.num$kids ~seg.hc.segment , ylab="kids", xlab="Cluster")



par(mfrow = c(1,2))
boxplot(seg.df.num$ownHome ~seg.k$cluster , ylab="ownHome", xlab="Cluster")
boxplot(seg.df.num$ownHome ~seg.hc.segment , ylab="ownHome", xlab="Cluster")


par(mfrow = c(1,2))
boxplot(seg.df.num$subscribe ~seg.k$cluster , ylab="subscribe", xlab="Cluster")
boxplot(seg.df.num$subscribe ~seg.hc.segment , ylab="subscribe", xlab="Cluster")


par(mfrow = c(1,2))
plot( jitter(as.numeric(seg.df$income)) ~ jitter(as.numeric(seg.df$subscribe))
      ,col=seg.k$cluster,yax="n", xaxt="n", ylab="", xlab="")
plot( jitter(as.numeric(seg.df$income)) ~ jitter(as.numeric(seg.df$subscribe))
      ,col=seg.hc.segment,yax="n", xaxt="n", ylab="", xlab="")


par(mfrow = c(1,2))
plot( jitter(as.numeric(seg.df$income)) ~ jitter(as.numeric(seg.df$age))
      ,col=seg.k$cluster,yax="n", xaxt="n", ylab="", xlab="")
plot( jitter(as.numeric(seg.df$income)) ~ jitter(as.numeric(seg.df$age))
      ,col=seg.hc.segment,yax="n", xaxt="n", ylab="", xlab="")


par(mfrow = c(1,2))
plot( jitter(as.numeric(seg.df$income)) ~ jitter(as.numeric(seg.df$ownHome))
      ,col=seg.k$cluster,yax="n", xaxt="n", ylab="", xlab="")
plot( jitter(as.numeric(seg.df$income)) ~ jitter(as.numeric(seg.df$ownHome))
      ,col=seg.hc.segment,yax="n", xaxt="n", ylab="", xlab="")


# visualize  cluster by plotting them against a dimension plot
library(cluster)
clusplot(seg.df,seg.hc.segment,color = TRUE, shade = TRUE # the elipse for group membership
         , labels=4 # label only the groups
         , line =0 # ommit distance lines between groups
         , main="K-means cluster plot"
         )

clusplot(seg.df,seg.k$cluster,color = TRUE, shade = TRUE # the elipse for group membership
         , labels= 4 # label only the groups
         , line =0 # ommit distance lines between groups
         , main="K-means cluster plot"
)




#########################  Model-based Clustering / mixture models
# the idea is observant come from groups with different distribution
# uses just numeric data

seg.df.num <- seg.df[seg.df$income > 0]
seg.df.num <- seg.df
seg.df.num$gender <- ifelse(seg.df$gender =="male" , 0 ,1)
seg.df.num$ownHome <- ifelse(seg.df$ownHome =="ownNo" , 0 ,1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe =="subNo" , 0 ,1)
seg.df.num 


install.packages("mclust")
library(mclust)
seg.mc <- Mclust(seg.df.num)
summary(seg.mc)

#llipsoidal, equal volume and shape:
#   (modeling the data as multivariateellipses)  fits the best


#----- logliklihood of 4 clusters:
seg.mc3 <- Mclust(seg.df.num,G=3)
summary(seg.mc3)
# it is got less log.liklihood

seg.mc4 <- Mclust(seg.df.num,G=4)
summary(seg.mc4)

#----- comparing models with BIC
BIC(seg.mc,seg.mc4)
# the lower the BIC is, the better
# * some method report negetive BIC. use BIC() method to know the real direction.
# * higher logloklihoodis better

# BIC difference     Odds of model superiority (%)   Strength of the evidence
#     0–2                    50–75                         Weak
#     2–6                    75–95                        Positive
#     6–10                   95–99                         Strong
#      >10                     >99                        Very strong



#----- assesment
seg.sum(seg.df , seg.mc3$classification)
library(cluster)

clusplot(seg.df,seg.mc3$classification,color = TRUE, shade = TRUE # the elipse for group membership
         , labels= 4 # label only the groups
         , line =0 # ommit distance lines between groups
         , main="K-means cluster plot"
)





################################### Latent Class Analysis  poLCA:
# is similar to mixture modeling in the assumption thatdifferences
# are attributable to unobserved groups that one wishes to uncover.
# - categorical data
seg.df.cut <-seg.df
seg.df.cut$age <- factor(ifelse(seg.df$age < median(seg.df$age),1,2)) 
seg.df.cut$income <- factor(ifelse(seg.df$income < median(seg.df$income),1,2)) 
seg.df.cut$kids <- factor(ifelse(seg.df$kids < median(seg.df$kids),1,2)) 
summary(seg.df.cut)

seg.f <- with(seg.df.cut
              ,cbind(age,gender,income,kids,ownHome,subscribe)~1)

seg.f
#---- fit
install.packages("poLCA")
library(poLCA)
set.seed(02807)
seg.LCA3 <- poLCA(seg.f,data = seg.df.cut, nclass = 3)
seg.LCA4 <- poLCA(seg.f,data = seg.df.cut, nclass = 4)

#--- asses and compare:
seg.LCA3$bic
seg.LCA4$bic
# it is not clear. so we use other things:

seg.sum(seg.df,seg.LCA3$predclass)

seg.sum(seg.df,seg.LCA4$predclass)
#--- group 3 has no subscribe they are identified as young girls with no kids

par(mfrow= c(1,2))
clusplot(seg.df, seg.LCA3$predclass, color=TRUE, shade=TRUE,
          labels=4, lines=0, main="LCA plot (K=3)")

clusplot(seg.df, seg.LCA4$predclass, color=TRUE, shade=TRUE,
         labels=4, lines=0, main="LCA plot (K=3)")







############### Comparing Clustrs:
# to compare 2 segmentation result ad see each class bind to which class
table(seg.LCA3$predclass,seg.LCA4$predclass)
# in 3-cluster:  G1 = 1,3,4 of 4-Cluster
# in 3-cluster:  G2 = 2     of 4-Cluster
# in 3-cluster:  G3 = 1     of 4-Cluster


#--- a better soloution:
library(mclust)
mapClass(seg.LCA3$predclass, seg.LCA4$predclass)

adjustedRandIndex(seg.LCA3$predclass, seg.LCA4$predclass)
#0.7288822 says that 1Cluster and 4 cluster differ from each other. much better than a change

#what is we were to test random assignment?
random.data <- sample(4,length(seg.LCA4$predclass),replace = TRUE)
random.data
adjustedRandIndex(seg.LCA3$predclass,random.data)
#0.007751372 :match between theclusters is no better than random chance.


table(seg.raw$Segment,seg.LCA4$predclass)
adjustedRandIndex(seg.raw$Segment,seg.LCA4$predclass)
# With a Rand index of 0.35, the LCA solution matches the
# true segment assignmentsmoderately better than chance alone.




















###################################################################
####################     CLASSIFICATION     #######################
###################################################################


##################  Naive Bayes
# use train to learn probability of class membership
# as a function of each predictor variable considered independently



#-------- split train and test
set.seed(04625)
train.prop <- 0.65
train.class <-sample(nrow(seg.raw),nrow(seg.raw)*train.prop)
seg.df.train <- seg.raw[train.class,]
seg.df.test <- seg.raw[-train.class,]



#------------ fit the Naive Bayes Model
library(e1071)
(seg.nb <- naiveBayes(Segment ~ . , data = seg.df.train))
# a member of moving up has 64% probability of being female



#----------- predict:
(seg.nb.class <- predict(seg.nb , seg.df.test))


#------------ visualize:
#  examine frequencies of prediction:
prop.table(table(seg.nb.class))


# A cluster plot against  principal components:
clusplot(seg.df.test[-7],seg.nb.class, color = TRUE, shade = TRUE
         ,labels = 4 , lines = 0
         ,main = "Naive Bayes classification, holdout data")

#------------- assesment:
# raw aggrement:
mean(seg.df.test$Segment == seg.nb.class)
#  0.847619 :  imperfect but better than chance

library(mclust)
adjustedRandIndex(seg.nb.class,seg.df.test$Segment)
#  0.6286037


# ompare performance
table(seg.nb.class , seg.df.test$Segment)


# compare means of real segment and predicted Segment of test data
seg.sum(seg.df.test, seg.nb.class)
seg.sum(seg.df.test, seg.df.test$Segment)
# also some observations ar miss classified but means of groups are almost the same
# => also assignment is not perfect in case by case basis, the overal group definitions
#    are quite similar

#--------- prediction + odds of  membership in each segment
predict(seg.nb , seg.df.test,type = "raw")
# why do we need it?
# in high cost campaigns: we can target just those who we are certain to be
# in a segment
# for low-cost campaing we might also targe people for secend-best segment + primary





############################### Random forest:
# - builds and ensemble of methods
# - each tree optimized to fit only some of the observations
# - predictions: all the trees try to predict and the consensus
#   which recieves the most vote is the answer
# => avoid dependencies on a precise mode
# +  resilliant in the face of difficult data

############################## usage:
#  1. checking a Cluster methos
#  2. Estimating importance of a variabe




#------- fit the method:

install.packages("randomForest")
library(randomForest)
set.seed(98040)
(seg.rf <- randomForest(Segment ~ . , data = seg.df.train
                        , ntree = 3000))
# set.seed is required bz it is random
# ntree: 5-10 tree per observation for small datasets like this
# it provides a confision matixby its "Out Of the Bag"  data

#----------- predict:
(seg.rf.class <- predict(seg.rf , seg.df.test[,-7]))


#----- visualize:
library(cluster)
clusplot(seg.df.test[, -7], seg.rf.class, color=TRUE, shade=TRUE,
         labels=4, lines=0, main="Random Forest classification, holdout data"
)

#---- all segment liklihood:
seg.rf.class.all <- predict(seg.rf,seg.df.test, predict.all = TRUE)
apply(seg.rf.class.all$individual[1:5,],1,table)/3000
apply(seg.rf.class.all$individual[1:5,],1,table)
# we can get #Votes.  devide by 3000 to get percentage


#---- assesmrnt
seg.sum(seg.df.test,seg.df.test$Segment)
seg.sum(seg.df.test,seg.rf.class)

mean(seg.df.test$Segment == seg.rf.class)
#  0.7714286

table(seg.df.test$Segment , seg.rf.class)


library(mclust)
adjustedRandIndex(seg.df.test$Segment , seg.rf.class)
# 0.5174946


#----- Check importance of Variables:
set.seed(98040)
(seg.rf <- randomForest(Segment ~. , data = seg.df.train, ntree = 3000
                        , importance =TRUE #********
                        ))
importance(seg.rf)

# variable importance by Segment:
#    age is important but gender is not
#
# MeanDecreaseAccuracy :  impact on accuracy :
# MeanDecreaseGini     :  and an assess-ment of the variable’s 
#    ability to assist classification better than chance labeling

varImpPlot(seg.rf , main = "VariableImportance by Segment")


# importance of variables by Segment:
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(seg.rf)[ , 1:4]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment")



#################################  Predicting Potential Costomers
#  we can develop a model to identify customers for whom the outcome
#  is most likely among new prospects.

#------- test train
set.seed(92118)
tran.prop <- 0.65
train.cases <- sample(nrow(seg.df), nrow(seg.df)*train.prop)
sub.df.train <- seg.df[train.cases,]
sub.df.test <- seg.df[-train.cases,]


# how well are subs and not subs differentiated
str(seg.df.train)
clusplot(sub.df.train[,-6] ,  sub.df.train$subscribe, color=TRUE, shade=TRUE
         ,  labels=4, lines=0, main="Subscriber clusters, training dat")
# not diffrenetiated when plots agiants principal component
# that mean problem is difficult



#------ fit model with Random forest
library(randomForest)
set.seed(11954)
(sub.rf <- randomForest(subscribe ~ . , data = sub.df.train, ntree = 3000))
# all subYes group is predicted are wrongly predicted
# Why?  - class imbalance: one category dominates the data
# soloution:
#  * sampsize should be <= cnt subscribers

table(sub.df.train$subscribe)
set.seed(11954)
(sub.rf <- randomForest(subscribe ~ . , data = sub.df.train, ntree = 3000
                        , sampsize = c(23,23) )) # ***to drwa equal number of subs and non-subs
# it gets better


sub.rf.sub_ <- predict(sub.rf , data = sub.df.test)
table(sub.rf.sub , sub.df.test$subscribe)

length(sub.rf.sub_)
length(sub.df.test$subscribe)
sub.df.test$subscribe
sub.rf.sub_


adjustedRandIndex(sub.rf.sub, sub.df.test$subscribe)
library(psych)
cohen.kappa(sub.rf.sub, sub.df.test$subscribe)
# With an adjusted Rand Index=0.19 and Cohen’s kappa=0.26 
# (confidence interval0.025–0.50), the model identifies subscribers in the
# test data modestly better than chance

# how to improve?  logestic Regression
