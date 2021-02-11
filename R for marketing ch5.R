###############################################################
################## Comparing Groups ###########################
###############################################################
###############################################################
# Do menor women subscribe to our service at a higher rate?
# Which demographic segment canbest afford our product? Does 
# the product appeal more to homeowners or renters?The answers
# help us to understand the market, to target customers effectively,
# and to evaluate the outcome of marketing activities such as promotions.

seg.df<- read.csv("D:/Shiva/R docs/rintro-chapter5.csv")
seg.df
remove(seg.df)

segVars <- c("age", "gender", "income", "kids", "ownHome", "subscribe")
segVarType <- c("norm", "binom", "norm", "pois", "binom", "binom")
segNames <- c("Suburb mix", "Urban hip", "Travelers", "Moving up")
segSize <- c(100, 50, 80, 70)

segMeans <- matrix( c( 40, 0.5, 55000, 2, 0.5, 0.1,
            24, 0.7, 21000, 1, 0.2, 0.2,
            58, 0.5, 64000, 0, 0.7, 0.05,
            36, 0.3, 52000, 2, 0.3, 0.2  ), ncol=length(segVars), byrow=TRUE)

segSDs <- matrix( c(
            5, NA, 12000, NA, NA, NA,
            2, NA,  5000, NA, NA, NA,
            8, NA, 21000, NA, NA, NA,
            4, NA, 10000, NA, NA, NA  ), ncol=length(segVars), byrow=TRUE)


#---------------filter by Group---------------
seg.df
str(seg.df)
unique(seg.df$Segment)
#-- mean ofincome for Movingups segment
mean(seg.df$income[seg.df$Segment=="Moving up"
                   & seg.df$subscribe=="subNo"])

# ----- By(DATA,INDICES,FUN)
by(seg.df$income,list(seg.df$Segment,seg.df$subscribe),mean)

aggregate(seg.df$income,list(seg.df$Segment,seg.df$subscribe), mean)
aggregate(income ~ Segment + ownHome , data=seg.df, mean)


with(seg.df , table(Segment,ownHome))
#Segment      ownNo ownYes
#Moving up     47     23
#Suburb mix    52     48
#Travelers     20     60
#Urban hip     40     10

#count of kids by segment
aggregate(seg.df$kids,list(seg.df$Segment),sum)
#0r
xtabs(kids~Segment,data=seg.df)
#Moving up Suburb mix  Travelers  Urban hip 
#134        192          0         55 

#--add to df
seg.income.mean <- aggregate(seg.df$income,list(seg.df$Segment), mean)
seg.df$seg.income <- seg.income.mean[seg.df$Segment,2] #--slect second row
seg.income.mean 

#--------------Visualizing Groups------------
install.packages("lattice")
require(lattice)

#-----proportional hist of groups
#histogram(formula, data, type)
histogram(~subscribe ,data = seg.df)
histogram(~subscribe | Segment,data = seg.df)
histogram(~Segment | subscribe  ,data = seg.df)
#type="count" => gives count instead of proportion
histogram(~subscribe | Segment ,data = seg.df , type="count"
          , layout=c(4,1)
          , col=c("burlywood", "darkolivegreen"))

histogram(~subscribe | Segment + ownHome,data = seg.df)

prop.table(table(seg.df$subscribe, seg.df$Segment),margin=2)
#no margin: prop to entire table
#margin = 1 : prop to row
#margin = 2 : prop to col

#bar chart of "yes" case
barchart(prop.table(table(seg.df$subscribe, seg.df$Segment),margin = 2)[2,])
barchart(prop.table(table(seg.df$subscribe, seg.df$Segment),margin = 2))

#---mean of income by group
library(lattice)
seg.mean <-aggregate(income ~ Segment,seg.df,mean)
seg.mean
barchart(income~Segment,data = seg.mean )

seg.mean <-aggregate(income ~ Segment + ownHome,seg.df,mean)
seg.mean
barchart(income~Segment+ownHome,data = seg.mean )
barchart(income~Segment,data = seg.mean
         ,groups = ownHome 
         ,auto.key=TRUE  #--- legends
         ,par.settings = simpleTheme(col=terrain.colors(2)) ) 
#--boxplot
boxplot(income ~ Segment, data = seg.df
        ,yaxt="n", ylab="Income ($)")
ax.seq<-seq(from=0 , to=120000 , by= 20000)
axis(side = 2, at = ax.seq,abels=paste(ax.seq/1000, "k", sep=""),las=1)














###########################################################
###########################################################
############### Statistical Test ##########################
###########################################################
#"It looksdifferent, but is it really different?"




#--------------------------  Chi sqr
tmp.tbl<-table(rep(c(1:4), times=c(25,25,25,20)))
chisq.test(tmp.tbl)
# p-value = 0.852
# null hypothis: is accepted

tmp.tbl<-table(rep(c(1:4), times=c(25,25,25,20)))
chisq.test(tmp.tbl)
# we can reject thenull hypothesis of no difference between the cells with "95 % confidence."
#In otherwords, the data in this sample suggests that the distribution
#of the values 1:4 is likelyto be unequal in the larger population, 
#assuming the data are a random sample


chisq.test(table(seg.df$Segment))
# sample does not support the hypothesis that there is anidentical 
# number of customers in each segment

table(seg.df$subscribe,seg.df$ownHome)
#Is subscription status independent from home ownership
chisq.test(table(seg.df$subscribe,seg.df$ownHome))
# null: facrots are unrelated => p=0.9 and we can not reject it
#  and home ownership is independent of subscription status.
#  defaults: Yates' correction
#  sim=TRUW  B=10000 : 10000 simulations


#--------------------testing observed proportions:--------
# to test the likeli-hood of randomly observing 12 cases out of 20 
# in one direction, if the true likelihoodis 50 %

binom.test(12,20,p = 0.5)
# bserving 60 % Seattle fans ina sample of 20 does not conclusively 
# demonstrate that there are more Seattle fansin the larger group of
# fans roaming New York.


# What are the odds of observinf 8Seattle dans out of 20 when rate 50%?
dbinom(9,20,0.5)

# What are the odds of observinf 8-12 Seattle dans out of 20 when rate 50%?
sum(dbinom(8:12,20,0.5))




#---------------  Group means------------------------
#----------------   T.Test   ------------------------
#is income different  among those how own a home and those who do not?
#  * data must be normal

aggregate(seg.df$income ~ seg.df$ownHome, FUN = mean)
histogram(~income | ownHome,data = seg.df)
# Yes they are almost norma, then cure the function:
t.test(income~ownHome,data=seg.df)
# H0: there is no difference
# t = -3.2731,  p-value = 0.001195 : reject H0. people with home have higher income
# mean in group ownNo       mean in group ownYes 
#      47391.01                  54934.68

histogram(~income | ownHome + Segment,data = seg.df)
histogram(~income | ownHome ,data = seg.df[seg.df$Segment=='Travelers',])
seg.df[seg.df$Segment=='Travelers',]
t.test(income ~ ownHome , data = seg.df[seg.df$Segment == 'Travelers',])
# p-value=0.79 : there is not a significant difference in meanincome among 
# those Travelers in our data who own homes and who don't
t.test(income ~ ownHome , data = seg.df[seg.df$Segment == 'Moving up',])



#--------------------------------------------------
#----------------  Anova   ------------------------
#--------------------------------------------------
# which factors are related to differ-ences in mean income in 
# the segment data? Specifically, is income related to homeownership, 
# or to segment membership, or both?
seg.aov.own<- aov(income~ownHome , data = seg.df)
seg.aov.own
anova(seg.aov.own)
# Pr(>F) : there is signifi-cant variation inincomebetween those who do and do not own their o

#income by segment
anova(aov(income~Segment , data = seg.df))

# what ownHome and Segment are both significance but Segment is more important
# what if ownHome relates to income in some segments and not all of them?
anova(aov(income~Segment * ownHome, data = seg.df))
# but segment is more important


#---compare 2 models woth Anova
anova( aov(income~ownHome * Segment , data = seg.df)
      ,aov(income~Segment * ownHome , data = seg.df))
#pe(>F) = 0.83 : there is no significantly difference between 2 models
#  *tocompare 2 model, they shoud be nested. one should be in another


#----- Visialize Anova
install.packages("multcomp")
library(multcomp)
par(mar=c(6,10,2,2))
plot(glht(aov(income~ -1 + Segment, data = seg.df)))
# *do not forget to remove intercept
# This show mean and confidence interval


#----- Step wise Modeling with Anova
#building model with adding and removing variables
#backward (def) :start with large set of Vars and remove them
#forward: adding variables one by one
step(aov(income~. , data = seg.df))
# removes item to reach Segment


#-------Bayes Anova----
#Is this hypothesis likely to betrue?"
#"How much confidence should I have?" 
# "What are the most likely val-ues?"
install.packages("BayesFactor")
library(BayesFactor)
set.seed(96761)
seg.bf1<-lmBF(income~Segment,data = seg.df)
seg.bf1
