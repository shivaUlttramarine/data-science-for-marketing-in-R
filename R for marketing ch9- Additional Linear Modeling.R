###############################################################
################## Comparing Groups ###########################
###############################################################


################## 1. Handling highly correlated Variables:
cust.df<- read.csv("D:/Shiva/R docs/rintro-chapter4.csv")
str(cust.df)
summary(cust.df)
str(subset(cust.df[,-1],online.spend>0))
str(cust.df)


spend.ml <- lm(online.spend ~ . ,data = subset(cust.df[,-1],online.spend>0))
# * having 0 online.spend got different factors and must be assessed 
#   another way. so we ommit them
summary(spend.ml)
library(gpairs)
gpairs(cust.df)


#---------BOXCOX
# ocefficients are so big then we needed scaling and boxcox
install.packages("forecast")
autoTransform <- function(x) {
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}
par(mfrow= c(1,2))
hist(cust.df$online.trans)
hist(BoxCox(cust.df$online.trans, BoxCox.lambda(cust.df$online.trans)))

cust.df.bc <- cust.df[complete.cases(cust.df), -1]
cust.df.bc <- subset(cust.df.bc, online.spend > 0)
numcols <- which(colnames(cust.df.bc) != "email")
cust.df.bc[ , numcols] <- lapply(cust.df.bc[ , numcols], autoTransform )
gpairs(cust.df.bc)
summary(cust.df.bc)

cust.df.bc$email <- factor(cust.df.bc$email)

cust.df.bc$stor <- NaN
cust.df.bc <- cust.df.bc[,c(1:11)]
spend.m2<- lm(online.spend ~ . , data=cust.df.bc)
summary(spend.m2)
anova(spend.ml, spend.m2)
#p-value is high and we can not reject it


#########
# variance inflation factor (VIF)
library(car)
vif(spend.ml)
#----vif>5 => collinearity

############## removing Collinearity:
# 1. ommiting vars that are highly correlated
# 2. Extacting principal components
# 3. use othe methods wich are robust to linearity like forest
# 4. creating a measur based on them. ex) transaction and spend are 
#    correlated. mix them: #spend per transaction
######################################

### ommiting vars that are highly correlated
spend.m3 <-lm(online.spend ~ . -online.trans -store.trans
              ,data = cust.df.bc)
summary(spend.m3)
anova(spend.m2,spend.m3)

####### PCA
cust.df.bc
summary(cust.df.bc)
pca<-prcomp(cust.df.bc[,c(1,2,4,5,6,7,8,9)])
pca
str(pca)
plot(pca)
biplot(pca,expand=5 , xlim=c(-0.6, 0.1), ylim=c(-0.4, 0.2))
#[online visit-onlinetrans-online spend]
#[sidance to stor]
#[age-credit,score]


#----- EFA
install.packages("nFactors")
library(nFactors)
nScree(cust.df.bc[,c(1,2,4,5,6,7,8,9)])

install.packages("GPArotation")
library(GPArotation)
efa <-factanal(cust.df.bc[,c(1,2,4,5,6,7,8,9)]
               ,factors = 3,rotation = "oblimin")
efa
library(gplots)
library(RColorBrewer)
heatmap.2(efa$loadings
          ,col = brewer.pal(9,"Greens")
          ,trace = "none", key = "FALSE" , dend = "none"
          ,colv=FALSE, cexCol = 1.2
          ,main="\n\n\n\n\nFactor loadings for brand adjectives")
#[online trans- online spend-online visit]
#[stor trans stor spend]
#[age-credit,score]
#[sidance to stor]


#--- to get the first component
pc.online<-prcomp(cust.df.bc[,c("online.visits","online.trans")])
cust.df.bc$online <- pc.online$x[,1]
cust.df.bc
head(cust.df.bc)

pc.store<-prcomp(cust.df.bc[,c("store.trans","store.spend")])
pc.store
cust.df.bc$store <- pc.store$x[,1]
colnames(cust.df.bc)


cust.df.bc$stor<NULL
cust.df.bc <-cust.df.bc[,c(1:12,14)]
colnames(cust.df.bc)

#-- model with no collinearity
spend.m4 <- lm(     online.spend ~  age +credit.score+ email+ distance.to.store
                  + sat.service +sat.selection + online + store
                , data = cust.df.bc)
summary(spend.m4)

spend.m5 <- lm(     online.spend ~  age + email
                    +  online 
                    , data = cust.df.bc)
summary(spend.m5)

plot(cust.df.bc$online.spend , cust.df.bc$age)



################## 2.Logestic Regression
# for questions like "Did he...? YESY/No"
# p(y)   =  e^y / (e^y + 1)
# y < 0  =>  p < 50% 
# y = 0  =>  p = 0
# y > 0  =>  p > 50%
# * plogis(2) = 0.88
# * log(0.88 / (1-0.88)) = 2
# * qlogis(0.88) = 2

pass.df<- read.csv("D:/Shiva/R docs/rintro-chapter9.csv")
pass.df$Promo <- factor(pass.df$Promo, levels = c("NoBundle", "Bundle"))
summary(pass.df)

# * GLM : you can feed not normal distribution vars into it
# * here the outcome is family = binomial
# * default link function of binomial is = logit. to change it 
#   we can say: family = binomial (link= "probit)


#Does promotion have effect on seasonal sale?
pass.ml <- glm(Pass~Promo , data = pass.df , family = binomial)
summary(pass.ml)
plot( pass.df$Promo , pass.df$Pass  )

# promBundle coefficient = 0.38879, means:
plogis(0.3888)/( 1 - plogis(0.3888))
exp(0.388)
exp(coef(pass.ml))
# sutomers are 1.475209 more likely to buy pass when it's offered in a bundle
# or
# Bundle increases the purchase liklihood by 47.5%

#---CI of odds ratio:
exp(confint(pass.ml))
# [1.28 - 1.69] significance positive effect


#-----------------------
library(vcd)
doubledecker(table(pass.df))
table(pass.df)


# cosider chanel in the model as well
pass.m2 <- glm(Pass~Promo +Channel,data = pass.df,family = binomial)
summary(pass.m2)
exp(coef(pass.m2))


# cosider interaction of chanel in the model as well
pass.m3 <- glm(Pass~Promo +Channel + Promo:Channel ,data = pass.df,family = binomial)
summary(pass.m3)
exp(confint(pass.m3))



#################### 3. Hirarchical Linear Model
# Individual level  effects
# Which subs are more interested in product / service?
# who among them wants which feature?
# who are most sensetive to price?

conjoint.df<- read.csv("D:/Shiva/R docs/rintro-chapter9conjoint.csv")
summary(conjoint.df)
conjoint.df

#how that 4 features efect ratins
by(conjoint.df$rating,conjoint.df$resp.id,mean)
colnames(conjoint.df)
conjoint.df$speed <- factor(conjoint.df$speed)
conjoint.df$height <- factor(conjoint.df$height)
conjoint.df$const <- factor(conjoint.df$const)
conjoint.df$theme <- factor(conjoint.df$theme)

ride.lm <-lm(rating  ~ speed  + height  + const  + theme,data = conjoint.df)
summary(ride.lm)

#### issue#1: 
# intercept(steel&dragon) + speed70 + height300
#            3.07         +   4.49  +  2.94     =  10.46: that is not right

#### issue#2:
#  constWood   -0.11826    0.11191  -1.057    0.291    
#  are people indifferent about constructor? are they are so equals that
#   cancels out when averaged?
histogram(~conjoint.df$const |conjoint.df$rating  )
histogram(~conjoint.df$rating | conjoint.df$const  )


# hlm allows individual to vary just in Intercept. when we expect some to rete 
# higher or lower than averarge in struct=>this would be an individual random
# effect for the intercept
# +(random effect | grouping) : +(1|resp.id) : uniq intercept term for each resp,id


library(lme4)
ride.hlm1 <-lmer(rating ~ speed + height + const + theme 
                 + (1|resp.id) , data = conjoint.df)
summary(ride.hlm1)
plot(ride.hlm1)
plot(ride.lm)

#---fix effect:
fixef(ride.hlm1)
# this the same as linear model. it s also available in summary(ride.hlm1)


#--random effect for 200 respondants:
ranef(ride.hlm1)

#-- compelet effect of respondant:
head(coef(ride.hlm1)$resp.id)
# it shown 3.07 (fixed) + 0.65 (ranef) = 2.42 (coef) for resp.id#1

###----------------- Complete hirarchical Linear Model
# Estimates random effect param for every coefficient 
# last one create 8 fixed and 200 random effect of intercept
# this one create 8 fixed + 8*200 random effect

library(minqa)
ride.hlm2 <-  lmer(rating ~ speed + height + const + theme +
                          ( speed + height + const + theme | resp.id)
                  , data = conjoint.df
                  #, control=lmerControl(optimizer ='bobyqa' ,optCtrl=list(maxfun=1000000))
                  )

summary(ride.hlm2)
# when max|grad| > 0.01 => increase itteration 
# or use different optimazationas:
#  1.BOBYQA (minqa and nloptr package implementations)
#  2.Nelder-Mead (lme4, nloptr, and dfoptim package implementations)
#  3.nlminb from base R (from the Bell Labs PORT library)
#  4.L-BFGS-B from base R, via optimx (Broyden-Fletcher-Goldfarb-Shanno, via Nash)
head(ranef(ride.hlm2)$resp.id)
head(coef(ride.hlm2)$resp.id)




############# usage
### beside customer level use:
### store,country,geographic region,advertising campaign
###,advertising creative,channel,bundle, andbrand.
###############################################################
################## Comparing Groups ###########################
###############################################################


################## 1. Handling highly correlated Variables:
cust.df<- read.csv("D:/Shiva/R docs/rintro-chapter4.csv")
str(cust.df)
summary(cust.df)
str(subset(cust.df[,-1],online.spend>0))
str(cust.df)


spend.ml <- lm(online.spend ~ . ,data = subset(cust.df[,-1],online.spend>0))
# * having 0 online.spend got different factors and must be assessed 
#   another way. so we ommit them
summary(spend.ml)
library(gpairs)
gpairs(cust.df)


#---------BOXCOX
# ocefficients are so big then we needed scaling and boxcox
install.packages("forecast")
autoTransform <- function(x) {
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}
par(mfrow= c(1,2))
hist(cust.df$online.trans)
hist(BoxCox(cust.df$online.trans, BoxCox.lambda(cust.df$online.trans)))

cust.df.bc <- cust.df[complete.cases(cust.df), -1]
cust.df.bc <- subset(cust.df.bc, online.spend > 0)
numcols <- which(colnames(cust.df.bc) != "email")
cust.df.bc[ , numcols] <- lapply(cust.df.bc[ , numcols], autoTransform )
gpairs(cust.df.bc)
summary(cust.df.bc)

cust.df.bc$stor <- NaN
spend.m2<- lm(online.spend ~ . , data=cust.df.bc)
summary(spend.m2)
anova(spend.ml, spend.m2)
#p-value is high and we can not reject it


#########
# variance inflation factor (VIF)
library(car)
vif(spend.ml)
#----vif>5 => collinearity

############## removing Collinearity:
# 1. ommiting vars that are highly correlated
# 2. Extacting principal components
# 3. use othe methods wich are robust to linearity like forest
# 4. creating a measur based on them. ex) transaction and spend are 
#    correlated. mix them: #spend per transaction
######################################

#######  1. ommiting vars that are highly correlated
spend.m3 <-lm(online.spend ~ . -online.trans -store.trans
              ,data = cust.df.bc)
summary(spend.m3)
anova(spend.m2,spend.m3)

cust.df.bc
summary(cust.df.bc)
pca<-prcomp(cust.df.bc[,c(1,2,4,5,6,7,8,9)])
pca
str(pca)
plot(pca)
biplot(pca,expand=5 , xlim=c(-0.6, 0.1), ylim=c(-0.4, 0.2))
#[online visit-onlinetrans-online spend]
#[sidance to stor]
#[age-credit,score]


#----- EFA
install.packages("nFactors")
library(nFactors)
nScree(cust.df.bc[,c(1,2,4,5,6,7,8,9)])

install.packages("GPArotation")
library(GPArotation)
efa <-factanal(cust.df.bc[,c(1,2,4,5,6,7,8,9)]
               ,factors = 3,rotation = "oblimin")
efa
library(gplots)
library(RColorBrewer)
heatmap.2(efa$loadings
          ,col = brewer.pal(9,"Greens")
          ,trace = "none", key = "FALSE" , dend = "none"
          ,colv=FALSE, cexCol = 1.2
          ,main="\n\n\n\n\nFactor loadings for brand adjectives")
#[online trans- online spend-online visit]
#[stor trans stor spend]
#[age-credit,score]
#[sidance to stor]


#--- to get the first component
pc.online<-prcomp(cust.df.bc[,c("online.visits","online.trans")])
cust.df.bc$online <- pc.online$x[,1]


pc.store<-prcomp(cust.df.bc[,c("store.trans","store.spend")])
cust.df.bc$store <- pc.store$x[,1]
colnames(cust.df.bc)


cust.df.bc$stor<NULL
cust.df.bc <-cust.df.bc[,c(1:12,14)]
colnames(cust.df.bc)

#-- model with no collinearity
spend.m4 <- lm(     online.spend ~  age +credit.score+ email+ distance.to.store
                    + sat.service +sat.selection + online + store
                    , data = cust.df.bc)
summary(spend.m4)

spend.m5 <- lm(     online.spend ~  age + email
                    +  online 
                    , data = cust.df.bc)
summary(spend.m5)

plot(cust.df.bc$online.spend , cust.df.bc$age)



################## 2.Logestic Regression
# for questions like "Did he...? YESY/No"
# p(y)   =  e^y / (e^y + 1)
# y < 0  =>  p < 50% 
# y = 0  =>  p = 0
# y > 0  =>  p > 50%
# * plogis(2) = 0.88
# * log(0.88 / (1-0.88)) = 2
# * qlogis(0.88) = 2

pass.df<- read.csv("D:/Shiva/R docs/rintro-chapter9.csv")
pass.df$Promo <- factor(pass.df$Promo, levels = c("NoBundle", "Bundle"))
summary(pass.df)

# * GLM : you can feed not normal distribution vars into it
# * here the outcome is family = binomial
# * default link function of binomial is = logit. to change it 
#   we can say: family = binomial (link= "probit)


#Does promotion have effect on seasonal sale?
pass.ml <- glm(Pass~Promo , data = pass.df , family = binomial)
summary(pass.ml)
plot( pass.df$Promo , pass.df$Pass  )

# promBundle coefficient = 0.38879, means:
plogis(0.3888)/( 1 - plogis(0.3888))
exp(0.388)
exp(coef(pass.ml))
# sutomers are 1.475209 more likely to buy pass when it's offered in a bundle
# or
# Bundle increases the purchase liklihood by 47.5%

#---CI of odds ratio:
exp(confint(pass.ml))
# [1.28 - 1.69] significance positive effect


#-----------------------
library(vcd)
doubledecker(table(pass.df))
table(pass.df)


# cosider chanel in the model as well
pass.m2 <- glm(Pass~Promo +Channel,data = pass.df,family = binomial)
summary(pass.m2)
exp(coef(pass.m2))


# cosider interaction of chanel in the model as well
pass.m3 <- glm(Pass~Promo +Channel + Promo:Channel ,data = pass.df,family = binomial)
summary(pass.m3)
exp(confint(pass.m3))



#################### 3. Hirarchical Linear Model
# Individual level  effects
# Which subs are more interested in product / service?
# who among them wants which feature?
# who are most sensetive to price?

conjoint.df<- read.csv("D:/Shiva/R docs/rintro-chapter9conjoint.csv")
summary(conjoint.df)
conjoint.df

#how that 4 features efect ratins
by(conjoint.df$rating,conjoint.df$resp.id,mean)
colnames(conjoint.df)
conjoint.df$speed <- factor(conjoint.df$speed)
conjoint.df$height <- factor(conjoint.df$height)
conjoint.df$const <- factor(conjoint.df$const)
conjoint.df$theme <- factor(conjoint.df$theme)

ride.lm <-lm(rating  ~ speed  + height  + const  + theme,data = conjoint.df)
summary(ride.lm)

#### issue#1: 
# intercept(steel&dragon) + speed70 + height300
#            3.07         +   4.49  +  2.94     =  10.46: that is not right

#### issue#2:
#  constWood   -0.11826    0.11191  -1.057    0.291    
#  are people indifferent about constructor? are they are so equals that
#   cancels out when averaged?
histogram(~conjoint.df$const |conjoint.df$rating  )
histogram(~conjoint.df$rating | conjoint.df$const  )

require(lattice)
histogram(~conjoint.df$rating |  conjoint.df$speed)
histogram(~conjoint.df$rating |  conjoint.df$speed + conjoint.df$const)


# hlm allows individual to vary just in Intercept. when we expect some to rete 
# higher or lower than averarge in struct=>this would be an individual random
# effect for the intercept
# +(random effect | grouping) : +(1|resp.id) : uniq intercept term for each resp,id


library(lme4)
ride.hlm1 <-lmer(rating ~ speed + height + const + theme 
                 + (1|resp.id) , data = conjoint.df)
summary(ride.hlm1)
plot(ride.hlm1)

#---fix effect:
fixef(ride.hlm1)
# this the same as linear model. it s also available in summary(ride.hlm1)


#--random effect for 200 respondants:
ranef(ride.hlm1)

#-- compelet effect of respondant:
head(coef(ride.hlm1)$resp.id)
# it shown 3.07 (fixed) + 0.65 (ranef) = 2.42 (coef) for resp.id#1


#---- confident interval
confint(ride.hlm1)
###----------------- Complete hirarchical Linear Model
# Estimates random effect param for every coefficient 
# last one create 8 fixed and 200 random effect of intercept
# this one create 8 fixed + 8*200 random effect

library(minqa)
ride.hlm2 <-  lmer(rating ~ speed + height + const + theme +
                     ( speed + height + const + theme | resp.id)
                   , data = conjoint.df
                   #, control=lmerControl(optimizer ='bobyqa' ,optCtrl=list(maxfun=1000000))
)

summary(ride.hlm2)

library(ggplot2)
ggplot(aes(rating,speed, data = conjoint.df)
       + geom_point()
       #+facet_wrap(~conjoint.df)
       )

colnames(conjoint.df)
(split_plot <- ggplot(aes(rating,speed),theme= theme, data = conjoint.df) + 
    geom_histogram() + 
    facet_wrap(~ const) + # create a facet for each mountain range
    xlab("rating") + 
    ylab("speed"))


# when max|grad| > 0.01 => increase itteration 
# or use different optimazationas:
#  1.BOBYQA (minqa and nloptr package implementations)
#  2.Nelder-Mead (lme4, nloptr, and dfoptim package implementations)
#  3.nlminb from base R (from the Bell Labs PORT library)
#  4.L-BFGS-B from base R, via optimx (Broyden-Fletcher-Goldfarb-Shanno, via Nash)
head(ranef(ride.hlm2)$resp.id)
head(coef(ride.hlm2)$resp.id)


library(ggplot2)
p1 <- ggplot(conjoint.df, aes(x = rating, y = speed, colour = const)) +
  geom_point(size=3) +
  geom_line(aes(y = predict(ride.lm)),size=1) 
print(p1)


p_hml <- ggplot(conjoint.df, aes(x = rating, y = speed, colour = const)) +
  geom_point(size=3) +
  geom_line(aes(y = predict(ride.hlm2)),size=1) 
print(p_hml)


predict(ride.hlm2 , c(10,10,70,300,'Wood','Eagle'))


p <- ggplot(conjoint.df, aes(x = rating, y = speed, colour = const)) +
  geom_point(size=3) +
  geom_line(aes(y = predict(fit2)),size=1) 
print(p)


#----predicttion
conjoint.df$predict_linear <- predict(ride.lm)
conjoint.df$predict_hlm1 <- predict(ride.hlm1)
conjoint.df$predict_hlm2 <- predict(ride.hlm2)

summary(ride.hlm1)
top.prefrences<-conjoint.df[conjoint.df$predict_hlm1>7,]
histogram(~top.prefrences$height | top.prefrences$speed,)
############# usage
### beside customer level use:
### store,country,geographic region,advertising campaign
###,advertising creative,channel,bundle, andbrand.
