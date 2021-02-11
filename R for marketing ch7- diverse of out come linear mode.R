###############################################################
################ Diverse of outcome  ##########################
################# with linear model ###########################
###############################################################


# "satisfaction driversanalysis":
# that means to check satisfactory in relation to elements of product

#"morketing mix modeling:" how  price and advetising relates to sale

# *Driver does not imply causation:  there is posotove assosation between price
# and satisfaction. but should we rise price to improve satisfaction?
# no! price associated with higher qualityand that increases the satisfaction


# *  marketing research textbook, where it might appear undera name such asregression analysis,linear regression,orleast-squares fitting

sat.df <- read.csv("http://goo.gl/HKnl74")
sat.df<- read.csv("D:/Shiva/R docs/rintro-chapter7.csv")
sat.df

# 1. sumary
summary(sat.df)
#2.
gpairs::gpairs(sat.df)
sat.df$logdist<- log(sat.df$dist)
str(sat.df)
gpairs::gpairs(sat.df)


# A common issue with marketing data and especially 
# satisfac-tion surveys is that variables may be highly correlated
# with one another. then we do this:
corrplot::corrplot.mixed((cor(sat.df[,c(2,4:9)])) , upper = "ellipse")
# *non of the scors are identical.  they are<0.8


# The goal of this analysis:
# relationship od satisfaction with features of a service
plot(sat.df$overall~sat.df$ride , data=sat.df)

m1<-lm(overall~rides,data = sat.df)
plot(sat.df$overall~sat.df$ride , data=sat.df)
abline(m1,col='red')
str(m1)
summary(m1)
#Coefficients -> Estimate -> (Intercept): -94.9622
#  rides -> 9.0790 (coef)

# Coefficients -> Estimate -> (Intercept): -94.9622
# t value Pr(>|t|) : Waldtest :which assesses whether the coefficient 
#                              is significantly different than zero
# CI : ±1.96×std.error : 1.7033±1.96×0.1055 = (1.495,1.910)
# to report CI:
confint(m1)

#Residuals: line - point
# median = 0.425 : it s quit symmetric


# Residual standard error: 12.88 on 498 degrees of freedom = sd(m1$residuals)
# Multiple R-squared:  0.3434,	Adjusted R-squared:  0.3421
#     : 1/3 of variation in overall is explaind by variation of rides
# *    = cor(sat.df$overall, sat.df$rides)^2
    
# F-statistic: 260.4 on 1 and 498 DF,  p-value: < 2.2e-16
# H0: model with no predicator works better than this => rejected
# *   =equal to anova(m1)
anova(m1)


#------- Model assessment
############## ASUMPTIONS ###############
###    relationship it is not linear  ###
#########################################
x <- rnorm(500)
y<- x^2 + rnorm(500)
l2<-lm(y~x)
plot(y~x)
abline(l2)
summary(l2)
# coeficciant X  = -0.01
# and wald significance test = not different from zero
# true relationship is:
l3<-lm(y~sqrt(x))
plot(y~sqrt(x))
abline(l3)
summary(l3)


############## ASUMPTIONS ########################
### prediction Errors are normally distibuted  ###
##################################################
plot(l2$fitted.values,l2$residuals)
# this should not have any special pattern
par(mfrow = c(2,4))
plot(m1)
plot(l2)

# 1. upper left: resudual vs fitted. should have no patter
# 2. lower left: square root of residual vs fitted: there should be no pattern
#     observations with high residuals are flagged as potention outlires
#     *  heteroskedasticity: When Range of errorsgets progressively 
#        larger for larger fitted values
#        = violation of linear model assumptions
# 3. QQ plot: wheather residuals are normal
# 4. Lower right:potential outliers. leverage vs resuduals
#    When a point has High leverage and high residuals, it means it comes
#    form a different pattern and undue influnens
#   leverage is with Cook's distance
#    thosewith numbers are problematic 
#    removing outliers:
sat.df[c(57,129,395),]


#----------------------  multiple Predictors  -----------------------------
m2<-lm(overall~rides + games + wait + clean ,data = sat.df)
summary(m2)
# R-squered is improve => better plan
# residual standard error is also improved
plot(overall~rides + games + wait + clean ,data = sat.df)
install.packages("coefplot")
library(coefplot)
#importanceof factors:
coefplot(m2, intercept = FALSE
         ,outerCI = 1.96
         ,lwdOuter = 1.5
         ,ylab = "Rating of Feature"
         ,xlab = "Association with Overall Satisfaction" )
# it shows that cleannessis the most important factor
# andthen rides and wait.
m4<-lm(overall~clean + rides + games + wait  ,data = sat.df)
summary(m4)
coefplot(m4, intercept = FALSE
         ,outerCI = 1.96
         ,lwdOuter = 1.5
         ,ylab = "Rating of Feature"
         ,xlab = "Association with Overall Satisfaction" )


#-----  Comparing models ---------------
summary(m2)$r.squared
summary(m1)$r.squared

summary(m2)$adj.r.squared
summary(m1)$adj.r.squared

plot(  sat.df$overall , fitted(m1) 
     , col='red', xlim = c(0,100)
     , xlab="Actual Overall Satisfaction"
     , ylab="Fitted Overall Satisfactio")
points(sat.df$overall ,fitted(m2), col= 'blue')
points(sat.df$overall ,fitted(m4), col= 'green')
legend("topleft", legend=c("model 1", "model 2")
       ,col=c("red", "blue"), pch=1)
#the one which is morenear 45 degree and tighter is better.

# anova test:
anova(m2,m1)
# they are significantly different
# we take the one with less RSS


#-------predict
coef(m2) %*% c(1,100,100,100,100)
plot(l2)

#predit first 10 rows of our data
predict(m2,sat.df[1:10,])

#first 10 rows are saved in model eather
fitted(m2)[1:10]



#----------standardization
scale(sat.df$distance)  # (x-mean(x) ) / sd(x)
sat.std<-sat.df[-3]
str(sat.std)
sat.std [,3:8]<- scale(sat.std[3:8])
summary(sat.std)

m.s<- lm(overall~ rides + games + wait + clean , data = sat.std)
summary(m.s)

library(lattice)
histogram(~overall |  num.child , data = sat.df)

#------ Using Factors as preditors
m5<- lm(overall~ rides + games + wait + clean +weekend +logdist 
        + num.child , data = sat.std)
summary(m5)
# those who come on weekends are -0.047 sd less satisfied that 
# those who come o weekdays


#num.child should be a factor:
sat.std$num.child.factor <-factor(sat.std$num.child)
m6<- lm(overall~ rides + games + wait + clean +weekend +logdist 
        + num.child.factor , data = sat.std)
summary(m6)
plot(overall ~ num.child.factor, data = sat.std)
# num.child.factor2 is one full SD heigher that no children


#therse is gap between those who own a childand those who own not
# then we create a flag
sat.std$has.child <- factor(sat.std$num.child>0)
sat.std$num.child.factor <-factor(sat.std$num.child)
m7<- lm(overall~ rides + games + wait + clean +logdist 
        + has.child , data = sat.std)
summary(m7)
#it isnegliggble change


#-------------interaction
#Does wait time differes for those who has child? and those who have not?
m8<- lm(overall~ rides + games + wait + clean +logdist 
        + has.child
        + rides:has.child + games:has.child + wait:has.child +clean:has.child
        + rides:weekend    + games:weekend 
        , data = sat.std)
summary(m8)

#now drop non significance ones
m9<- lm(overall~ rides + games + wait + clean +logdist 
        + has.child
        + wait:has.child 
        , data = sat.std)
summary(m9)


#-------to share the result:
library(coefplot)
coefplot(m1,intercept = FALSE, outerCI = 1.96
         , lwdOuter=1.5
         , lab="Rating of Feature"
         , xlab="Association with Overall Satisfaction"
         )


#  * y???.-x: all except x
#  * X * y : x + y +x:y
#
#  1.data cleansing: convert to datafram - dim()-head()-tail()-some()
#         str() - summary() - psyvh::describe()    
#  2. check distribution: gpairs()
#     if not normal: cosider transforming:
#     Unit sales, revenue, household income,price: log(x)
#     Distance                                   : 1/x,1/x2,log(x)
#     Market or preference share based on autility value: ex/(1+ex)
#     Right-tailed distributions (generally)     : ???xor log(x)(watch out for log(x???0))
#     Left-tailed distributions (generally)      : x2
#     BOX-COX: y^lambda     lambda = 0   -> log(y)
#                           lambda <> 0  -> y^(lambda-1)/lambda
#             library (car)
#             lambda<- coef(powerTransform(1 / df$field))
#             bcPower(df$field,lambda)
#
#
#     #---------BOXCOX
#     ----- ocefficients are so big then we needed scaling and boxcox
#      install.packages("forecast")
#      autoTransform <- function(x) {
#        library(forecast)
#        return(scale(BoxCox(x, BoxCox.lambda(x))))
#      }
#      par(mfrow= c(1,2))
#      hist(cust.df$online.trans)
#      hist(BoxCox(cust.df$online.trans, BoxCox.lambda(cust.df$online.trans)))
#      
#      numcols <- which(colnames(cust.df.bc) != "email")
#      cust.df.bc[ , numcols] <- lapply(cust.df.bc[ , numcols], autoTransform )
#
#  3. check for extremly correlated ones:r>0.9 r>0.8:
#     if so ommir them or transform them:  gpairs()
#     OR
#     library(corrplot)
#     corrplot(cor(brand.sc[,1:9]),order="hclust")
#
#  4. Standardize the data scale()
#  5. ml<- lm(y~x)
#     plot(y~x)
#     abline(ml)
#  5. Check the resuduals:
#     plot(toy.model$fitted.values, toy.model$residuals)l
#     par(mfrow=c(2,2))
#     plot(m1)  
#  6. Try other Models and compare them with ANOVA
#  7. predict(c(v1,v2,v3,...vn))
#  8. make test set and train set: avoid bias
#  9. Use factor() for dimensions
#  10.to show intreaction use x:y is product of 2 predictors
#  11.Model building is the process of adding and removing predictors
#     from a modelto find a set of predictors that fits the data well.
#     We can compare the fit of dif-ferent models using the R-squared
#     value or, if models are nested (see Sect.6.5)by using the more
#     formal ANOVA test (anova())
#  12.Predict :  conjoint.df$predict_linear <- predict(ride.lm)
  
#####------learn more
# about marketing: churn etc: owman, D., & Gatignon, H. (2010).Market response and marketing mixmodels. Foundations and Trends in Marketing. Boston: Now Publishers Inc.
# about vars with posion binomial and etc use glm:
#      Dobson, A. J. (2008).An introduction to generalized linear models(3rd ed.).Boca Raton: Chapman & Hall

corona.df <- data.frame( n=c(139,245,388,593,978,1501,2336,2922,3613,4747,5823,6566,7161,8042,9000))
corona.df
str(corona.df)
library(car)
lmbd <- coef(powerTransform(corona.df$n))
corona.df$n.trns <-bcPower(corona.df$n , lmbd)
corona.df$n.log<- log(corona.df$n)
corona.df$sqr <- sqrt(corona.df$n)
par(mfrow=c(1,4))
plot(corona.df$n)
plot(corona.df$n.trns)
plot(corona.df$n.log)
plot(corona.df$sqr)
x<-c(1:length(corona.df$n))
x
model <- lm(corona.df$sqr~x)
plot(corona.df$sqr~x)
abline(model,col='red')

par(mfrow=c(2,2))
plot(model)
corona.df$back<-NaN
corona.df
lambda
powerTransform(corona.df$n)
#--------------------------

