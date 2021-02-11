store.df <- read.csv("D:/Shiva/R docs/rintro-chapter3.csv")


k.stores <- 20
k.weeks <- 104

store.df <- data.frame(matrix(NA, ncol=10, nrow=k.stores*k.weeks))
names(store.df) <- c("storeNum", "Year", "Week", "p1sales", "p2sales","p1price", "p2price", "p1prom", "p2prom", "countr")
store.num <- 101:(100+k.stores)
store.df$storeNum <- rep(store.num, each=k.weeks)

(store.cty <- c(rep("US", 3), rep("DE", 5), rep("GB", 3), rep("BR", 2),
                                rep("JP", 4), rep("AU", 1), rep("CN", 2)))
store.cty
store.df$country  <- rep(store.cty, each=k.weeks)
store.df$Week <- rep(1:52, times=k.stores*2)
        

store.df$p1prom <- rbinom(n=nrow(store.df), size=1, p=0.1)# 10% promoted of product 1 in all weeks
store.df$p2prom <- rbinom(n=nrow(store.df), size=1, p=0.15)# 15% promoted of product 2 1 in all weeks

store.df$p1price <- sample(x=c(2.19, 2.29, 2.49, 2.79, 2.99),size=nrow(store.df), replace=TRUE)
store.df$p2price <- sample(x=c(2.29, 2.49, 2.59, 2.99, 3.19),size=nrow(store.df), replace=TRUE)
     
rpois(nrow(store.df), lambda=120)

tmp.sales1 <- rpois(nrow(store.df), lambda=120)
tmp.sales2 <- rpois(nrow(store.df), lambda=100)
hist(rpois(nrow(store.df), lambda=20))

store.df$p1sales <- floor(tmp.sales1*(1 + store.df$p1prom*0.3))
store.df$p2sales <- floor(tmp.sales2*(1 + store.df$p2prom*0.4))

tmp.sales1
df<-data.frame(tmp.sales1)
df[1:4,1]

library(ggplot2)
ggplot(df,aes(x=1))+geom_histogram(binwidth =5)

store.df[1:9,]
str(store.df)

#----------------------------descrete var
str(store.df)
store.df$p1sales
table(store.df$p1sales)
p1.table<-table(store.df$p1sales)
plot(p1.table)

#----------------------------continues var
min(store.df$p1sales)
max(store.df$p1sales)
mean(store.df$p1sales)
median(store.df$p1sales)
var(store.df$p1sales)
sd(store.df$p1sales)
IQR(store.df$p1sales)
mad(store.df$p1sales)
quantile(store.df$p1sales, probs=c(0.25, 0.5, 0.75))
quantile(store.df$p1sales, probs=c(0.005, 0.5, 0.995))

#--------summary matrix
mysummary.df<-data.frame(matrix(NA,nrow = 2,ncol = 3))
names(mysummary.df)<-c("median","IQR","sd")
rownames(mysummary.df)<-c("product 1","product 2")
mysummary.df[1,1]<-median(store.df$p1sales)
mysummary.df[2,1]<-median(store.df$p2sales)

mysummary.df[1,2]<-IQR(store.df$p1sales)
mysummary.df[2,2]<-IQR(store.df$p2sales)

mysummary.df[1,3]<-sd(store.df$p1sales)
mysummary.df[2,3]<-sd(store.df$p2sales)

mysummary.df



summary(store.df)


install.packages("psych") #-Installing package ...> 
library(psych)
describe(store.df)
dim(store.df)


#1. Import your data withread.csv()or another appropriate function and checkthat the importation process gives no errors.

#2. Convert it to a data frame if needed (my.data <- data.frame(DATA)and set column names (names(my.data) <- c(...))if needed.

#3. Examinedim()to check that the data frame has the expected number of rowsand columns.

#4. Usehead()andtail(my.data)to check the first few and last few rows;make sure that header rows at the beginning 
#   and blank rows at the end werenot included accidentally. Also check that no good rows were skipped at thebeginning.

#5. Usesome()from thecarpackage to examine a few sets of random rows.

#6. Check the data frame structure withstr() to ensure that variable types andvalues are appropriate. 
#   Change the type of variables—especially tofactortypes—as
#   necessary.

#7. Runsummary()and look for unexpected values, especiallyminandmaxthatare unexpected.
#8. Load thepsychlibrary and examine basic descriptives withdescribe().
#  Reconfirm the observation counts by checking thatnis the same for each vari-able, and check trimmed mean 
#  and skew (if relevant).
   

#-------------------    
#apply(x=DATA,MARGIN=MARGIN, FUN=FUNCTIOTION)  #  1= rows - 2=cols  c(1,2)=both
apply(X =store.df[,4:9], MARGIN =  2, FUN =  mean)  # gives average for each column
store.df[1:5,4:9]
apply(X =store.df[1:5,4:9], MARGIN = 1, FUN =  mean) # gives average for each row

apply(X = store.df[,4:9] , MARGIN = 2, function(x) {median(x)-mean(x)})



#----------------Single Variable visualization---------------
hist(store.df$p1sales,main = "Product 1 weeklysales",xlab = "sales(unit)",ylab = "Count")

hist(store.df$p1sales,breaks = 35,col="red") # break = number of bars

hist(store.df$p1sales,breaks = 35,freq = FALSE,col="red") # freq=False: use density instead of count
hist(store.df$p2sales,breaks = 35,freq = FALSE,col='yellow') # freq=False: use density instead of count
lines(density(store.df$p1sales))
lines(density(store.df$p2sales))

hist(store.df$p2sales,breaks = 35,freq = FALSE,col='yellow') # freq=False: use density instead of count

boxplot(store.df$p1sales)
boxplot(store.df$p2sales ~ store.df$storeNum, horizontal =TRUE)  # box plot on eaxh storeum
boxplot(store.df$p2sales ~ store.df$p1prom, horizontal =TRUE)  # box plot on eaxh storeum

# --- qqplot ------qq plot to check normality
qqnorm(store.df$p1sales)
qqline(store.df$p1sales)
# end of the curve is far from the line and there for far from normality

# since marketing data is almost logarithmic wi test it like this:

plot(store.df$p1sales)
plot(store.df$p2sales)

plot(log(store.df$p1sales))
plot(log(store.df$p2sales))


qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))

#-----------------  Cumulitive Distibution
plot(ecdf(store.df$p1sales))
# to show 90% of sales
abline(h=0.9, lty=8) 
abline(v=quantile(store.df$p1sales, pr=0.9), lty=3)




########################  group by #######################
plsales.sum<-aggregate(store.df$p1sales , by=list(store.df$p1prom , store.df$storeNum ) ,mean)


########################  maps #######################
aggregate(store.df$p1sales, by= list(store.df$country),sum)
install.packages(c("rworldmap", "RColorBrewer"))
library(rworldmap)
library(RColorBrewer)
str(store.df)
p1sales.sum <- aggregate(store.df$p1sales,by=list(country=store.df$country), sum)
plsales.map <-joinCountryData2Map(store.df,joinCode = "ISO2",nameJoinColumn = "country")
mapCountryData(plsales.map, nameColumnToPlot="x",
               mapTitle="Total P1 sales by Country",
               colourPalette=brewer.pal(7, "Greens"),
               catMethod="fixedWidth", addLegend=FALSE)





###############################################################
###############################################################
######################### CHAPTER 4 ###########################
###############################################################



cust.df<- read.csv("D:/Shiva/R docs/rintro-chapter4.csv")
str(cust.df)
#remove(cust.df)
set.seed(21821)
ncust<-1000
cust.df<-data.frame(cust.id=as.factor(1:ncust))

cust.df$age<-rnorm(n = ncust,mean = 35,sd=1)
cust.df$creadit.score <- rnorm(n = ncust, mean = 3*cust.df$age+620,sd = 50)
cust.df$email<-factor(sample(c("yes","no"),size = ncust,replace = TRUE,prob = c(0.8,0.2)))
cust.df$dist.to.stor <-exp(rnorm(n = ncust, mean = 2,sd = 1.2))

cust.df$online.visits <- rnbinom(ncust, size=0.3,                                  
                          mu = 15 + ifelse(cust.df$email=="yes", 15, 0)
                          - 0.7*(cust.df$age-median(cust.df$age)))
#---- 30% of visits end in transaction
cust.df$online.trans<- rnbinom(ncust,cust.df$online.visits,prob = 0.3)

cust.df$online.spend <- exp( rnorm(n = ncust,mean = 3,sd = 1))
hist(cust.df$online.spend,breaks = 50)

sat.overall <- rnorm(ncust, mean=3.1, sd=0.7)
sat.service <- floor(sat.overall + rnorm(ncust, mean=0.5, sd=0.6))
sat.selection <- floor(sat.overall + rnorm(ncust, mean=-0.2, sd=0.6))
summary(cbind(sat.service, sat.selection))

sat.service[sat.service > 5] <- 5
sat.service[sat.service < 1] <- 1
sat.selection[sat.selection > 5] <- 5
sat.selection[sat.selection < 1] <- 1
summary(cbind(sat.service, sat.selection))


no.response <- as.logical(rbinom(ncust, size=1, prob=cust.df$age/1))
sat.service[no.response] <- NA                               
sat.selection[no.response] <- NA
cust.df$sat.service <- sat.service
cust.df$sat.selection <- sat.selection
cust.df
str(cust.df)
summary(cust.df)

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#---------  Explore Association between variables-------------
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#“Customers who live closer to our store visit more often than those
#who live fartheraway,” or “Customers of our online shop buy as much 
#in person at the retail shop asdo customers who do not purchase online.”
#if people who live closer to a store visitmore frequently and buy more, 
#then an obvious strategy would be to send advertise-ments to people who 
#live in the area.

plot(cust.df$age,cust.df$creadit.score)
abline(h=mean(cust.df$age),lty="dotted")
abline(v=mean(cust.df$creadit.score),lty="dotted")


#--does customers who shop online , shope less in stors ?
plot(cust.df$online.trans,cust.df$store.trans,cex=0.7)
plot(cust.df$online.spend,cust.df$store.spend,cex=0.7) #---cex=0.7: smallen the points


hist(cust.df$store.spend,
      breaks = (0:ceiling(max(cust.df$store.spend)/10))*10
     ,ain="Customers as of June 2014"
     ,xlab="Prior 12 months online sales ($)"
     ,ylab="Count of customers"
)


#!!!!!!!!!!!!!!!!!!!!!!!----- my Function to select random color----
func.random.color<-function(v) {
  num <- length (unique(v)) 
  color.name <- data.frame(colors())
  color_num  <- as.character( ceiling(runif(num,1,675)) ) 
  return(as.character( color.name[color_num,1] ))
}

test_num<-c(1,1,1,2,2,3,3,3,4,4,4,4,4,4,5,5,5,6,7,7,7,7,7,7,7,7,7)

color<-func.random.color(test_num)
color
typeof(color)
hist(test_num,col =color)
#----------------------------------------------------------------------
func.random.point<-function(v) {
  num <- length (unique(v)) 
  color_num  <-  ceiling(runif(num,1,25) ) 
  return(color_num)
}

#my.point<-
my.color<-func.random.color(cust.df$email)
my.pch<-func.random.point(cust.df$email)
my.pch
plot(cust.df$store.spend,cust.df$online.spend
     ,col=my.color[cust.df$email]
     ,pch=my.pch[cust.df$email]
     ,cex=0.7
    )
legend(x="topright",legend = paste("Email on file:",levels(cust.df$email))
       ,col = my.color,pch = my.pch)

#----------------------------------
# **** A common solution for such scatterplots with skewed data
#      is to plot the dataon alogarithmicscale
#      because both online and in-store sales are skewed,
#      we use a log scalefor both axes
#
#  * use + 1 to evoide error on log(0)=undefined
plot(cust.df$store.spend +1 ,cust.df$online.spend + 1
     ,col=my.color[cust.df$email]
     ,pch=my.pch[cust.df$email]
     ,cex=0.7
     ,log="xy"
)
legend(x="topright",legend = paste("Email on file:",levels(cust.df$email))
       ,col = my.color,pch = my.pch)

#---------multiple obj in plot
par(mfrow=c(2,2) )
plot(cust.df$distance.to.store,cust.df$store.spend,main="store")
plot(cust.df$distance.to.store,cust.df$online.spend,main="online")
plot(cust.df$distance.to.store+1,cust.df$store.spend+1,main="store",log="xy")
plot(cust.df$distance.to.store+1,cust.df$online.spend+1,main="online",log="xy")


#-----------Pairs----------------------------
names(cust.df)
pairs(formula =~  age + log(credit.score) + email + distance.to.store
 + online.visits + online.trans + log(online.spend) + store.trans + store.spend
 + sat.service + sat.selection,
 data=cust.df)
str(cust.df)

pairs(cust.df)
# above code and below one are the same. but in above code we can give it formulas
# log()
pairs(cust.df[ , c(2:10)])


#------- scatter plot matrix-------
install.packages("car")
library(car)
scatterplotMatrix(formula=~  age + credit.score + email + distance.to.store
                  + online.visits + online.trans + online.spend + store.trans + store.spend
                  + sat.service + sat.selection
                  ,data=cust.df
                  ,diagonal="histogram"
                  #,col=c('purple','blue','black') 
                  )

scatterplotMatrix(formula =~ age + credit.score + email 
                  +  distance.to.store + online.visits + online.trans 
                  + online.spend + store.trans + store.spend,
                  data=cust.df, diagonal="histogram"
                  ,col=c('purple','blue','black','yellow') )

#--------- tovisualize discrete variables better --------
#---------------------------gpairs------------------------
install.packages("gpairs")
library(gpairs)
gpairs(cust.df[,c(2:10)])


#----------------correlation coefficient----------------
cov(cust.df$age,cust.df$credit.score)
# it is difficult to interpret the magnitude of covariance 
# because the scaledepends on the variables involved
# therefore we use corellation
#
#pearson product-moment corelation coefficient
cor(cust.df$age,cust.df$credit.score)
# 0r
cov(cust.df$age,cust.df$credit.score)/ 
  ( sd(cust.df$age)*sd(cust.df$credit.score) )
#cohen rule:  
#  r=0.1:week or small (yet to be considered) 
#  r=0.3:medium
#  r>=0.5: large
# *This rules apply only when distribution is normal
#  otherwise we need to transfer variables with log(),...

#----- corelation Test
cor.test(cust.df$age,cust.df$credit.score)
#This tells us thatr=0.25 and the 95 % confidence interval
#isr=0.196−0.312.Because the confidence interval forrdoes 
#not include 0 (and thus hasp-value ofp<0.05), 
#the association is statistically significant. Such a 
#correlation, showing amedium-sized effect and
#statistical significance, probably should not be ignored 
#insubsequent analyses.

#----CORELLATION MATRIX------------
cor(cust.df[,c(2,3,5:12)])

install.packages("corrplot")
install.packages("gplot")
library(corrplot)
library(gplots)
corrplot.mixed(corr = cor(cust.df[ , c(2,3,5:12)])
               ,use="complete.obs"
               ,upper="ellipse"
               ,tl.pos = "lt"
               ,col=colorpanel(50,"red","gray60","blue4"))
corrplot.mixed(corr = cor(cust.df[ , c(2, 3, 5:12)]
                , use="complete.obs")
#                , upper="ellipse", tl.pos="lt"
#                , col = colorpanel(50, "red", "gray60", "blue4")
                
)


# ------------ transformation----------------
#unit sales,revenue, houshold income,price           : log(x)
#Distance                                            : 1/x    1/x^2  log(x)
#Market or preference share based on a utility value : e^x/  (1+e^x)
#right-tailed distribution (generally)               : squar(x) log(x)(what out for log(x)<0)
#left tailed  distribution x^2                       : x^2

plot(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)


# ------------ BOX-COX Transformation
library(car)
powerTransform(cust.df$distance.to.store)
l.dist <- coef(powerTransform(cust.df$distance.to.store))
l.spent<- coef(powerTransform(cust.df$store.spend+1))

par(mfrow=c(1,2))
hist(cust.df$distance.to.store)
hist(bcPower(cust.df$distance.to.store,l.dist))


par(mfrow=c(1,2))
hist(cust.df$store.spend)
hist(bcPower(cust.df$store.spend+1,l.spent))

cor(bcPower(cust.df$store.spend+1,l.spent), 
    bcPower(cust.df$distance.to.store,l.dist))

plot(bcPower(cust.df$store.spend+1,l.spent), 
        bcPower(cust.df$distance.to.store,l.dist))


powerTransform(cust.df$age) #1 =mean no corellation is needed

#-------------------- Satisfaction (Jitter)---------------------------
par(mfrow=c(1,2))
plot(cust.df$sat.service,cust.df$sat.selection)
plot(jitter(cust.df$sat.service) , jitter(cust.df$sat.selection))

#------------------  polychorid ----------------
