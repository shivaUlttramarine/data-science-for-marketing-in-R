###############################################################
###########   Confirmatory Factor Analysis ####################
###########               &                ####################
###########   Structural Equation Modeling ####################
###############################################################
#--------------------------------------------------------------
# covariance structural Analysis
# Analysis of Moment Structure
# LISERAL: linear structural Rrlationship
# Causal Modeling

# If the goal is predicting key target constructs or identifying key 'driver' constructs, select PLS-SEM.
# If the goal is theory testing, theory confirmation, or comparison of alternative theories, select CB-SEM.
# If the research is exploratory or an extension of an existing structural theory, select PLS-SEM.”



piesSimData <- read.csv("D:/Shiva/R docs/rintro-chapter10pies.csv")
summary(piesSimData)
str(piesSimData)

library(car)
library(RColorBrewer)
scatterplotMatrix(piesSimData[, c(1, 2,3, 4, 5, 8, 9)], diag="histogram",
                  col=brewer.pal(3, "Paired"), ellipse=TRUE )

# do a efa:
factanal(piesSimData,factors = 3)





#----modeling
library(lavaan)
piesModel <- " General =~i1+i2+i3
                Feature =~i4+i5+i6 +i7
                Image   =~i8 + i9 + i10 + i11
                PIES =~General + Feature + Image "

pies.fit <- cfa(piesModel, data=piesSimData)

summary(pies.fit,fit.measures = TRUE)
#### these 2 means that model fits the data well
#  Comparative Fit Index (CFI)                    0.975
#  Tucker-Lewis Index (TLI)                       0.966

#residuals are low:  RMSEA = 0.041
# model paths are significant:  p(>|z|) = 0
# estimates are 0.822 to 1.035 ~ 1: they are similar to one another

library(semPlot)
semPaths(pies.fit, what="est"
         ,fade=FALSE ,residuals = FALSE, edge.label.cex = 0.75)




################################
# after creating a model, no matter how good it loks we should test it
# with 1.a simpler and 2.a more complicated one
#
#------------- a simpler methos:
piesModelNH1 <- " PIES =~ i1 + i2 + i3 + i4 + i5 + i6 + i7+ i8  + i9 + i10 + i11"
pies.fit.NH1 <- cfa(piesModelNH1,data = piesSimData)
summary(pies.fit.NH1,fit.measures = TRUE)
summary(pies.fit.NH1)
#------------- a more complicated:
# cince it hase exactly same number of relations and factors with main Model
# wi needto diffrentiate it, we need to constrian latent Variables correlation
# to a small value, such thatthey are not part of a hirarchi.
# Zerois unreasonable then we use 0.1
piesModelNH3 <- " General =~ i1 + i2 + i3
 Feature =~ i4 + i5 + i6 +i7
 Image   =~ i8 + i9 + i10+ i11
 General ~~ 0.1*Feature
 General ~~ 0.1*Image
 Feature ~~ 0.1*Image "
pies.fit.NH3 <- cfa(piesModelNH3,data = piesSimData)
summary(pies.fit.NH3,fit.measur=TRUE)

#----------- compare all this models now
install.packages("semTools")

library(semTools)
compareFit(pies.fit,pies.fit.NH1,pies.fit.NH3)
# Model Fit Indices:
# Best CFI and lowest RMSEA belongs to pies.fit(main model) - the one with dagger
# Nested Model Comparison:
# chisq shows significance different


#-------------------covariance-based SEM
################################### Questions to answer:
# how much does perception of quality relate to satisfaction?
# Is quality more importantthan perceived value?
# What is the largest determinant of stated intent to purchaseagain?
# And so forth


#################################### Creating simulated data by a model:
satDataModel <- " Quality =~0.59*CSat + 0.56*Value + 0.9*q1 + 0.9*q2 + 0.9*q3 + 0*Cost
                  Cost    =~-0.5*Value + -0.29*Repeat + 0.9*c1 + 0.9*c2 + 0.9*c3
                  Value   =~0.06*CSat + 0.9*v1 + 0.9*v2 + 0.9*v3
                  CSat    =~0.48*Repeat + 0.9*cs1 + 0.9*cs2 + 0.9*cs3
                  Repeat  =~0.9*r1 + 0.9*r2 + 0.9*r3 "
set.seed(33706)# continuing the island tour
satData.norm <- simulateData(satDataModel, sample.nobs=200)
satSimData <- data.frame(lapply(satData.norm,function(x) { as.numeric(cut(x, breaks=7)) } ))
                                
                                
##################################### or just upload it:
satSimData <- read.csv("D:/Shiva/R docs/rintro-chapter10sat.csv")
summary(satSimData)

satModel <- " Quality =~CSat + Value + q1 + q2 + q3  + 0*Cost
 Cost    =~Value + Repeat + c1 + c2 + c3
 Value   =~CSat + v1 + v2 + v3
 CSat    =~Repeat + cs1 + cs2 + cs3
 Repeat  =∼r1+r2+r3 "
# first line : Quality influences CSat and Value, and is manifested as items q1, q2, and q3.
# Quality ~  0*Cost   :  that means there is no relationshipd 0*Cost

sat.fit <-sem(satModel,data = satSimData,std.lv=TRUE)
summary(sat.fit,fit.measure = TRUE)

semPaths(sat.fit , what = "est" , fade = FALSE , residuals = FALSE,
         layout = "tree" , structural = TRUE, nCharNodes = 7, edge.label.cex = 1)


#------ simpler model
satAltModel <- " Quality =~CSat + Value + q1 + q2 + q3  + 0*Cost
 Cost    =~Value  + c1 + c2 + c3
Value   =~CSat + v1 + v2 + v3
CSat    =~Repeat + cs1 + cs2 + cs3
Repeat  =∼r1+r2+r3 "


sat.alt.fit <-sem(satAltModel, data = satSimData , std.lv=TRUE)
compareFit(sat.fit,sat.alt.fit)






################################################
######### Partial Least Squesrs  ###############
################ PLS-SEM #######################
################################################
# when we do not have enough tdata, SEM fials:
set.seed(90704)

satSimData2 <- satSimData[sample(nrow(satSimData), 50),]
summary(satSimData2)
nrow(satSimData2)
sat.fit2 <- sem(satModel,data=satSimData2,std.lv=TRUE)
summary(sat.fit2,fit.measure=TRUE)

# :) actually heare the results are ok!



satPLSmm <- matrix(c(
  "Quality", "q1",
  "Quality", "q2",
  "Quality", "q3",
  "Cost",    "c1",
  "Cost",    "c2",
  "Cost",    "c3",
  "Value",   "v1",
  "Value",   "v2",
  "Value",   "v3",
  "CSat",    "cs1",
  "CSat",    "cs2",
  "CSat",    "cs3",
  "Repeat",  "r1",
  "Repeat",  "r2",
  "Repeat",  "r3" ), ncol=2, byrow=TRUE)


satPLSsm <- matrix(c(
  "Quality", "CSat",
  "Quality", "Value",
  "Cost",    "Value",
  "Cost",    "Repeat",
  "Value",   "CSat",
  "CSat",    "Repeat" ), ncol=2, byrow=TRUE)
install.packages("semPLS")
library(semPLS)
sat.pls.mod <-plsm(data = satSimData2 , strucmod = satPLSsm, measuremod = satPLSmm )
satPLS.fit <-sempls(model = sat.pls.mod, data = satSimData2)
plsLoadings(satPLS.fit)

pathCoeff(satPLS.fit)
#----- visualize
pathDiagram(satPLS.fit,file ="satPLSstruct" , full = FALSE , digits = 2
            , edge.labels = "values" , output.type = "graphics", graphics.fmt = "pdf")

#-- assesment
rSquared(satPLS.fit) 
#R2>0.09 could be a reasonablegoal for a moderately strong association
#but its not a realy good aproach. this one is better


set.seed(04460)
satPLS.boot <-bootsempls(satPLS.fit,nboot = 500,start = "ones")
#There were 86 apparent convergence failures;   :  it s so good
summary(satPLS.boot)
parallelplot(satPLS.boot,reflinesAt = 0,alpha=0.8)
# red solid line = median
# red dotted line = 95% interval
# gray = individuals
# 1=Cost   2=Quality  3=Value   
# 4=cSat   5=Repeat
# Cost and repeat(1-5) is strongly negetive but some hold the relationship strongly Positive!
# 2 of6 is range stradle zero

# if numbers sample are more--= lenght of graph will be narrower
