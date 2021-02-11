##################################################################
#####################                    #########################
#####################   Choice Modeling  #########################
#####################                    #########################
##################################################################
#--------------------------------------------------------------

cbc.df <- read.csv("D:/Shiva/R docs/rintro-chapter13conjoint.csv")
cbc.df$seat <- factor(cbc.df$seat)
cbc.df$price <- factor(cbc.df$price)

summary(cbc.df)
str(cbc.df)
head(cbc.df)

# compute Choice counts:
xtabs(choice ~ ques + alt, data = cbc.df)

xtabs(choice ~ price , data = cbc.df)

xtabs(choice ~ cargo , data = cbc.df)


#------------- reformat data into long
install.packages("mlogit")
library(mlogit)
cbc.df 

cbc.mlogit <- mlogit.data(data = cbc.df , choice = "choice"
                          ,"long" ,  varying = 3:6 , alt.levels = paste0("pos",1:3) 
                          ,id.var = "resp.id")

head(cbc.mlogit)

m1 <- mlogit(choice ~ 0  # means we do not wat intercept. that means 1 pos,2 pos,3 pos of question
             + seat + cargo + eng + price
             , data = cbc.mlogit)
summary(m1)
# Estimate: mean value. must be interpreted relative to best leve of each attrib:
#   seat7    -0.535280 
#   seat8    -0.305840
#   seat7 means comparison between 7 seat and 6 seat. negative means: 6 seat is pereffered.
#   
#   engelec  -1.530762  :  strongly dislike electric relative to base which is gas
#   enghyb   -0.811282 
#   
#   price35  -0.913656  : distlike 35 relative to base=30
#   price40  -1.725851
#
# Std.Err: How accurate is the data. more data => les std

