# Classification Trees #
# Adopted problem #
# Dataset : Carseats 
# Problem : A simulated data set containing sales of child car
# seats at 400 different stores.
# A data frame with 400 observations on the following 11 variables.
####################################################################
#  11 variables Description
# Sales         #: Unit sales (in thousands) at each location
# CompPrice     #: Price charged by competitor at each location
# Income        #: Community income level (in thousands of dollars)
# Advertising   #: Local advertising budget for company at each location
#                  (in thousands of dollars)
# Population    #: Population size in region (in thousands)
# Price         #: Price company charges for car seats at each site
# ShelveLoc     #: A factor with levels Bad, Good and Medium indicating 
#                  the quality of the shelving location for the car seats at each site
# Age           #: Average age of the local population
# Education     #: Education level at each location
# Urban         #: A factor with levels No and Yes to indicate 
#                  whether the store is in an urban or rural location
# US            #: A factor with levels No and Yes to indicate whether the store is in the US or not
####################################################################


library (tree) # install packages if required
library (ISLR) # install packages if required
attach (Carseats )
####################################################################
# Create derived variable High using Sales and merge it with Carseats
####################################################################
High=ifelse (Sales <=8," No"," Yes ")
Carseats =data.frame(Carseats ,High)
####################################################################
tree.carseats =tree(High~.-Sales ,Carseats )
summary (tree.carseats )
plot(tree.carseats )
text(tree.carseats ,pretty =0)
tree.carseats
#split the observations into a training set and a test

set.seed (2)
train=sample (1: nrow(Carseats ), 200)
Carseats.test=Carseats [-train ,]
High.test=High[-train ]
#build the tree using the training set, and evaluate its performance on
#the test data
tree.carseats =tree(High~.-Sales ,Carseats ,subset =train )
tree.pred=predict(tree.carseats ,Carseats.test ,type ="class")
table(tree.pred ,High.test)
(86+57)/200
set.seed (3)
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats )
cv.carseats

par(mfrow =c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")
prune.carseats =prune.misclass (tree.carseats ,best =9)
plot(prune.carseats )
text(prune.carseats ,pretty =0)

tree.pred=predict (prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)

(94+60) /200

prune.carseats =prune.misclass (tree.carseats ,best =15)
plot(prune.carseats )
text(prune.carseats ,pretty =0)
tree.pred=predict (prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)

(86+62) /200

