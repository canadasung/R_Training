#Pre-load Libraries
library(RcmdrPlugin.BCA)
library(BCA)
library(ggplot2)
library(rpart)
library(RcmdrMisc)
library(nnet)
library(rpart)
library(rpart.plot)
library(foreign)
library(flexclust)

# For Drawing Nicer Classification and Regression Trees
# See: http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script



# load packages, if missing run 
#install.packages("RcmdrPlugin.BCA")
#install.packages("gmodels")
#install.packages("eeptools")
#library("RcmdrPlugin.BCA")
#library("BCA")
#library("RcmdrMisc")
#library("eeptools") # for age difference
#library("gmodels")
#library("foreign")  # for read.dbf function
# to create and plot the classification tree
#library("rpart")
#library("rpart.plot")


#----- CHAPTER 3 -----
# Read Data

#jack.jill <- read.csv(file.choose())
jack.jill <- read.table("C:/Users/SUNGI7/OneDrive/SFU/BUSINESS/400/BUS445/LectureSlides/Week1/jackjill.csv", 
                        header = TRUE, sep=",", na.strings = "NA", dec = ".", strip.white = TRUE)
summary(jack.jill)
?read.csv
jack2 <- read.table("C:/Users/SUNGI7/OneDrive/SFU/BUSINESS/400/BUS445/LectureSlides/Week1/jackjill.csv", 
                    header = TRUE, sep=",", na.strings = "NA", dec = ".", strip.white = TRUE)
data(CCS, package = "BCA")



# Data Summary
summary(jack2)
head(jack2)
head(jack.jill)


?data()
data(CCS, package = "BCA")


# Remove ID
jack.jill$HH.ID <- NULL

variable.summary(jack.jill)
?variable.summary()
summary.data.frame(jack.jill)


# Numerical Summary
numSummary(jack.jill[,"Spending"], statistics=c("mean","sd","IQR", "quantiles"), quantiles = c(0, .25, .5, .75,1))
?numSummary()
summary(jack.jill$Spending)
IQR(jack.jill$Spending)


# Numerical Summary by Factor Group
numSummary(jack.jill[,"Spending"], 
           groups = jack.jill$Children, 
           statistics=c("mean","sd","IQR", "quantiles"), 
           quantiles = c(0, .25, .5, .75,1))
numSummary(jack.jill[,"Spending"], 
           groups = jack.jill$Income, 
           statistics=c("mean","sd","IQR", "quantiles"), 
           quantiles = c(0, .25, .5, .75,1))
numSummary(jack.jill[,"Spending"], 
           groups = jack.jill$Employment, 
           statistics=c("mean","sd","IQR", "quantiles"), 
           quantiles = c(0, .25, .5, .75,1))

# Correlation Analysis
?cor()
CCS
cor(CCS[, c("AveDonAmt", "DonPerYear", "LastDonAmt", "YearsGive")], use = "complete.obs")


# Histogram
?hist()
?Hist()
jack.jill$Spending
hist(jack.jill$Spending, breaks = 60, col = "darkgray", xlab = "Spending")
Hist(jack.jill$Spending, breaks = 60, scale = "frequency" , col = "darkgray", xlab = "Spending")
Hist(jack.jill$Spending, breaks = 60, scale = "percent" , col = "darkgray", xlab = "Spending")
Hist(jack.jill$Spending, breaks = 60, scale = "density" , col = "darkgray", xlab = "Spending")
ggplot(data = jack.jill, aes(x=Spending)) + 
  geom_histogram(binwidth = 60, colour = "Black")
ggplot(data = jack.jill, aes(x=Spending)) + 
  geom_histogram(binwidth = 60, aes(fill = New_Children),
                 colour = "Black")
ggplot(data = jack.jill, aes(x=Spending)) + 
  geom_histogram(binwidth = 60, aes(fill = Birth.Country),
                 colour = "Black")


# Create Bins; binVariable() == bin.var()
?bin.var()
jack.jill$Spend.Cat <- bin.var(jack.jill$Spending, bins = 3,
                               method = "proportion",
                               labels = c("Low","Medium","High"))
#jack.jill$Spend.Cat <- bin.var(jack.jill$Spending, bins = 3,
#                               method = "proportion",
#                               labels = FALSE)

jack.jill$Spend.Cat
showData(jack.jill, placement = "-20+200", font = getRcmdr("logFont"), maxwidth=80, maxheight = 30)


# Frequency Distributions
?table()
freq1 <- table(jack.jill$Spend.Cat)
freq1 # counts for Spend.Cat

?round()
round(100 * freq1 / sum(freq1), 2) # percentages for Spend.Cat

rm(freq1)

# Bar Plot
?barplot()
?table()
table(jack.jill$Children)
barplot(table(jack.jill$Children), xlab = "Children", ylab = "Frequency")
ggplot(data = jack.jill, aes(x=New_Children)) + 
  geom_bar()


# wrong approach
hist(table(jack.jill$Children), breaks = 3)
hist(table(jack.jill$Children), breaks = 3, plot = FALSE)

# Relabel Factor Variable: Children
?relabel.factor()
jack.jill$Children
jack.jill$New_Children <- relabel.factor(jack.jill$Children, 
                                         new.labels = c("1 Child", "2 Children", "3+ Children", "3+ Children"))
jack.jill$New_Children

?droplevels()

barplot(table(jack.jill$Children), xlab = "Children", ylab = "Frequency")
barplot(table(jack.jill$New_Children), xlab = "Children", ylab = "Frequency")


# Contingency Table
?xtabs()
cont1 <- xtabs(~Spend.Cat+New_Children, data = jack.jill)
cont1
?colPercents()
colPercents(cont1)
rowPercents(cont1)
?chisq.test()
chi1 <- chisq.test(cont1, correct = FALSE)
#chi1 <- chisq.test(cont1, correct = TRUE)
chi1
chi1$expected
chi1$observed
chi1$p.value

cont2 <- xtabs(~Spend.Cat+Income, data = jack.jill)
colPercents(cont2)
chi2 <- chisq.test(cont2, correct = FALSE)
chi2
chi2$expected
chi2$observed
chi2$p.value

#Chagne the order of factor
?factor()
jack.jill$Income <- factor(jack.jill$Income, levels = c('$0-$20k', '$20k-$30k', '$30k-$40k', '$40k-$50k','$50k-$60k','$60k-$75k','$75k-$100k','$100k+'), ordered = TRUE)

cont2 <- xtabs(~Spend.Cat+Income, data = jack.jill)
colPercents(cont2)
chi2 <- chisq.test(cont2, correct = FALSE)
chi2
chi2$expected
chi2$observed
chi2$p.value

#remove rows containing Education = "No female head"
jack.jill = jack.jill[jack.jill$Education != "No female head",]



#----- CHAPTER 4 -----
# Multiple Linear Regression

data("Eggs", package = "BCA")
head(Eggs)
summary(Eggs)
variable.summary(Eggs)

?lm()
Reg1 <- lm(Cases ~ Egg.Pr, data = Eggs)
summary(Reg1)

# Scatterplot
?scatterplot()
scatterplot(Cases~Egg.Pr, regLine = TRUE, smooth = TRUE,
            spread = TRUE, boxplots = 'xy', span=0.5, data=Eggs)
scatterplot(Cases~Egg.Pr, regLine = TRUE, smooth = FALSE,
            boxplots = 'xy', data=Eggs)

?scatterplotMatrix()
scatterplotMatrix(~Beef.Pr + Cases + Cereal.Pr + Chicken.Pr + 
                    Egg.Pr + Pork.Pr + Week, 
                  smooth = TRUE, diagonal = list(method="boxplot"), data = Eggs)



ggs1 <- ggplot(data = Eggs, aes(x = Egg.Pr, y = Cases))
?geom_smooth
ggs1 + 
  geom_point(shape=1, aes(colour = Month), size = 3) + 
  geom_smooth(method=lm)
ggs1 + 
  geom_point(shape=1, aes(colour = Month), size = 3) + 
  geom_smooth(method=lm, se=FALSE)

# Line Graph
?matplot()
matplot(Eggs$Week, Eggs[, c("Cases")], type = "b", lty = 1, ylab = "Cases", pch = 1)

# Boxplot
Boxplot(Cases~Easter, data=Eggs, id.method="y")
p <- ggplot(data = Eggs, aes(x = Easter, y = Cases))
p + geom_boxplot()
?geom_boxplot()
p + geom_boxplot() + 
  geom_jitter()

# Linear Regression
?lm()
LinearEggs <- lm(Cases ~ Beef.Pr + Cereal.Pr +
                   Chicken.Pr + Easter + Egg.Pr +
                   First.Week + Month + Pork.Pr, data = Eggs)
summary(LinearEggs)

# ANOVA Test
?Anova()
Anova(LinearEggs, type = "II")

# Log Transformation
?log()
Eggs$Log.Cases <- with(Eggs, log(Cases))
Eggs$Log.Beef <- with(Eggs, log(Beef.Pr))
Eggs$Log.Cereal <- with(Eggs, log(Cereal.Pr))
Eggs$Log.Chicken <- with(Eggs, log(Chicken.Pr))
Eggs$Log.Egg <- with(Eggs, log(Egg.Pr))
Eggs$Log.Pork <- with(Eggs, log(Pork.Pr))
LinearEggs2 <- lm(Log.Cases ~ Log.Beef + Log.Cereal +
                   Log.Chicken + Easter + Log.Egg +
                   First.Week + Month + Log.Pork, data = Eggs)
summary(LinearEggs2)






#----- CHAPTER 5 -----
# Logistic Regression

?data()
data("CCS", package = "BCA")
head(CCS)
summary(CCS)
variable.summary(CCS)

# Create Samples
?create.samples()
CCS$Sample <- create.samples(CCS, est = 0.50, val = 0.50, rand.seed = 1)
CCS$Sample <- factor(CCS$Sample)
#showData(CCS)
#variable.summary(CCS)
?Recode()
CCS$MonthGive.Num <- Recode(CCS$MonthGive, '"Yes" = 1; "No" = 0', as.factor = FALSE)

scatterplot(MonthGive.Num~AveDonAmt, regLine = TRUE, smooth = TRUE,
            boxplots = 'xy', data=CCS)
scatterplotBCA(MonthGive.Num~AveDonAmt, regLine = TRUE, smooth = TRUE,
               spread = FALSE, boxplots = 'xy', span=0.5, data=CCS)

# Bin Numerical Variables
?bin.var()
CCS$AveDonAmt.Cat <- bin.var(CCS$AveDonAmt, bins = 4, method = 'proportions', labels = NULL)
variable.summary(CCS)

# Plot of Means
?plotMeans()
plotMeans(CCS$MonthGive.Num, CCS$AveDonAmt.Cat, error.bars = "se")

plotMeans(CCS$MonthGive.Num, CCS$Region, error.bars = "se")



# Logistic Regression
?glm()
LinearCCS <- glm(MonthGive ~ Age20t29 + Age70pls +
                   AveDonAmt + AveIncEA + DonPerYear +
                   EngPrmLang + FinUnivP + LastDonAmt +
                   Region + YearsGive, family = binomial(logit),
                 data = CCS, subset = Sample == "Estimation")
summary(LinearCCS)

#McFadden R2 Value
1 - (LinearCCS$deviance/LinearCCS$null.deviance) #McFadden R2

#AIC Value, the lower the better
#McFadden R2 Value, the higher the better

#?Anova()
Anova(LinearCCS, type = "II", test.statistic = "LR")

?numSummary()
numSummary(CCS[, c("DonPerYear", "YearsGive")], 
           statistics = c("mean", "sd", "IQR", "quantiles"),
           quantiles = c(0,.25,.5,.75,1))

# Log Transformation
CCS$Log.AveDonAmt <- with(CCS, log(AveDonAmt))
CCS$Log.LastDonAmt <- with(CCS, log(LastDonAmt))
CCS$Log.DonPerYear <- with(CCS, log(DonPerYear + 1))
CCS$Log.YearsGive <- with(CCS, log(YearsGive + 1))

showData(CCS)


?glm()
LogCCS <- glm(MonthGive ~ Log.AveDonAmt + Log.DonPerYear +
                   Log.LastDonAmt + Log.YearsGive + Region +
                   Age20t29 + Age70pls + EngPrmLang +
                   AveIncEA + FinUnivP, family = binomial(logit),
                 data = CCS, subset = Sample == "Estimation")
summary(LogCCS)

#McFadden R2 Value
1 - (LogCCS$deviance/LogCCS$null.deviance) #McFadden R2

#AIC Value, the lower the better
#McFadden R2 Value, the higher the better
Anova(LogCCS, type = "II", test.statistic = "LR")

#Relabel Region Factor
CCS$New.Region <- relabel.factor(CCS$Region, 
                                 new.labels = c('Other','VanFraser','VanFraser','Other','Other','Other'))

# New MixedCCS Logit Regression
MixedCCS <- glm(MonthGive ~ AveIncEA + DonPerYear +
                  LastDonAmt + Log.AveDonAmt +
                  New.Region + YearsGive
                  , family = binomial(logit),
              data = CCS, subset = Sample == "Estimation")
summary(MixedCCS)
1 - (MixedCCS$deviance/MixedCCS$null.deviance) #McFadden R2
# UPdated MixedCCS Logit Regression
MixedCCS2 <- glm(MonthGive ~ AveIncEA + DonPerYear +
                  Log.AveDonAmt + New.Region + Log.LastDonAmt, 
                 family = binomial(logit),
                data = CCS, subset = Sample == "Estimation")
summary(MixedCCS2)
1 - (MixedCCS2$deviance/MixedCCS2$null.deviance) #McFadden R2


#----- CHAPTER 6 -----
# Lift Chart

# Create Lift Chart from Previous Created Models
?lift.chart()
lift.chart(c("LinearCCS", "LogCCS", "MixedCCS", "MixedCCS2"),
           CCS[CCS$Sample == "Estimation",], "Yes", 0.01, type = "cumulative", "Estimation Sample")

# Lift Chart for Validation Subset
lift.chart(c("LinearCCS", "LogCCS", "MixedCCS", "MixedCCS2"),
           CCS[CCS$Sample == "Validation",], "Yes", 0.01, type = "cumulative", "Validation Sample")

#Incremental Lift Chart for the MixedCCS2 Model
lift.chart(c("MixedCCS2"),
           CCS[CCS$Sample == "Validation",], "Yes", 0.01, type = "incremental", "Validation Sample")


#----- CHAPTER 7 -----
# Tree Models

?rpart()
tree1CCS <- rpart(MonthGive ~ AdultAge + Age20t29 + Age20t39 +
                    Age60pls + Age70pls + Age80pls + AveDonAmt +
                    AveIncEA + DonPerYear + DwelValEA + FinUnivP +
                    EngPrmLang + hh1mem + hh1t2mem + LastDonAmt +
                    NewDonor + Region + SomeUnivP + YearsGive,
                  data = CCS, cp = 0.001, subset = Sample == "Estimation")
#Display CP Table
printcp(tree1CCS)
#Plot Cross-validation Results
plotcp(tree1CCS)
tree1CCS # Tree Leaf Summary

?printcp()

#Detailed Results Including Surrogate Splits
summary(tree1CCS)

# Tree Ploting, NOT WORKING on RStudio
#?rpart.plot()
rpart.plot(tree1CCS, type = 0, extra = 2, uniform = FALSE, fallen.leaves = TRUE)
rpart.plot(tree1CCS, type = 0, extra = 2, uniform = TRUE, fallen.leaves = FALSE)

# Plot the Tree
?plot()
?plot.rpart
# Plot the Decision Tree
plot(tree1CCS, uniform = TRUE, main = "CCS Tree")
# Label the Decision Tree Plot
text(tree1CCS, use.n = TRUE, all = TRUE, cex=.8)

plot(tree1CCS, uniform = TRUE, main = "CCS Tree")

# Remove AveDonAmt and LastDonAmt, see page 173 for detail
# noAorLDA model
noAorLDA <- rpart(MonthGive ~ AdultAge + Age20t29 + Age20t39 +
                    Age60pls + Age70pls + Age80pls +
                    AveIncEA + DonPerYear + DwelValEA + FinUnivP +
                    EngPrmLang + hh1mem + hh1t2mem +
                    NewDonor + Region + SomeUnivP + YearsGive,
                  data = CCS, cp = 0.001, subset = Sample == "Estimation")

# noAorLDA04 model
noAorLDA04 <- rpart(MonthGive ~ AdultAge + Age20t29 + Age20t39 +
                    Age60pls + Age70pls + Age80pls +
                    AveIncEA + DonPerYear + DwelValEA + FinUnivP +
                    EngPrmLang + hh1mem + hh1t2mem +
                    NewDonor + Region + SomeUnivP + YearsGive,
                  data = CCS, cp = 0.04, subset = Sample == "Estimation")

# noAorLDA013 model
noAorLDA013 <- rpart(MonthGive ~ AdultAge + Age20t29 + Age20t39 +
                    Age60pls + Age70pls + Age80pls +
                    AveIncEA + DonPerYear + DwelValEA + FinUnivP +
                    EngPrmLang + hh1mem + hh1t2mem +
                    NewDonor + Region + SomeUnivP + YearsGive,
                  data = CCS, cp = 0.013, subset = Sample == "Estimation")

# noAorLDA012 model
noAorLDA012 <- rpart(MonthGive ~ AdultAge + Age20t29 + Age20t39 +
                       Age60pls + Age70pls + Age80pls +
                       AveIncEA + DonPerYear + DwelValEA + FinUnivP +
                       EngPrmLang + hh1mem + hh1t2mem +
                       NewDonor + Region + SomeUnivP + YearsGive,
                     data = CCS, cp = 0.012, subset = Sample == "Estimation")

# Lift Chart for Estimation Samples
lift.chart(c("noAorLDA", "noAorLDA012", "noAorLDA013","noAorLDA04"),
           CCS[CCS$Sample == "Estimation",], "Yes", 0.01, "cumulative", "Estimation")

# Lift Chart for Validation Samples
lift.chart(c("noAorLDA", "noAorLDA012", "noAorLDA013","noAorLDA04"),
           CCS[CCS$Sample == "Validation",], "Yes", 0.01, "cumulative", "Validation")

# Full001 model
Full001 <- rpart(MonthGive ~ AdultAge + Age20t29 + Age20t39 +
                       Age60pls + Age70pls + Age80pls + AveDonAmt +
                       AveIncEA + DonPerYear + DwelValEA + FinUnivP +
                       EngPrmLang + hh1mem + hh1t2mem + LastDonAmt +
                       NewDonor + Region + SomeUnivP + YearsGive,
                     data = CCS, cp = 0.001, subset = Sample == "Estimation")

# Full02 model
Full02 <- rpart(MonthGive ~ AdultAge + Age20t29 + Age20t39 +
                   Age60pls + Age70pls + Age80pls + AveDonAmt +
                   AveIncEA + DonPerYear + DwelValEA + FinUnivP +
                   EngPrmLang + hh1mem + hh1t2mem + LastDonAmt +
                   NewDonor + Region + SomeUnivP + YearsGive,
                 data = CCS, cp = 0.02, subset = Sample == "Estimation")

# Full011 model
Full011 <- rpart(MonthGive ~ AdultAge + Age20t29 + Age20t39 +
                   Age60pls + Age70pls + Age80pls + AveDonAmt +
                   AveIncEA + DonPerYear + DwelValEA + FinUnivP +
                   EngPrmLang + hh1mem + hh1t2mem + LastDonAmt +
                   NewDonor + Region + SomeUnivP + YearsGive,
                 data = CCS, cp = 0.011, subset = Sample == "Estimation")

# Lift Chart for Estimation Samples
lift.chart(c("Full001", "Full02", "Full011", "noAorLDA"),
           CCS[CCS$Sample == "Estimation",], "Yes", 0.01, "cumulative", "Estimation")

# Lift Chart for Validation Samples
lift.chart(c("Full001", "Full02", "Full011", "noAorLDA"),
           CCS[CCS$Sample == "Validation",], "Yes", 0.01, "cumulative", "Validation")

rpart.plot(Full011, type = 0, extra = 2, uniform = TRUE, fallen.leaves = FALSE)
?rpart.plot()
?prp()

#----- CHAPTER 8 -----
# Neural Network Models

?nnet()
#Hidden Layer Units = 2
NN.HL2 <- nnet(MonthGive ~ AveIncEA + DonPerYear + AveDonAmt +
                 LastDonAmt + Region, data = CCS, decay = 0.10,
               size = 2, subset = Sample == "Estimation")
NN.HL2$value #Final Objective Function Value
summary(NN.HL2)

#Hidden Layer Units = 3
NN.HL3 <- nnet(MonthGive ~ AveIncEA + DonPerYear + AveDonAmt +
                 LastDonAmt + Region, data = CCS, decay = 0.10,
               size = 3, subset = Sample == "Estimation")
NN.HL3$value #Final Objective Function Value
summary(NN.HL3)

#Hidden Layer Units = 4
NN.HL4 <- nnet(MonthGive ~ AveIncEA + DonPerYear + AveDonAmt +
                 LastDonAmt + Region, data = CCS, decay = 0.10,
               size = 4, subset = Sample == "Estimation")
NN.HL4$value #Final Objective Function Value
summary(NN.HL4)

#Hidden Layer Units = 6
NN.HL6 <- nnet(MonthGive ~ AveIncEA + DonPerYear + AveDonAmt +
                 LastDonAmt + Region, data = CCS, decay = 0.10,
               size = 6, subset = Sample == "Estimation")
NN.HL6$value #Final Objective Function Value
summary(NN.HL6)

#Lift Chart
lift.chart(c("MixedCCS2", "NN.HL2", "NN.HL4", "NN.HL6", "Full011"),
           CCS[CCS$Sample == "Estimation",], "Yes", 0.01, type = "cumulative", "CCS Estimation Sample")

lift.chart(c("MixedCCS2", "NN.HL2", "NN.HL4", "NN.HL6", "Full011"),
           CCS[CCS$Sample == "Validation",], "Yes", 0.01, type = "cumulative", "CCS Validation Sample")



#----- CHAPTER 9 -----
# UBC Wesbrook Example
?read.dbf
Wesbrook <- read.dbf("C:\\Users\\SungI7\\OneDrive\\SFU\\BUSINESS\\400\\BUS445\\Tutorial_Assignments\\wesbrook.dbf", as.is = FALSE)

# Create Sample Subset
Wesbrook$Sample <- create.samples(Wesbrook, est = 0.70, val = 0.30, rand.seed = 1)

# Create new variable that the years since the individual
# received his or her first UBC degree: 1999 - FRSTYEAR
Wesbrook$YRFDGR <- with(Wesbrook, 1999-FRSTYEAR)

# Create new variable that the years since the individual
# received his or her most recent degree from UBC: 1999 - GRADYR1
Wesbrook$YRLDGR <- with(Wesbrook, 1999-GRADYR1)

# Remove Unnecessary Variables: YRFDGR& YRLDGR
Wesbrook$FRSTYEAR <- NULL
Wesbrook$GRADYR1 <- NULL
Wesbrook$BIGBLOCK <- NULL #Remove because unary, useless for predictive purposes

# Check NULL Values
variable.summary(Wesbrook)

# Replace Numerical Variable NA to 0
?Recode()
Wesbrook$YRFDGR <- Recode(Wesbrook$YRFDGR, "NA = 0", as.factor = FALSE)
Wesbrook$YRLDGR <- Recode(Wesbrook$YRLDGR, "NA = 0", as.factor = FALSE)

# Replace Categorical Variable NA to "ND"; ND means "No Degree"
Wesbrook$DEPT1 <- Recode(Wesbrook$DEPT1, "NA = 'ND'", as.factor = TRUE)
Wesbrook$FACULTY1 <- Recode(Wesbrook$FACULTY1, "NA = 'ND'", as.factor = TRUE)

# Remove MARITAL Variable due to useless
Wesbrook$MARITAL <- NULL
# Remove TOTLGIVE due to not useful to predict non Wesbrook donors
Wesbrook$TOTLGIVE <- NULL
# Remove ID as it is meaningless for prediction
Wesbrook$ID <- NULL

# Remove Other Useless Variables
Wesbrook$EA <- NULL
Wesbrook$INDUPDT <- NULL
Wesbrook$MAJOR1 <- NULL


# Create Tree Model
WesTree <- rpart(WESBROOK ~ ATHLTCS + AVE_INC + CHILD +
                         CNDN_PCT + DEPT1 + DWEL_VAL + ENG_PCT +
                         FACSTAFF + FACULTY1 + HH_1PER + HH_2PER +
                         HH_3PER + HH_45PER + MOV_DWEL + OTHERACT +
                         OWN_PCT + PARENT + PROV + SD_INC + SEX +
                         SPOUSE + YRFDGR + YRLDGR,
                       data = Wesbrook, cp = 0.001, subset = Sample == "Estimation")
#Display CP Table
printcp(WesTree)
#Plot Cross-validation Results
plotcp(WesTree)

WesTree <- rpart(WESBROOK ~ ATHLTCS + AVE_INC + CHILD +
                   CNDN_PCT + DEPT1 + DWEL_VAL + ENG_PCT +
                   FACSTAFF + FACULTY1 + HH_1PER + HH_2PER +
                   HH_3PER + HH_45PER + MOV_DWEL + OTHERACT +
                   OWN_PCT + PARENT + PROV + SD_INC + SEX +
                   SPOUSE + YRFDGR + YRLDGR,
                 data = Wesbrook, cp = 0.0072, subset = Sample == "Estimation")

rpart.plot(WesTree, type = 0, extra = 2, uniform = TRUE, fallen.leaves = FALSE)

# Remove Missing Variables
?na.omit()
Wesbrook2 <- na.omit(Wesbrook)

variable.summary(Wesbrook2)

WesTree2 <- rpart(WESBROOK ~ ATHLTCS + AVE_INC + CHILD +
                   CNDN_PCT + DEPT1 + DWEL_VAL + ENG_PCT +
                   FACSTAFF + FACULTY1 + HH_1PER + HH_2PER +
                   HH_3PER + HH_45PER + MOV_DWEL + OTHERACT +
                   OWN_PCT + PARENT + PROV + SD_INC + SEX +
                   SPOUSE + YRFDGR + YRLDGR,
                 data = Wesbrook2, cp = 0.0072, subset = Sample == "Estimation")
printcp(WesTree2)
plotcp(WesTree2)
rpart.plot(WesTree2, type = 0, extra = 2, uniform = TRUE, fallen.leaves = FALSE)

# Correlation Matrix
?cor
cor(Wesbrook2[,c("AVE_INC","CNDN_PCT","DWEL_VAL","ENG_PCT","HH_1PER",
                 "HH_2PER","HH_3PER","HH_45PER","MOV_DWEL","OWN_PCT",
                 "SD_INC","YRFDGR","YRLDGR")], use = "complete.obs")


# Wesbrook2 Logit Regression
WesLogis <- glm(WESBROOK ~ ATHLTCS + AVE_INC + CHILD +
                  CNDN_PCT + DEPT1 + DWEL_VAL + ENG_PCT +
                  FACSTAFF + FACULTY1 + HH_1PER + HH_2PER +
                  HH_3PER + HH_45PER + MOV_DWEL + OTHERACT +
                  OWN_PCT + PARENT + PROV + SD_INC + SEX +
                  SPOUSE + YRFDGR + YRLDGR, 
                 family = binomial(logit),
                 data = Wesbrook2, subset = Sample == "Estimation")
summary(WesLogis)
1 - (WesLogis$deviance/WesLogis$null.deviance) #McFadden R2
Anova(WesLogis, type = "II", test.statistic = "LR")

# Stepwise 
?stepwise()
?step()
WesStep <- step(WesLogis, direction = "both", k = 2)
summary(WesStep)
1 - (WesStep$deviance/WesStep$null.deviance) #McFadden R2
Anova(WesStep, type = "II", test.statistic = "LR")

# Neural Network Model
?nnet()
WesNnet <- nnet(WESBROOK ~ ATHLTCS + AVE_INC + CHILD +
                  DWEL_VAL + FACSTAFF + FACULTY1 + HH_1PER +
                  HH_2PER + HH_3PER + SD_INC + SEX + SPOUSE + YRFDGR,
                data = Wesbrook2, decay = 0.10, size = 3,
                subset = Sample == "Estimation")
WesNnet$value #Final Objective Function Value
summary(WesNnet)

WesNnet2 <- nnet(WESBROOK ~ ATHLTCS + AVE_INC + CHILD +
                   CNDN_PCT + DEPT1 + DWEL_VAL + ENG_PCT +
                   FACSTAFF + FACULTY1 + HH_1PER + HH_2PER +
                   HH_3PER + HH_45PER + MOV_DWEL + OTHERACT +
                   OWN_PCT + PARENT + PROV + SD_INC + SEX +
                   SPOUSE + YRFDGR + YRLDGR,
                data = Wesbrook2, decay = 0.10, size = 3,
                subset = Sample == "Estimation")
WesNnet2$value #Final Objective Function Value
summary(WesNnet2)


# Lift Chart for Estimation Samples
?lift.chart()
lift.chart(c("WesTree2", "WesStep", "WesNnet", "WesNnet2"),
           Wesbrook2[Wesbrook2$Sample == "Estimation",], "Y", 0.01, "cumulative", "Estimation")

# Lift Chart for Validation Samples
lift.chart(c("WesTree2", "WesStep", "WesNnet", "WesNnet2"),
           Wesbrook2[Wesbrook2$Sample == "Validation",], "Y", 0.01, "cumulative", "Validation")

# Rank Score
?rankScore
BCA::rankScore("WesNnet", Wesbrook2, "Y")
Wesbrook2$Score <- rankScore("WesNnet", Wesbrook2, "Y")
Wesbrook2$SCORE <- rankScore("WesNnet", Wesbrook2, "Y")
?rawProbScore
Wesbrook2$Score2 <- rawProbScore("WesNnet", Wesbrook2, "Y")
# If the True Respond Rate is not 100%
?adjProbScore


#----- CHAPTER 10 -----
# Ward's Method of Cluster Analysis
# and Principal Components

# Load Data
data("Athletic", package = "BCA")

# Create Hierarchical Cluster Analysis
?hclust()
?model.matrix()
WardsAthletic <- hclust(dist(model.matrix(~-1 + Attnd +
                                            Fem + Finan + Grad +
                                            Teams + Violat + Win,
                                          Athletic))^2, method = "ward.D")
plot(WardsAthletic, main = "Cluster Dendrogram for Solution WardAthletic",
     xlab = "Observation Number in Data Set Athletic",
     sub = "Method = ward; Distance = squared-euclidian")

# Create Cluster Plot Centroids Chart
summary(as.factor(cutree(WardsAthletic, k = 4))) # Cluster Sizes

# Cluster Centroids Table
by(model.matrix(~-1 + Attnd + Fem + Finan + Grad + Teams +
                  Violat + Win, Athletic),
   as.factor(cutree(WardsAthletic, k = 4)), colMeans) # Cluster Centroids

# Create CP Plot
bpCent(prcomp(model.matrix(~-1 + Attnd + Fem + Finan + Grad + Teams +
                             Violat + Win, Athletic)),
       cutree(WardsAthletic, k = 4),
       data.pts = TRUE, centroids = TRUE,
       xlabs = as.character(cutree(WardsAthletic, k = 4)))


# Add Hierarchical Clustering to Dataset
Athletic$Wards4 <- assignCluster(model.matrix(~-1 + Attnd + Fem + Finan + Grad +
                                                Teams + Violat + Win, Athletic),
                                 Athletic, cutree(WardsAthletic, k = 4))

# The Mean plot shows that cluster 2 cares most about "Grad",
# and the error bars indicate that this high level of caring
# is statistically different from the lwoer level of caring
# in other three clusters.
plotMeans(Athletic$Grad, Athletic$Wards4, error.bars = "conf.int", level = 0.95)



# Create 3D bi-plot of clusters
bpCent3d(prcomp(model.matrix(~-1 + Attnd + Fem + Finan + Grad + Teams +
                             Violat + Win, Athletic)),
       cutree(WardsAthletic, k = 4),
       data.pts = TRUE, centroids = TRUE,
       xlabs = as.character(cutree(WardsAthletic, k = 4)))

numSummary(Athletic[,c("Attnd", "Fem", "Finan", "Grad", "Teams",
                       "Violat","Win")], groups = Athletic$Wards4,
           statistics = c("mean", "sd", "IQR", "quantiles"),
           quantiles = c(0, .25, .5, .75, 1))
numSummary(Athletic[,c("Attnd", "Fem", "Finan", "Grad", "Teams",
                       "Violat","Win")], groups = Athletic$Wards4,
           statistics = c("mean"),
           quantiles = c(0, .25, .5, .75, 1))

### Run PC Analysis
### Example from Ryan
#?prcomp()
ooMBA.pc <- prcomp(scaled, center = FALSE, scale. = FALSE)
ooMBA.pcFit <- prcomp(scaledFit, center = FALSE, scale. = FALSE)
# Same Result!
print(ooMBA.pc)
print(ooMBA.pcFit)

# Summary
summary(ooMBA.pc)
summary(ooMBA.pcFit)
?prcomp
abc <- prcomp(Athletic, center = FALSE, scale. = TRUE)

# Graph the biPlot
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)

g <- ggbiplot(abc, obs.scale = 1, var.scale = 1, 
              groups = scaled.programs, ellipse = TRUE, 
              circle = FALSE, alpha = 0.5)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top',
               text = element_text(size=12))
print(g)

?ggbiplot()




#----- CHAPTER 11 -----
# K-Centroids Partitioning Cluster Analysis

#
?bootCVD()
bootCVD(model.matrix(~-1 + Attnd + Fem + Finan + Grad +
                       Teams + Violat + Win, Athletic),
        k = 2:7, nboot = 100, nrep = 3, method = "kmn",
        col1 = 1, col2 = 2, dsname = "Athletic")

.cluster <- stepFlexclust(model.matrix(~-1 + Attnd + Fem + Finan +
                                         Grad + Teams +
                                         Violat + Win, Athletic),
                          k = 4, nrep = 10, FUN = kcca,
                          family = kccaFamily("kmeans"))
summary(.cluster)
print(.cluster@centers) # Cluster Centroids

# Create CP Plot
?bpCent
#bpCent(prcomp(model.matrix(~-1 + Attnd + Fem + Finan + Grad + Teams +
#                             Violat + Win, Athletic)),
#       cutree(WardsAthletic, k = 4),
#       data.pts = TRUE, centroids = TRUE,
#       xlabs = as.character(cutree(WardsAthletic, k = 4)))
?clusters
bpCent(prcomp(model.matrix(~-1 + Attnd + Fem + Finan +
                             Grad + Teams +
                             Violat + Win, Athletic)),
       clusters(.cluster), data.pts = TRUE, centroids = FALSE,
       xlabs = as.character(clusters(.cluster)))
Athletic$KMeans4 <- as.factor(clusters(.cluster))

remove(.cluster)

#
.Table <- xtabs(~KMeans4 + Wards4, data = Athletic)
.Table
remove(.Table)




