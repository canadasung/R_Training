
seq()
rep()
mean()
?median()
?mode()
?sqrt()

stat_mode <- function(x, return_multiple = TRUE, na.rm = FALSE) {
  if(na.rm){
    x <- na.omit(x)
  }
  ux <- unique(x)
  freq <- tabulate(match(x, ux))
  mode_loc <- if(return_multiple) which(freq==max(freq)) else which.max(freq)
  return(ux[mode_loc])
}

stat_mode(c(1,1,1,2,2,3,3,3,3,4,4,5,5,6,6,7,7,7))



?matrix()
matrix(data = NA, nrow = 3, ncol=3)

my.data <- 1:100
my.data

a <- matrix(my.data, 10, 10, byrow = TRUE)
a
a[1,10]


#rbind()
r1 <- c("I", "am", "happy")
r2 <- c("What","a","day")
r3 <- c(1,2,3)

C<- rbind(r1,r2,r3)
C

#cbind()
D <- cbind (r1,r2,r3)
D

V <- c(1,2,3)
colnames(C) <- V
rownames(C) <- V
C[2,2]

#Named Vectors
Charlie <-1:5
Charlie
names(Charlie) <- c("a", "b", "c", "d", "e")
names(Charlie) <- NULL


# Naming Matrix Dimensions
temp.vec <- rep(c("a","b","zZ"), each=3)
temp.vec

Bravo <- matrix(temp.vec, 3,3)
Bravo

rownames(Bravo) <- c("How", "are", "you")
colnames(Bravo) <- c("x","y","z")

Bravo["How","z"]

Games
rownames(Games)
colnames(Games)
Games["LeBronJames", "2012"]

FieldGoals
round(FieldGoals/Games,1)
round(MinutesPlayed/Games)


?matplot
matplot(FieldGoals)
matplot(t(FieldGoals), type="b", pch=15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players, col=c(1:4,6), pch=15:18, horiz=F)



#Subsetting
#Using "drop" parameter to retrieve matrix instead of vector
x <- c("a", "b", "c", "d", "e")
x

x[c(1,5)]
x[1:5]

Games
Games[1:3,6:10]
Games[c(1,10)]
Games[,c("2008","2009")]
Games[1,]
Games[1,5]

is.matrix(Games[1,])
is.vector(Games[1,5])

Games[1,,drop=F]
Games[1,5,drop=F]



Data <- MinutesPlayed[1,,drop=F]
matplot(t(Data), type="b", pch=15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players[1:3], col=c(1:4,6), pch=15:18, horiz=F)
Data


#Create Function
myplot <- function(datatypes,rows=1:10){
  Data <- datatypes[rows,,drop=F]
  matplot(t(Data), type="b", pch=15:18, col=c(1:4,6))
  legend("bottomleft", inset=0.01, legend=Players[rows], col=c(1:4,6), pch=15:18, horiz=F)
}

myplot(MinutesPlayed/Games,3)



#Salary
myplot(Salary)
myplot(Salary/Games)
myplot(Salary/FieldGoals)

#In-Game Metrics
myplot(MinutesPlayed)
myplot(Points)

#In-Game Metric Normalized
myplot(FieldGoals/Games)
myplot(FieldGoals/FieldGoalAttempts)
myplot(FieldGoalAttempts/Games)
myplot(Points/Games)

#Interesting Observation
myplot(MinutesPlayed/Games)
myplot(Games)

#Time is valuable
myplot(FieldGoals/MinutesPlayed)

#Player Style
myplot(Points/FieldGoals)



#Demographic Analysis
?read.csv()

#Method 1: Select The File Manually
stats <- read.csv(file.choose())
stats


#Method 2: Set WD and Read Data
getwd()

#Windows:
setwd("C:\\Users\\SungI7\\OneDrive\\R_Training\\DataFrames")
#Mac:
setwd("/Users/SungI7/OneDrive/R_Training/DataFrames")
rm(stats)

stats <- read.csv("DemographicData.csv")
stats


#-------------------- Exploring Data
nrow(stats)
ncol(stats)
head(stats)
head(stats, n = 10)
tail(stats, n = 3)
str(stats)
?str()
?runif()
summary(stats)
?summary()


#-------------------- Using the $ Sign
head(stats)
stats[3,3]
stats[3,"Birth.rate"]
stats[3,3]
stats$Internet.users
stats$Internet.users[2]
levels(stats$Income.Group)


#-------------------- Basic Operations with a DF
stats[1:10,] #subsetting
stats[3:9,]
stats[c(4,100),]

#Remember how the [] work
is.data.frame(stats[1,]) #no need for drop=F
is.data.frame(stats[,1])
is.data.frame(stats[,1,drop=F])
stats[,1,drop=F]

#multiply columns
head(stats)
stats$Birth.rate * stats$Internet.users
stats$Birth.rate + stats$Internet.users

#add column
head(stats)
stats$MyCalc <- stats$Birth.rate * stats$Internet.users

#test of knowledge
stats$xyz <- 1:5      #what would happen?

#remove column
head(stats)
stats$xyz <- NULL
stats$MyCalc <- NULL


#-------------------- Filtering Data Frames
head(stats)
stats$Internet.users < 2 #returns true/false values
filter <- stats$Internet.users < 2 #saved into an object
stats[filter,] #only true value would be displayed

stats[stats$Birth.rate > 40,]
stats[stats$Birth.rate > 40 & stats$Internet.users <2,]
stats[stats$Income.Group == "High income",]

stats[stats$Country.Name == "Malta",]


#-------------------- Introduction to ggplot & qplot()
?qplot()
qplot(data = stats, x = Internet.users)
qplot(data = stats, x = Income.Group, y = Birth.rate)
qplot(data = stats, x = Income.Group, y = Birth.rate, 
      size=I(3), colour = I("blue"))
qplot(data = stats, x = Income.Group, y = Birth.rate, 
      geom = "boxplot")

qplot(data = stats, x = Internet.users, y = Birth.rate,
      size = I(2), colour = Income.Group)


#-------------------- Creating Data Frames
mydf <- data.frame(Countries_2012_Dataset, Codes_2012_Dataset, 
                  Regions_2012_Dataset)
head(mydf)
colnames(mydf) <- c("Country", "Code", "Region")

rm(mydf)


mydf <- data.frame(Country = Countries_2012_Dataset, 
                   Code = Codes_2012_Dataset, 
                   Region = Regions_2012_Dataset)
?data.frame()

summary(mydf)
str(mydf)
levels(mydf$Region)

#-------------------- Merging Data Frames
?merge()
?merge.data.frame()
merged <- merge(stats, mydf, by.x = "Country.Code", by.y = "Code")

head(merged)

merged$Country <- NULL
tail(merged)


#-------------------- Visualizing With new Split
qplot(data = merged, x = Internet.users, y = Birth.rate,
      colour = Region)

#1. Shapes
qplot(data = merged, x = Internet.users, y = Birth.rate,
      colour = Region, size = I(5), shape = I(2))

#2. Transparency
qplot(data = merged, x = Internet.users, y = Birth.rate,
      colour = Region, size = I(5), shape = I(19), 
      alpha = I(0.5))

#3. Add Title
qplot(data = merged, x = Internet.users, y = Birth.rate,
      colour = Region, size = I(5), shape = I(19), 
      alpha = I(0.5), main = "Birth Rate vs Internet Users")



####################
# Grammar of Graphics
# - Data: data behind graphs
# - Aesthetics: How your data maps to what you want to see
# - Geometries: dot, line, circle, square.....etc.
# - Satistics: transformation to new variables may be required
# - Facets: Multiple Charts under one Graph
# - Coordinates: X Y Coordinates, zoom-in/zoom-out
# - Theme: Colours/Labels/Legends
####################


#-------------------- Factor
getwd()
setwd("C:\\Users\\SungI7\\OneDrive\\R_Training\\Section6_ggplot2")

movies <- read.csv("Movie-Ratings.csv")


str(movies)
head(movies)
tail(movies)
colnames(movies) <- c("Film", "Genre", "CriticRating", "AudienceRating", "BudgetMillions", "Year")
summary(movies)

movies$Year <- factor(movies$Year)


#-------------------- Aesthetics
library(ggplot2)

ggplot(data = movies, aes(x = CriticRating, y = AudienceRating))


#add geometry
ggplot(data = movies, aes(x = CriticRating, y = AudienceRating)) + geom_point()


#add colour
ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                          colour = Genre)) + geom_point()


#add size
ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                          colour = Genre, size = Genre)) + geom_point()


#add size - better way
ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                          colour = Genre, size = BudgetMillions)) + geom_point()
#>>> This is 1st graph, we will improve it later


#-------------------- Plotting With Layers
?ggplot()

p <- ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                               colour = Genre, size = BudgetMillions))
#point
p + geom_point()
#lines
p + geom_line()
#multiple layers
p + geom_line() + geom_point()


#-------------------- Overriding Aesthetics
q <- ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                               colour = Genre, size = BudgetMillions))

#add geom layer
q + geom_point()

#overriding aes
q + geom_point(aes(size = CriticRating))

#overriding colour
q + geom_point(aes(colour = BudgetMillions))

#overriding x,y coordinates
q + geom_point(aes(x = BudgetMillions)) + xlab("Budget Millions $$$")
#>>> 2nd chart

#multiple layers reducing line size
p + geom_line() + geom_point()
p + geom_line(size = 1) + geom_point()


#-------------------- Mapping versus Setting
r <- ggplot(data = movies, aes(x = CriticRating, y = AudienceRating))
r + geom_point()

#add colour
#1 by mapping
r + geom_point(aes(colour = Genre))
#2 by setting
r + geom_point(colour = "DarkGreen")
#ERROR: 
r + geom_point(aes(colour = "DarkGreen"))


#1 Mapping
r + geom_point(aes(size = BudgetMillions))
#2 Setting
r + geom_point(size = 10)
#ERROR:
r + geom_point(aes(size = 10))


#-------------------- Histograms and Density Charts
s <- ggplot(data = movies, aes(x = BudgetMillions))
s + geom_histogram(binwidth = 10)

#add colour
s + geom_histogram(binwidth = 10, aes(fill = Genre))
#add a border
s + geom_histogram(binwidth = 10, aes(fill = Genre), colour = "Black")
#>>>3rd chart, we will improve it

#Density Charts
s + geom_density(aes(fill = Genre))
s + geom_density(aes(fill = Genre), position = "Stack")

#-------------------- Starting Layer Tips
t <- ggplot(data = movies, aes(x = AudienceRating))
t + geom_histogram(binwidth = 10, 
                   fill = "white", colour = "Blue")

#another way: (with more flexibility for aes adjustments)
t <- ggplot(data = movies)
t + geom_histogram(binwidth = 10, 
                   aes(x = AudienceRating), 
                   fill = "white", colour = "Blue")
#>>>4th Chart

t + geom_histogram(binwidth = 10, 
                   aes(x = CriticRating), 
                   fill = "white", colour = "Blue")
#>>>5th Chart

t <- ggplot() #use when dealing with multiple datasets


#-------------------- Statistical Transformations
?geom_smooth

u <- ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                               colour = Genre))
u + geom_point() + geom_smooth(fill = NA)

#boxplots
u <- ggplot(data = movies, aes(x = Genre, y = AudienceRating, 
                               colour = Genre))
u + geom_boxplot()
u + geom_boxplot(size = 1.2) + geom_point()
#tip / hack:
u + geom_boxplot(size = 1.2) + geom_jitter()
#another way:
u + geom_jitter() + geom_boxplot(size = 1.2, alpha = 0.5)
#>>>6th Chart
#CriticRating
u <- ggplot(data = movies, aes(x = Genre, y = CriticRating, 
                               colour = Genre))
u + geom_boxplot(size = 1.2)
u + geom_jitter() + geom_boxplot(size = 1.2)
u + geom_boxplot(size = 1.2) + geom_jitter()


#-------------------- Using Facets
v <- ggplot(data = movies, aes(x = BudgetMillions))
v + geom_histogram(binwidth = 10, aes(fill = Genre), colour = "black")

#facets
v + geom_histogram(binwidth = 10, aes(fill = Genre), 
                   colour = "black") + 
  facet_grid(Genre~.)
v + geom_histogram(binwidth = 10, aes(fill = Genre), 
                   colour = "black") + 
  facet_grid(Genre~., scales = "free")

v + geom_histogram(binwidth = 10, aes(fill = Genre), 
                   colour = "black") + 
  facet_grid(.~Genre)


#scatterplots:
w <- ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
            colour = Genre))
w + geom_point(size = 3)
#facets
w + geom_point(size = 3) + facet_grid(.~Genre)
w + geom_point(size = 3) + facet_grid(Genre~.)

w + geom_point(size = 3) + facet_grid(.~Year)

w + geom_point(size = 3) + facet_grid(Genre~Year)

w + geom_point(size = 3) + facet_grid(Genre~Year) +
  geom_smooth(fill = NA)

w + geom_point(aes(size = BudgetMillions)) + 
  facet_grid(Genre~Year) +
  geom_smooth(fill = NA)

w + geom_point(aes(size = BudgetMillions)) + 
  facet_grid(Genre~Year) +
  geom_smooth(fill = NA)
#>>> 1 but still will improve


#-------------------- Coordinates
#limits
#zoom

m <- ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                               size = BudgetMillions, 
                               colour = Genre))
m + geom_point()

m + geom_point() + 
  xlim(50,100) + 
  ylim(50,100)

#won't work well always
n <- ggplot(data = movies, aes(x = BudgetMillions))
n + geom_histogram(binwidth = 10,
                   aes(fill = Genre),
                   colour = "Black")

n + geom_histogram(binwidth = 10,
                   aes(fill = Genre),
                   colour = "Black") + ylim(0,50)

#instead - zoom:
n + geom_histogram(binwidth = 10,
                   aes(fill = Genre),
                   colour = "Black") + coord_cartesian(ylim = c(0,50))

#improve #1
w + geom_point(aes(size = BudgetMillions)) + 
  facet_grid(Genre~Year) +
  geom_smooth() +
  coord_cartesian(ylim=c(0,100))


#-------------------- Theme
o <- ggplot(data = movies, aes(x = BudgetMillions))
h <- o + geom_histogram(binwidth = 10, aes(fill = Genre),
                   colour = "Black")
h
#axis labels
h + xlab("Money Axis") + ylab("Number of Movies")

#label formatting
h + xlab("Money Axis") + ylab("Number of Movies") +
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 30),
        axis.title.y = element_text(colour = "Red", size = 30))

#tick mark formatting
h + xlab("Money Axis") + ylab("Number of Movies") +
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 30),
        axis.title.y = element_text(colour = "Red", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))
?theme()

#legend formatting
h + xlab("Money Axis") + ylab("Number of Movies") +
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 30),
        axis.title.y = element_text(colour = "Red", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = c(1,1),
        legend.justification = c(1,1))


#title
h + 
  xlab("Money Axis") + 
  ylab("Number of Movies") +
  ggtitle("Movie Budget Distribution") + 
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 30),
        axis.title.y = element_text(colour = "Red", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        
        plot.title = element_text(colour = "DarkBlue",
                                  size = 40,
                                  family = "Courier"))


#-------------------- Homework 6
getwd()
setwd("C:\\Users\\SungI7\\OneDrive\\R_Training\\Section6_ggplot2")

m2 <- read.csv("Section6-Homework-Data.csv")
heads(m2)
str(m2)
summary(m2)

colnames(m2) <- c("DayofWeek", "Director", "Genre", "MovieTitle", 
                  "RleaseDate", "Studio", "AdjustGrossMills",
                  "BudgetMills", "GrossMills","IMDBRating",
                  "MovieLensRating", "OverseaMills", 
                  "OverseaPercent", "ProfitMills", 
                  "ProfitPercent", "RuntimeMinute",
                  "USRevMills", "USGrossPercent")

a <- ggplot(data = m2)

a + geom_point(aes(x = MovieLensRating, y = IMDBRating, colour = Genre, size = USRevMills))

levels(m2$Genre)

#activate ggplot2
library(ggplot2)

#cool insight:
ggplot(data = m2, aes(x=DayofWeek)) + geom_bar()


ggplot(data = m2, aes(x=Studio)) + geom_bar()


#filter for the data frame
filt <- (m2$Genre == "action") | (m2$Genre == "adventure") | (m2$Genre == "animation") | (m2$Genre == "comedy") | (m2$Genre == "drama")

filt2 <- m2$Studio %in% c("Buena Vista Studios", "WB", "Fox", "Universal", "Sony", "Paramount Pictures")



mov2 <- m2[filt & filt2,]
mov2


#prepare the plot's data and aes layers:
p <- ggplot(data = mov2, aes(x = Genre, y = USGrossPercent))
p + geom_point()

p + geom_boxplot() + 
  geom_jitter()

#add geometries
q <- p +
    geom_jitter(aes(size = BudgetMills, colour = Studio)) + 
    geom_boxplot(alpha = 0.7, outlier.colour = NA)

#non-data ink
q <- q +
  xlab("Genre") + 
  ylab("USGrossPercent") +
  ggtitle("Domestic Gross % by Genre")
q

#Theme
q <- q +
  theme(
    axis.title.x = element_text(colour = "Blue", size = 30),
    axis.title.y = element_text(colour = "Blue", size = 30),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    
    plot.title = element_text(size = 40),
    
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    
    text = element_text(family = "Comic Sans MS")
  )
q

#Final touch
q$labels$size <- "Budget $M"
q

warnings()
y

install.packages("extrafont")

library(extrafont)
font_import()

loadfonts()
