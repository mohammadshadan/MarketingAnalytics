
# install.packages(c("lavaan", "semPlot", "corrplot","multcomp"))
library(lavaan)
library(semPlot)
library(corrplot)
library(multcomp)

satData <- read.csv("http://goo.gl/UDv12g")
# write.csv(output, file = "satData.csv", row.names=FALSE)
satData$Segment <- factor(satData$Segment)

# It has 500 (simulated) consumers' answers to a survey with four items
# asking about satisfaction with a product (iProdSAT), sales (iSalesSAT) experience,
# and likelihood to recommend the product and salesperson (iProdREC
#                                                          and iSalesREC, respectively). 
# Each respondent is also assigned to a numerically
# coded segment (Segment). In the second line of R code above, we set Segment
# to be a categorical factor variable.

#Summary Statistics
dim(satData)
str(satData)
head(satData)
summary(satData)

# plot the correlation matrix, omitting the categorical Segment variable in column 3

corrplot.mixed(cor(satData[, -3]))

# compute the mean satisfaction for each segment using the aggregate() function:
aggregate(iProdSAT ~ Segment, satData, mean)

sat.anova <- aov(iProdSAT ~ -1 + Segment, satData)
summary(sat.anova)


# We plot the ANOVA model to visualize confidence intervals for mean product satisfaction
# by segment:

par(mar=c(4,8,4,2))
plot(glht(sat.anova))

satModel <- "SAT =~ iProdSAT + iSalesSAT
 REC =~ iProdREC + iSalesREC
REC ~ SAT "

#Comparative Fit Index
sat.fit <- cfa(satModel, data=satData)
summary(sat.fit, fit.m=TRUE)

semPaths(sat.fit, what="est",
         residuals=FALSE, intercepts=FALSE, nCharNodes=9)

?"?"
??anova




rm(list=ls()) # caution, deletes all objects! See explanation below
store.num <- factor(c(3, 14, 21, 32, 54)) # store id
store.rev <- c(543, 654, 345, 678, 234) # store revenue, $1000
store.visits <- c(45, 78, 32, 56, 34) # visits, 1000s
store.manager <- c("Annie", "Bert", "Carla", "Dave", "Ella")
store.df <- data.frame(store.num, store.rev, store.visits, store.manager, stringsAsFactors=F) # F = FALSE
head(store.df)
str(store.df)

#2.6 Loading and Saving Data
save(store.df, file="store-df-backup.RData")
rm(store.df) # caution, only if save() gave no error
mean(store.df$store.rev) # error
load("store-df-backup.RData")
mean(store.df$store.rev)

# save() can also take a group of objects as an argument; just replace the single
# object name with list=c() and fill in c() with a character vector. For instance:

save(list=c("store.df","store.visits"), file="store-df-backup.RData")

getwd()
# setwd("???/Documents/R")


# 2.6.2 CSV Files

# A handy way to test CSV files is to use the command without a file name, which
# sends the output to the console just as it would be written to a file:

write.csv(store.df, row.names=FALSE)

write.csv(store.df, file="store-df.csv", row.names=FALSE)
read.csv("store-df.csv")
store.df2 <- read.csv("store-df.csv", stringsAsFactors=FALSE)


store.df2 <- read.csv("store-df.csv", stringsAsFactors=FALSE)
store.df2$store.num <- factor(store.df2$store.num)

all.equal(store.df, store.df2)
store.df == store.df2


#2.7 Writing Your Own Functions*
se <- function(x) { sd(x) / sqrt(length(x)) }
se(store.df$store.visits)
mean(store.df$store.visits) + 1.96 * se(store.df$store.visits)
se(store.df$store.manager)


se <- function(x) {
  # computes standard error of the mean
  tmp.sd <- sd(x) # standard deviation
  tmp.N <- length(x) # sample size
  tmp.se <- tmp.sd / sqrt(tmp.N) # std error of the mean
  return(tmp.se)
}

se

# 2.7.1 Language Structures*

# if (TEST) EXPR [else EXPR.b] # do EXPR if TEST is true, else EXPR.b
# 
# while (TEST) EXPR # repeat EXPR while TEST is true


# ifelse(TEST, YES, NO). ifelse() applies TEST to every element in a
# vector and returns the value of the expression YES for elements that pass the test as
# TRUE and the value of the expression NO for those that do not pass.

x <- -2:2
log(x)
ifelse(x > 0, x, NA)
log(ifelse(x > 0, x, NA))

# 2.7.2 Anonymous Functions*
my.data <- matrix(runif(100), ncol=5) # 100 random numbers in 5 columns
apply(my.data, 2, median) / 2

# A second solution is a function with a name such as halfmedian, with
apply():
halfmedian <- function (x) { median(x) / 2 }
apply(my.data, 2, halfmedian)

# 2.8 Clean Up!
# ls()
# rm(store.num)
# rm(list=c("store.rev", "store.visits"))
# rm(list=ls(pattern="store"))
# rm(list=ls())  # deletes all visible objects in memory

?gcookbook
library(ggplot2)
install.packages("gcookbook")
library(gcookbook)
head(cabbage_exp)
ggplot(cabbage_exp, aes(x=Cultivar, y=Weight)) + geom_bar(stat="identity") +
  facet_grid(. ~ Date) +
  theme(strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="black",
                                        size=1))

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
