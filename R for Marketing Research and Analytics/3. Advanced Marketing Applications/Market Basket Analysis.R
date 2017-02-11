library(arules)
library(arulesViz)

data("Groceries")

data(package="arules")

head(Groceries)

# ?write.transactions

# write.csv(inspect(Groceries), "Groceries.csv")
# write(Groceries, file = "Groceries2.csv", sep=",")

summary(Groceries)
class(Groceries)

9835*169
9835*169*0.02609146
2513+1903+1809+1715+1372+34055

inspect(Groceries[1:3])
inspect(head(Groceries, 3))
inspect(tail(Groceries, 3))

#Support : how frequently an item occurs in data
itemFrequency(Groceries[,1])
itemFrequency(Groceries[,1:6])


itemFrequencyPlot(Groceries, support=0.10)
itemFrequencyPlot(Groceries, topN=20)

#Confidence

groc.rules <- apriori(Groceries, parameter=list(supp=0.01, conf=0.3, target="rules"))

inspect(subset(groc.rules, lift > 3))