library(arules)
groceries <- read.transactions("groceries.txt", sep = ",")
summary(groceries)
################################################################################33
#9835 rows refer to the store transactions,
#and 169 columns are features for each of the 169 different items that might appear in
#someone's grocery basket.

#Each cell in the matrix is a 1 if the item was purchased for
#the corresponding transaction, or 0 otherwise.

  #The density value of 0.02609146 (2.6 percent) refers to the proportion of non-zero
  #matrix cells.

#total of 1662115 * 0.02609146 = 43367 items were purchased during
#the store's 30 days of operation (assuming no duplicate items were purchased).
######################################################################################

# most commonly found in the transactional data.

######################################################################################

# set of statistics about the size of transactions.

######################################################################################
# The first five transactions can be viewed as follows:
inspect(groceries[1:5])

#To examine a particular
#item (that is, a column of data), it is possible use the row, column matrix notion.
#Using this with the itemFrequency() function allows us to see the proportion of
#transactions that contain the item.
itemFrequency(groceries[, 1:3])
# This allows you to produce at a bar chart depicting the proportion of transactions
# containing certain items.
itemFrequencyPlot(groceries, support = 0.1)

itemFrequencyPlot(groceries, topN = 20)

image(sample(groceries, 100))

groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.006, confidence = 0.25, minlen = 2))

groceryrules

summary(groceryrules)

inspect(groceryrules[1:3])

berryrules <- subset(groceryrules, items %in% "berries")

inspect(berryrules)

