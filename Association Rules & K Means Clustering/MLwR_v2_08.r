##### Chapter 8: Association Rules -------------------

## Example: Identifying Frequently-Purchased Groceries ----
## Step 2: Exploring and preparing the data ----

# load the grocery data into a sparse matrix
library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries) #matrix is ordered alphabetically automatically by item name


# look at the first five transactions
inspect(groceries) #to read complete sparse matrix
inspect(groceries[1:10])

# examine the frequency of items
itemFrequency(groceries[, 1:3])

# plot the frequency of items
itemFrequencyPlot(groceries, support = 0.1) #plots anything that has frequency of 0.1
itemFrequencyPlot(groceries, topN = 20) # plots top 20 items by frequency of order

# a visualization of the sparse matrix for the first five transactions
image(groceries[1:10])

# visualization of a random sample of 100 transactions
image(sample(groceries, 500)) #visualization of sparse matrix of first 500 transactions

## Step 3: Training a model on the data ----
library(arules)

# default settings result in zero rules learned
apriori(groceries) #apriori will try to generate rules

# set better support and confidence levels to learn more rules
groceryrules <- apriori(groceries, parameter = list(support =
                          0.006, confidence = 0.25, minlen = 2)) #25% confidence that user will buy it
#atleast item should occur one in the transaction. so 1/169 = 0.6 is support value
#minlen = 2 atleast 2 items are ordered
groceryrules

## Step 4: Evaluating model performance ----
# summary of grocery association rules
summary(groceryrules)
str(groceryrules)
#eg. item1 --> item2 indicate rulelength of 2
#eg. item1,item2 --> item3 indicate rulelength of 3


# look at the first three rules
inspect(groceryrules[1:3])
inspect(groceryrules) #what is support, confidence and lift
?inspect
#to calculate the support :
itemFrequency(groceries["potted plants"])
## Step 5: Improving model performance ----

# sorting grocery rules by lift
inspect(sort(groceryrules, by = "lift")[1:10])

# finding subsets of rules containing any berry items
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

sodarules = subset(groceryrules, subset = rhs %in% "soda")
inspect(sort(sodarules, by = "lift")[1:5])

###prules = subset(groceryrules, subset = rhs %pin% "ol") #partial count
###crules =
### ccrules = 
# writing the rules to a CSV file
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# converting the rule set to a data frame
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)

groceryrules_dfs = groceryrules
