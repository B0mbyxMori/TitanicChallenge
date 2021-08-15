# GenericSurvivalStats class will be the parent for all SurvivalStats classes.
# trainingSet <- read.csv("../Datasets/train.csv")

# Responsibilities:

# Accept variable from user and read csv table.
# (Key(s) will be added to child classes)
# Assemble a ReturnStat object.
# Return a ReturnStats object to user.


# Assemble:

# overallSurvivalRate - Reports survival rate of entire dataset
	# mean(trainingSet$Survived) * 100

# variableSurvivalRate - Reports overall survival rate of variable

# variableKeySurvivalRate - Reports survival rate of variable and key

# populationPrecentage - Reports the precentage of the population that
# belongs to the variable/key pair

# biasScore - Attempts to guess how bias the information produced may be
# based on populationPrecentage, NAs, etc.

# standardDeviation - Reports standard deviation from overall survival
# rate

setClass("GenericSurvivalStats", representation(
	# table = ADD: call method to generate table
	variable = "character")
)