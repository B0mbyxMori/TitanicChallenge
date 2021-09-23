trainingSet <- read.csv("../Datasets/train.csv")

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

overallSurvivalRate <- function() {
	mean(trainingSet$Survived) * 100
}

# overallSurvivalRate()