trainingSet <- read.csv("../Datasets/train.csv")

# survivalRate - Reports survival rate
survivalRate <- function(column = NULL, variable1 = NULL, variable2 = NULL) {
	if(missing(column) && missing(variable1) && missing(variable2)) {
		return(overallSurvivalRate())
	}

	if(missing(variable2)
		&& typeof(column) == "character"
		&& (typeof(variable1) == "character" || typeof(variable1) == "double")) {
		return("discreteSurvivalRate called.")
	}

	if(typeof(column) == "character"
		&& typeof(variable1) == "double"
		&& typeof(variable2) == "double") {
		return("continuousSurvivalRate called.")
	}

	stop("Bad argument(s).") # TO DO: Deliver detailed error message.
}

# No arguments - returns survival rate of entire dataset
overallSurvivalRate <- function() {
	mean(trainingSet$Survived) * 100
}

# returnSurvivalRateMean - Returns overallSurvivalRate if missing argument.
# If data frame is supplied, returns the survival rate.
# TO DO: Add data type validation
returnSurvivalRateMean <- function(obtainedDataFrame = NULL) {
	if(missing(obtainedDataFrame)) {
		return(overallSurvivalRate())
	}

	mean(obtainedDataFrame$trainingSet.Survived) * 100
}

# Column, variable1 (for discrete data) - returns survival rate of variable1.
# For example, survivalRate(Sex, female) would return the survival rate of
# all females in column.
# unique(trainingSet$Sex) # Back pocket function
discreteSurvivalRate <- function(column, variable1) {
	completeDiscreteSurvivalRateDataFrame = na.omit(data.frame(trainingSet$Survived, trainingSet[column]))
	subsetDiscreteSurvivalRateDataFrame = subset(completeDiscreteSurvivalRateDataFrame,
		completeDiscreteSurvivalRateDataFrame[column] == variable1)

	# mean(subsetDiscreteSurvivalRateDataFrame$trainingSet.Survived) * 100
	returnSurvivalRateMean(subsetDiscreteSurvivalRateDataFrame)
}

# Column, low_numeric, high_numeric (for continuous data) - returns survival rate
# for passengers that fall within the low_numeric and high_numeric range. For example,
# survivalRate(Fare, 2, 50) will return the survival rate of all passengers
# that paid fare between and including $2 and $50 dollars.
continuousSurvivalRate <- function(column, variable1, variable2) {
	completeContinousSurvivalRateDataFrame = na.omit(data.frame(trainingSet$Survived, trainingSet[column]))
	subsetContinousSurvivalRateDataFrame = subset(completeContinousSurvivalRateDataFrame,
		variable1 <= completeContinousSurvivalRateDataFrame[column] & variable2 >= completeContinousSurvivalRateDataFrame[column])

	# mean(subsetContinousSurvivalRateDataFrame$trainingSet.Survived) * 100
	returnSurvivalRateMean(subsetContinousSurvivalRateDataFrame)
}

# populationPrecentage - Reports the precentage of the population
	# Column, character - returns the population precentage of character. For example,
		# survivalRate(Sex, female) returns the precentage of females that were on-board.
	# Column, low_numeric, high_numeric - returns the population precentage of
		# passengers that fall within the given range. survivalRate(Fare, 2, 50) will
		# return the population of passagers that paid fair from $2 to $50.


# survivalRateDifference - Reports the difference of the obtained (discrete or continuous) survival rate from
# overall survival rate.
survivalRateDifference <- function(obtainedSurvivalRate) {
	return(obtainedSurvivalRate - overallSurvivalRate())
}

# confidenceScore - Reports a precentage that guesses how bias the data point may be. The
# higher the number, the more likely the data point may be bias.

## REMOVE
discreteSurvivalRate("Sex", "male") # Returns 18.89081
discreteSurvivalRate("Sex", "female") # Returns 74.20382
## REMOVE


## REMOVE
continuousSurvivalRate("Age", 0, 1) # Returns 85.71429
continuousSurvivalRate("Age", 50, 51) # Returns 41.17647
## REMOVE


## REMOVE
survivalRateDifference(continuousSurvivalRate("Age", 0, 1))
survivalRateDifference(continuousSurvivalRate("Age", 50, 51))
## REMOVE