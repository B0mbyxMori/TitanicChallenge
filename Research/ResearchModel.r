trainingSet <- read.csv("../Datasets/train.csv")

# survivalRate - Reports survival rate
survivalRate <- function(column = NULL, variable1 = NULL, variable2 = NULL) {
	if(missing(column) && missing(variable1) && missing(variable2)) {
		return(overallSurvivalRate())
	}

	if(missing(variable2)
		&& typeof(column) == "character"
		&& (typeof(variable1) == "character" || typeof(variable1) == "double")) {
		return(returnSurvivalRateMean(buildDiscreteSurvivalList(column, variable1)))
	}

	if(typeof(column) == "character"
		&& typeof(variable1) == "double"
		&& typeof(variable2) == "double") {
		return(returnSurvivalRateMean(buildContinuousSurvivalList(column, variable1, variable2)))
	}

	stop("Bad argument(s).") # TO DO: Deliver detailed error message.
}

# No arguments - returns survival rate of entire dataset
overallSurvivalRate <- function() {
	mean(trainingSet$Survived) * 100
}

# returnSurvivalRateMean - Returns overallSurvivalRate if missing argument.
# If list is supplied, returns the survival rate.
# TO DO: Add list validation test
returnSurvivalRateMean <- function(obtainedDataFrame = NULL) {
	if(missing(obtainedDataFrame)) {
		return(overallSurvivalRate())
	}

	if(typeof(obtainedDataFrame) != "list") {
		stop("Bad argument.") # TO DO: Deliver detailed error message and verify columns.
	}

	mean(obtainedDataFrame$trainingSet.Survived) * 100
}

# Column, variable1 (for discrete data) - returns list with 2 columns: the
# column filtered to include variable1 and Survived.
# unique(trainingSet$Sex) # Back pocket function
buildDiscreteSurvivalList <- function(column, variable1) {
	completeDiscreteSurvivalDataFrame = na.omit(data.frame(trainingSet$Survived, trainingSet[column]))
	subsetDiscreteSurvivalList = subset(completeDiscreteSurvivalDataFrame,
		completeDiscreteSurvivalDataFrame[column] == variable1)

	return(subsetDiscreteSurvivalList)
}

# Column, low_numeric, high_numeric (for continuous data) - returns list with 2 columns:
# the column filtered to include data between variable1 and variable2 inclusive and Survived.
buildContinuousSurvivalList <- function(column, variable1, variable2) {
	completeContinousSurvivalDataFrame = na.omit(data.frame(trainingSet$Survived, trainingSet[column]))
	subsetContinousSurvivalList = subset(completeContinousSurvivalDataFrame,
		variable1 <= completeContinousSurvivalDataFrame[column] & variable2 >= completeContinousSurvivalDataFrame[column])

	return(subsetContinousSurvivalList)
}

# populationPrecentage - Reports the precentage of the population
	# Column, character - returns the population precentage of character. For example,
		# survivalRate(Sex, female) returns the precentage of females that were on-board.
	# Column, low_numeric, high_numeric - returns the population precentage of
		# passengers that fall within the given range. survivalRate(Fare, 2, 50) will
		# return the population of passagers that paid fair from $2 to $50.
## nrow(...) builds


# survivalRateDifference - Reports the difference of the obtained (discrete or continuous) survival rate from
# overall survival rate.
survivalRateDifference <- function(obtainedSurvivalRate) {
	return(obtainedSurvivalRate - overallSurvivalRate())
}

# confidenceScore - Reports a precentage that guesses how bias the data point may be. The
# higher the number, the more likely the data point may be bias.