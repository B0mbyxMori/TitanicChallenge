trainingSet <- read.csv("../Datasets/train.csv")
testingSet <- read.csv("../Datasets/test.csv")

# survivalRate - Reports survival rate.
survivalRate <- function(column = NULL, variable1 = NULL, variable2 = NULL) {
	if(missing(column) && missing(variable1) && missing(variable2)) {
		return(overallSurvivalRate())
	}

	if(missing(variable2)
		&& typeof(column) == "character"
		&& (typeof(variable1) == "character" || typeof(variable1) == "double" || typeof(variable1) == "integer")) {
		return(returnSurvivalRateMean(buildDiscreteSurvivalList(column, variable1)))
	}

	if(typeof(column) == "character"
		&& (typeof(variable1) == "double" || typeof(variable1) == "integer")
		&& (typeof(variable2) == "double" || typeof(variable2) == "integer")) {
		return(returnSurvivalRateMean(buildContinuousSurvivalList(column, variable1, variable2)))
	}

	stop("Bad argument(s).")
}

# overallSurvivalRate - Returns survival rate of entire dataset.
overallSurvivalRate <- function() {
	mean(trainingSet$Survived) * 100
}

# returnSurvivalRateMean - Returns overallSurvivalRate if missing argument.
# If list is supplied, returns the survival rate.
returnSurvivalRateMean <- function(obtainedList = NULL) {
	if(missing(obtainedList)) {
		return(overallSurvivalRate())
	}

	if(typeof(obtainedList) != "list") {
		stop("Bad argument.")
	}

	mean(obtainedList$trainingSet.Survived) * 100
}

# buildDiscreteSurvivalList - Returns list with 2 columns: the column filtered to include variable1 and Survived.
buildDiscreteSurvivalList <- function(column, variable1) {
	completeDiscreteSurvivalDataFrame = na.omit(data.frame(trainingSet$Survived, trainingSet[column]))
	subsetDiscreteSurvivalList = subset(completeDiscreteSurvivalDataFrame,
		completeDiscreteSurvivalDataFrame[column] == variable1)

	return(subsetDiscreteSurvivalList)
}

# buildContinuousSurvivalList - returns list with 2 columns: the column filtered to include data
# between variable1 and variable2 inclusive and Survived.
buildContinuousSurvivalList <- function(column, variable1, variable2) {
	completeContinousSurvivalDataFrame = na.omit(data.frame(trainingSet$Survived, trainingSet[column]))
	subsetContinousSurvivalList = subset(completeContinousSurvivalDataFrame,
		variable1 <= completeContinousSurvivalDataFrame[column] & variable2 >= completeContinousSurvivalDataFrame[column])

	return(subsetContinousSurvivalList)
}

# populationPercentage - Returns the percentage the obtained population is of the total population.
populationPercentage <- function(obtainedList) {
	if(missing(obtainedList)) {
		stop("Requires list.")
	}

	populationPercentage = (nrow(obtainedList) / nrow(trainingSet)) * 100

	return(populationPercentage)
}


# survivalRateDifference - Reports the difference of the obtained (discrete or continuous) survival rate from
# overall survival rate.
survivalRateDifference <- function(obtainedSurvivalRate) {
	return(obtainedSurvivalRate - overallSurvivalRate())
}


# selectDataSetLine - Selects line from data set.
selectDataSetLine <- function(dataSetLineNumber) {
	columnAndLineData = testingSet[c(dataSetLineNumber),]

	return(columnAndLineData)
}

# generateLine - Generates a line to be added to be appended to the prediction csv file.
# NOTE: Function's logic depends on the assumption dataSetLine has at least one non-NA data point.
generateLine <- function(dataSetLine) {
	referenceColumnList = list(
		c(columnName = "Pclass", quantitativeVariableType = "discrete"),
		c(columnName = "Sex", quantitativeVariableType = "discrete"),
		c(columnName = "Age", quantitativeVariableType = "continuous"),
		c(columnName = "SibSp", quantitativeVariableType = "discrete"),
		c(columnName = "Parch", quantitativeVariableType = "discrete"),
		c(columnName = "Fare", quantitativeVariableType = "continuous"),
		c(columnName = "Cabin", quantitativeVariableType = "discrete"),
		c(columnName = "Embarked", quantitativeVariableType = "discrete")
	)

	totalSurvivalRate = 0
	totalSurvivalRatesCalculated = 0

	for(referenceColumn in referenceColumnList) {
		data = dataSetLine[[referenceColumn[[1]]]]

		# NOTE: This logic is not tested yet.
		if(!(is.null(data)) && !(is.na(data))) {
			individualSurvivalRate = survivalRateCaller(referenceColumn[[1]], data, referenceColumn[[2]])
			totalSurvivalRatesCalculated = totalSurvivalRatesCalculated + 1

			if(totalSurvivalRatesCalculated == 0) {
				totalSurvivalRate = individualSurvivalRate
			} else {
				if(!(is.na(individualSurvivalRate))) {
					totalSurvivalRate = totalSurvivalRate + individualSurvivalRate
				} else {
					# Decrement totalSurvivalRatesCalculated if individualSurvivalRate is na.
					totalSurvivalRatesCalculated = totalSurvivalRatesCalculated - 1
				}
			}
		}
	}

	totalSurvivalRate = (totalSurvivalRate / totalSurvivalRatesCalculated)

	survivalGuess = survivalPrediction(totalSurvivalRate)

	assembledLine = assembleLine(dataSetLine[[1]], survivalGuess)
	return(assembledLine)
}

# survivalRateCaller - Generates a call to survivalRate based on the data and quantitative type.
survivalRateCaller <- function(column, data, quantitativeVariableType) {
	if(quantitativeVariableType == "discrete") {
		return(survivalRate(column, data))
	}

	if(quantitativeVariableType == "continuous") {
		if(data < 5) {
			return(survivalRate(column, 0, (data * 2)))
		}
	}

	return(survivalRate(column, (data - 5), (data + 5)))
}

# assembleLine - Assembles a line to be appended to the prediction csv file.
assembleLine <- function(passengerId, survivalGuess) {
	returnLine = paste(passengerId, ",", survivalGuess, sep = "")
	return(returnLine)
}

survivalPrediction <- function(totalSurvivalRate) {
	if(totalSurvivalRate >= 49.99) {
		return(1)
	}

	return(0)
}

# generateFile - Kick off sequence that generates the prediction csv file.
generateFile <- function() {
	file.create("../Datasets/survivalPredictionDataSet.csv")
	survivalPredictionDataSet = file("../Datasets/survivalPredictionDataSet.csv")

	cat("PassengerId,Survived", file = survivalPredictionDataSet, sep = "\n")

	connection = file("../Datasets/test.csv")
	dataSetTotalLineNumber = length(readLines(connection)) - 1
	close(connection)

	currentDataSetLineNumber = 1

	while(currentDataSetLineNumber <= dataSetTotalLineNumber) {
		dataSetLine = selectDataSetLine(currentDataSetLineNumber)
		pushLine = generateLine(dataSetLine)

		cat(pushLine, file = "../Datasets/survivalPredictionDataSet.csv", sep = "\n", fill = FALSE, labels = NULL, append = TRUE)
		currentDataSetLineNumber = currentDataSetLineNumber + 1
	}
}

## Kick off
generateFile()