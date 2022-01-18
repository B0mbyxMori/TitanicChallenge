# TO DO: Deal with NAs
# FIX: Update functions to accept any dataset
trainingSet <- read.csv("../Datasets/train.csv")
testingSet <- read.csv("../Datasets/test.csv")

# survivalRate - Reports survival rate
survivalRate <- function(column = NULL, variable1 = NULL, variable2 = NULL) {
	if(missing(column) && missing(variable1) && missing(variable2)) {
		return(overallSurvivalRate())
	}

	## REMOVE
	# TO DO: Remove double check and associated tests since data comes in as integer.
	# Add tests for integer add on continuous check.
	## REMOVE
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

	stop("Bad argument(s).") # TO DO: Deliver detailed error message.
}

# No arguments - returns survival rate of entire dataset
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
		stop("Bad argument.") # TO DO: Deliver detailed error message
	}

	# TO DO: Create verify column function and call it here

	mean(obtainedList$trainingSet.Survived) * 100
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

# populationPercentage - Returns the percentage the obtained population is of the total population.
populationPercentage <- function(obtainedList) {
	if(missing(obtainedList)) {
		stop("Requires list.") # TO DO: Deliver detailed error message
	}

	# TO DO: Create verify column function and call it here (NOTE: Will break populationPercentageReturnsDouble test b/c fake.)

	populationPercentage = (nrow(obtainedList) / nrow(trainingSet)) * 100

	return(populationPercentage)
}


# survivalRateDifference - Reports the difference of the obtained (discrete or continuous) survival rate from
# overall survival rate.
survivalRateDifference <- function(obtainedSurvivalRate) {
	return(obtainedSurvivalRate - overallSurvivalRate())
}


# confidenceScore - Reports a precentage that guesses how bias the data point may be. The
# higher the number, the more likely the data point may be bias.


# colnames(...) - Back pocket function
selectDataSetLine <- function(dataSetLineNumber) { # UPDATE: function(anyDataSet, dataSetLineNumber)
	columnAndLineData = trainingSet[c(dataSetLineNumber),] # columnAndLineData = anyDataSet[c(dataSetLineNumber),]

	return(columnAndLineData)
}

## ADD: Test for new functions.

# NOTE: Function's logic depends on the assumption dataSetLine has at least one non-NA data point.
## FUNCTION DEPENDENCIES: survivalRateCaller(...)
generateLine <- function(dataSetLine) {
	# TO DO: Place in seperate file and reference. NOTE: Once moved to a seperate file, remove
	# as param for columnMatcher.
	referenceColumnList = list(
		c(columnName = "Pclass", quantitativeVariableType = "discrete"),
		c(columnName = "Sex", quantitativeVariableType = "discrete"),
		c(columnName = "Age", quantitativeVariableType = "continuous"),
		c(columnName = "SibSp", quantitativeVariableType = "discrete"),
		c(columnName = "Parch", quantitativeVariableType = "discrete"),
		# c(columnName = "Ticket", quantitativeVariableType = "continuous"),
		c(columnName = "Fare", quantitativeVariableType = "continuous"),
		c(columnName = "Cabin", quantitativeVariableType = "discrete"),
		c(columnName = "Embarked", quantitativeVariableType = "discrete")
	)

	totalSurvivalRate = 0
	totalSurvivalRatesCalculated = 0

	# FIX: Design this alongside referenceColumnList.
	for(referenceColumn in referenceColumnList) {
		## Step 1: (Above in outer for loop) Correctly build columnReferenceList and
		## loop over all referenceColumns.
		## Step 2: Use referenceColumn to choose correct column in dataSetLine (make list var containing dataSetColumn and variable)
		## (Handled by survivalRateCaller) Step 3: Use referenceColumn to identify if column is continuous or discrete. If is
		## continuous, send dataSetList though logic to add variable2.
		## (Handled by survivalRateCaller) Step 4: Call survivalRate(...)
		## Step 5: Use survivalRate(...) double to calculate the totalSurvivalRate (so far) and increment
		## totalSurvivalRatesCalculated by 1.
		## Step 6: Fall out of loop and calculate the totalSurvivalRate.
		## (Handled by survivalPrediction(...)) Step 7: If totalSurvivalRate >= 49.99, return 1. Else, return 0.
		## (Handled by assembleLine(...)) Step 8: Generate line.
		## Step 9: Return assembledLine (more hand waving for now).

		data = dataSetLine[[referenceColumn[[1]]]]

		if(data != "") { # Error on passenger 6. # || !is.na(data)
			# Error:  missing value where TRUE/FALSE needed
			individualSurvivalRate = survivalRateCaller(referenceColumn[[1]], data, referenceColumn[[2]])
			# print(individualSurvivalRate)
			totalSurvivalRatesCalculated = totalSurvivalRatesCalculated + 1

			if(totalSurvivalRatesCalculated == 0) {
				totalSurvivalRate = individualSurvivalRate
			} else {
				totalSurvivalRate = totalSurvivalRate + individualSurvivalRate
			}
		}
	}

	totalSurvivalRate = (totalSurvivalRate / totalSurvivalRatesCalculated)

	survivalGuess = survivalPrediction(totalSurvivalRate)

	assembledLine = assembleLine(dataSetLine[[1]], survivalGuess)
	return(assembledLine) # Example: 89,1

	# return(totalSurvivalRate)
}

# List time complexity: https://www.refsmmat.com/posts/2016-09-12-r-lists.html
# NOTE: Worried about computation time.
## FUNCTION DEPENDENCIES: survivalRate(...)
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

assembleLine <- function(passengerId, survivalGuess) {
	returnLine = paste(passengerId, ',', survivalGuess)
	return(returnLine)
}

survivalPrediction <- function(totalSurvivalRate) {
	if(totalSurvivalRate >= 49.99) {
		return(1)
	}

	return(0)
}

## FUNCTION DEPENDENCIES: selectDataSetLine(...), generateLine(...)
# generateFile <- function() { # UPDATE: function(anyDataSet)
	# survivalPredictionDataSet = file("survivalPredictionDataSet.csv")

	# cat("PassengerId,Survived", file = survivalPredictionDataSet, sep = "\n")

	# dataSetLineNumber = 1
	## Responsible for: Kickoff sequence
		# if(dataSetLineNumber > dataSetTotalLineNumber) {
			# dataSetLine = selectDataSetLine(dataSetLineNumber) # selectDataSetLine(anyDataSet, dataSetLineNumber)
			# pushLine = generateLine(dataSetLine)
			# cat(pushLine, file = survivalPredictionDataSet, sep = "\n", append = TRUE)

			# dataSetLineNumber = dataSetLineNumber + 1
		# }

	# return(survivalPredictionDataSet)
# }

# dataLineCounter()
# dataLineCounter(10)

# generateFile() # generateFile(anyDataSet)
exampleLine = selectDataSetLine(6)
generateLine(exampleLine)

# survivalRateCaller("Sex", "male", "discrete")
# survivalRateCaller("Age", 3, "continuous")