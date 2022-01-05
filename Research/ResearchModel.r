# TO DO: Deal with NAs
# FIX: Update functions to accept any dataset
trainingSet <- read.csv("../Datasets/train.csv")
testingSet <- read.csv("../Datasets/test.csv")

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
# generateLine <- function(dataSetLine) {
	## START FIX: Determine best way to build list.
	# TO DO: Place in seperate file and reference. NOTE: Once moved to a seperate file, remove
	# as param for columnMatcher.
	# referenceColumnList = list(
	# 	list(columnName = "Pclass", quantitativeVariableType = "discrete"),
	# 	list(columnName = "Sex", quantitativeVariableType = "discrete"),
	# 	list(columnName = "Age", quantitativeVariableType = "continuous"),
	# 	list(columnName = "SibSp", quantitativeVariableType = "discrete"),
	# 	list(columnName = "Parch", quantitativeVariableType = "discrete"),
	#	list(columnName = "Ticket", quantitativeVariableType = "continuous"),
	# 	list(columnName = "Fare", quantitativeVariableType = "continuous"),
	# 	list(columnName = "Cabin", quantitativeVariableType = "discrete"),
	#	list(columnName = "Embarked", quantitativeVariableType = "discrete"),
	# )
	## END FIX: Determine best way to build list.

	# totalSurvivalRate = double()
	# totalSurvivalRatesCalculated = 0


	# FIX: Design this alongside referenceColumnList.
	# for(columnName in referenceColumnList) {
		## Step 1: (Above in outer for loop) Correctly build columnReferenceList and
		## loop over all columnNames.
		## Step 2: Use columnName to choose correct column in dataSetLine (make list var containing dataSetColumn and variable)
		## (Handled by survivalRateCaller) Step 3: Use columnName to identify if column is continuous or discrete. If is
		## continuous, send dataSetList though logic to add variable2.
		## (Handled by survivalRateCaller) Step 4: Call survivalRate(...)
		## Step 5: Use survivalRate(...) double to calculate the totalSurvivalRate (so far) and increment
		## totalSurvivalRatesCalculated by 1.
		## Step 6: Fall out of loop and calculate the totalSurvivalRate.
		## (Handled by survivalPrediction(...)) Step 7: If totalSurvivalRate >= 49.99, return 1. Else, return 0.
		## (Handled by assembleLine(...)) Step 8: Generate line.
		## Step 9: Return assembledLine (more hand waving for now).

		# columnAndData = dataSetLine[[columnName]]

		# if(columnData[[1]] != "") {
		# 	individualSurvivalRate = survivalRateCaller(columnAndData, referenceColumnList[[[columnName]quantitativeVariableType]])
		# 	totalSurvivalRatesCalculated = totalSurvivalRatesCalculated + 1

		# 	if(totalSurvivalRatesCalculated = 0) {
		# 		totalSurvivalRate = individualSurvivalRate
		# 	} else {
		# 		totalSurvivalRate = totalSurvivalRate + individualSurvivalRate
		# 	}
		# }
	# }

	# totalSurvivalRate = (totalSurvivalRate / totalSurvivalRatesCalculated)

	# survivalGuess = survivalPrediction(totalSurvivalRate)

	# assembledLine = assembleLine(referenceColumnList[["PassengerId"]], survivalGuess)
	# return(assembledLine) # Example: 89,1
# }

# List time complexity: https://www.refsmmat.com/posts/2016-09-12-r-lists.html
# NOTE: Worried about computation time.
## FUNCTION DEPENDENCIES: survivalRate(...)
# survivalRateCaller <- function(columnAndData, quantitativeVariableType) {
	# individualSurvivalRate = double()
	# ADD: Check continuous value range.

	## Step 1: Check if continuous or discrete. If discrete call survivalRate(colname(columnAndData), columnAndData[[1]]). Else...
	## Step 2: Check data is greater than 5. If less, set variable1 to 0 and variable2 = (data * 2) and return. Else, set variable1
	## to (data - 5) and variable2 to (data + 5) and return.

	## REMOVE
	# ADD: survivalRate(...) call logic.
	# individualSurvivalRate = survivalRate(...)
	## REMOVE

	# if(quantitativeVariableType = "discrete") {
		# return(survivalRate(colname(columnAndData), columnAndData[[1]])
	# }

	# if(quantitativeVariableType = "continuous") {
		# if(columnAndData[[1]] <= 5) {
			# return(survivalRate(colName(columnAndData), 0, (columnAndData[[1]] * 2)))
		# }
	# }

	# return(survivalRate(colname(columnAndData), (columnAndData[[1]] - 5), (columnAndData[[1]] + 5)))
# }

# assembleLine <- function(passengerId, survivalGuess) {
	# returnLine = paste(passengerId, ',', survivalGuess)
	# return(returnLine)
# }

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

# selectDataSetLine(1)

# dataLineCounter()
# dataLineCounter(10)

# generateFile() # generateFile(anyDataSet)