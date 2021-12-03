# TO DO: Deal with NAs
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

# TO DO: Will build survivalPrediction (as prototype) before building confidenceScore (refined product)
# confidenceScore - Reports a precentage that guesses how bias the data point may be. The
# higher the number, the more likely the data point may be bias.

# survivalPrediction - Returns a survival prediction in the form of a 0 (passed on) or 1 (survived).
survivalPrediction <- function(dataPoint) {
}

## TO DO: Build out file generator and helper functions.
# selectDataPoint - Will read dataset one line at a time.

# generateCSVfile - Returns CSV with two columns: PassengerId and Survived


## REMOVE
# Survived,Pclass,Name,Sex,Age,SibSp,Parch,Ticket,Fare,Cabin,Embarked
##

# 0,3,"Braund, Mr. Owen Harris",male,22,1,0,A/5 21171,7.25,,S
# Pass (26.47 < 50.00)
(survivalRate("Pclass", 3) + survivalRate("Sex", "male") + survivalRate("Age", (22 - 5), (22 + 5))) / 3

# 1,3,"McDermott, Miss. Brigdet Delia",female,,0,0,330932,7.7875,,Q
# Fail (49.22 < 50.00)
(survivalRate("Pclass", 3) + survivalRate("Sex", "female") ) / 2


# 1,2,"Smith, Miss. Marion Elsie",female,40,0,0,31418,13,,S
# Pass (54.63 > 50.00)
(survivalRate("Pclass", 2) + survivalRate("Sex", "female") + survivalRate("Age", (40 - 5), (40 + 5))) / 3

# 0,1,"Widener, Mr. Harry Elkins",male,27,0,2,113503,211.5,C82,C
# Pass (40.50 < 50.00)
(survivalRate("Pclass", 1) + survivalRate("Sex", "male") + survivalRate("Age", (27 - 5), (27 + 5))) / 3

# 0,3,"Peduzzi, Mr. Joseph",male,,0,0,A/5 2817,8.05,,S
# Pass (21.56 < 50.00)
(survivalRate("Pclass", 3) + survivalRate("Sex", "male")) / 2

# 1,3,"Peter, Mrs. Catherine (Catherine Rizk)",female,,0,2,2668,22.3583,,C
# Fail (49.22 < 50.00)
(survivalRate("Pclass", 3) + survivalRate("Sex", "female")) / 2

# 1,3,"Leeni, Mr. Fahim (""Philip Zenni"")",male,22,0,0,2620,7.225,,C
# Fail (26.47 < 50.00)
(survivalRate("Pclass", 3) + survivalRate("Sex", "male") + survivalRate("Age", (22 - 5), (22 + 5))) / 3

# 1,3,"Jonsson, Mr. Carl",male,32,0,0,350417,7.8542,,S
# Fail (28.70 < 50.00)
(survivalRate("Pclass", 3) + survivalRate("Sex", "male") + survivalRate("Age", (32 - 5), (32 + 5))) / 3

## REMOVE