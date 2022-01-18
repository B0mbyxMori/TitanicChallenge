# trainingSet <- read.csv("../Datasets/train.csv")
researchModel <- source("ResearchModel.r")

# NOTE: Remove repeated code and review code. Some tests are rigid.
# TO DO: Printed statement display function name with pass/fail information.
# TO DO: Separate functions.
runSurvivalRateTests <- function() {
	print(survivalRateNoArgumentsReturnsDouble())
	print(survivalRateCharacterColumnCharacterTestVariable1ReturnsDouble(character(), character()))
	print(survivalRateCharacterColumnDoubleTestVariable1ReturnsDouble(character(), double()))
	print(survivalRateCharacterColumnIntegerTestVariableReturnsDouble(character(), integer()))
	print(survivalRateCharacterColumnNotCharacterOrDoubleTestVariable1ReturnsExpectedError(character(), (!double() | !character())))
	print(survivalRateNotCharacterColumnDoubleTestVariable1ReturnsExpectedError(!character(), double()))
	print(survivalRateCharacterColumnDoubleTestVariable1DoubleTestVariable2ReturnsDouble(character(), double(), double()))
	print(survivalRateCharacterColumnNotDoubleTestVariable1DoubleTestVariable2ReturnsExpectedError(character(), !double(), double()))
	print(survivalRateCharacterColumnDoubleTestVariable1NotDoubleTestVariable2ReturnsExpectedError(character(), double(), !double()))
	print(survivalRateNotCharacterColumnDoubleTestVariable1DoubleTestVariable2ReturnsExpectedError(!character(), double(), double()))
	print(returnSurvivalRateMeanEmptyParameterReturnsDouble())
	print(returnSurvivalRateMeanObtainedListReturnsDouble(data.frame("trainingSet.Survived" = c(0,0,1,1))))
	print(returnSurvivalRateMeanNotListReturnsError(!list()))
	# print(overallSurvivalRateReturnsDouble())
	print(buildDiscreteSurvivalListReturnsList(character(), character())) # Assumes testVariable1 is character for now.
	print(buildContinuousSurvivalListReturnsList(character(), double(), double()))
	print(populationPercentageReturnsDouble(matrix(c(0, 0, 0, 0), nrow = 2))) # Fake is double for now.
	print(populationPercentageMissingListReturnsError())
	print(survivalRateDifferenceReturnsDouble(double()))
	print(selectDataSetLineReturnsList(1))
	print(survivalRateCallerReturnsDoubleForDiscreteData("Sex", "female", "discrete"))
	print(survivalRateCallerReturnsDoubleForContinuousDataGreaterThanFive("Age", 7, "continuous"))
	print(survivalRateCallerReturnsDoubleForContinuousDataLessThanFive("Age", 3, "continuous"))
	print(survivalPredictionReturns0IftotalSurvialRateIsLessThanThreshold(48.50))
	print(survivalPredictionReturns1IftotalSurvivalRateIsGreaterThanThreshold(51.50))
}

## START: survivalRate Tests
survivalRateNoArgumentsReturnsDouble <- function() {
	if(typeof(survivalRate()) == "double") {
		return(TRUE)
	}

	return(FALSE)
}

survivalRateCharacterColumnCharacterTestVariable1ReturnsDouble <- function(testColumn, testVariable1) {
	if(typeof(survivalRate(testColumn, testVariable1)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}

survivalRateCharacterColumnDoubleTestVariable1ReturnsDouble <- function(testColumn, testVariable1) {
	if(typeof(survivalRate(testColumn, testVariable1)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}

survivalRateCharacterColumnIntegerTestVariableReturnsDouble <- function(testColumn, testVariable1) {
	if(typeof(survivalRate(testColumn, testVariable1)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}

survivalRateCharacterColumnNotCharacterOrDoubleTestVariable1ReturnsExpectedError <- function(testColumn, testVariable1) {
	tryCatch(
		expr = {
			survivalRate(testColumn, testVariable1)
			return(FALSE)
		},
		error = function(errorReturned) {
			if(errorReturned[1] == "Bad argument(s).") {
				return(TRUE)
			}

			return(FALSE)
		}
	)
}

survivalRateNotCharacterColumnDoubleTestVariable1ReturnsExpectedError <- function(testColumn, testVariable1) {
	tryCatch(
		expr = {
			survivalRate(testColumn, testVariable1)
			return(FALSE)
		},
		error = function(errorReturned) {
			if(errorReturned[1] == "Bad argument(s).") {
				return(TRUE)
			}

			return(FALSE)
		}
	)
}

survivalRateCharacterColumnDoubleTestVariable1DoubleTestVariable2ReturnsDouble <- function(testColumn, testVariable1, testVariable2) {
	if(typeof(survivalRate(testColumn, testVariable1, testVariable2)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}

survivalRateCharacterColumnNotDoubleTestVariable1DoubleTestVariable2ReturnsExpectedError <- function(testColumn, testVariable1, testVariable2) {
	tryCatch(
		expr = {
			survivalRate(testColumn, testVariable1, testVariable2)
			return(FALSE)
		},
		error = function(errorReturned) {
			if(errorReturned[1] == "Bad argument(s).") {
				return(TRUE)
			}

			return(FALSE)
		}
	)
}

survivalRateCharacterColumnDoubleTestVariable1NotDoubleTestVariable2ReturnsExpectedError <- function(testColumn, testVariable1, testVariable2) {
	tryCatch(
		expr = {
			survivalRate(testColumn, testVariable1, testVariable2)
			return(FALSE)
		},
		error = function(errorReturned) {
			if(errorReturned[1] == "Bad argument(s).") {
				return(TRUE)
			}

			return(FALSE)
		}
	)
}

survivalRateNotCharacterColumnDoubleTestVariable1DoubleTestVariable2ReturnsExpectedError <- function(testColumn, testVariable1, testVariable2) {
	tryCatch(
		expr = {
			survivalRate(testColumn, testVariable1, testVariable2)
			return(FALSE)
		},
		error = function(errorReturned) {
			if(errorReturned[1] == "Bad argument(s).") {
				return(TRUE)
			}

			return(FALSE)
		}
	)
}
## END: survivalRate Tests


## TO DO: Add tests.
## START: returnSurvivalRateMean Tests
returnSurvivalRateMeanEmptyParameterReturnsDouble <- function() {
	if(typeof(returnSurvivalRateMean()) == "double") {
		return(TRUE)
	}

	return(FALSE)
}

returnSurvivalRateMeanNotListReturnsError <- function(testObtainedObject) {
	tryCatch(
		expr = {
			returnSurvivalRateMean(testObtainedObject)
			return(FALSE)
		},
		error = function(errorReturned) {
			if(errorReturned[1] == "Bad argument.") {
				return(TRUE)
			}

			return(FALSE)
		}
	)
}

# NOTE: Data frame fake created during function call.
returnSurvivalRateMeanObtainedListReturnsDouble <- function(testObtainedList) {
	if(typeof(returnSurvivalRateMean(testObtainedList)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}
## END: returnSurvivalRateMean Tests


## START: overallSurvivalRate Tests
# NOTE: Kinda pointless as it's just testing an external function's return (mean).
# overallSurvivalRateReturnsDouble <- function() {
# 	if(typeof(overallSurvivalRate()) == "double") {
# 		return(TRUE)
# 	}

# 	return(FALSE)
# }
## END: overallSurvivalRate Tests


## START: buildDiscreteSurvivalList Tests
buildDiscreteSurvivalListReturnsList <- function(testColumn, testVariable1) {
	if(typeof(buildDiscreteSurvivalList(testColumn, testVariable1)) == "list") {
		return(TRUE)
	}

	return(FALSE)
}
## END: buildDiscreteSurvivalList Tests


## START: buildContinuousSurvivalList Tests
buildContinuousSurvivalListReturnsList <- function(testColumn, testVariable1, testVariable2) {
	if(typeof(buildContinuousSurvivalList(testColumn, testVariable1, testVariable2)) == "list") {
		return(TRUE)
	}

	return(FALSE)
}
## END: buildContinuousSurvivalList Tests


## START: populationPercentage Tests
populationPercentageReturnsDouble <- function(testObtainedList) {
	if(typeof(populationPercentage(testObtainedList)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}

populationPercentageMissingListReturnsError <- function() {
	tryCatch(
		expr = {
			populationPercentage()
			return(FALSE)
		},
		error = function(errorReturned) {
			if(errorReturned[1] == "Requires list.") {
				return(TRUE)
			}

			return(FALSE)
		}
	)
}
## END: populationPercentage Tests


## START: survivalRateDifference Tests
survivalRateDifferenceReturnsDouble <- function(testObtainedSurvivalRate) {
	if(typeof(survivalRateDifference(testObtainedSurvivalRate)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}
## END: survivalRateDifference Tests


## START: confidenceScore Tests

## END: confidenceScore Tests


## START: selectDataSetLine Tests
# NOTE: If params are added, test will break.
selectDataSetLineReturnsList <- function(testDataSetLineNumber) {
	if(typeof(selectDataSetLine(testDataSetLineNumber)) == "list") {
		return(TRUE)
	}

	return(FALSE)
}
## END: selectDataSetLine Tests


## TO DO: Add to test lists.

## START: generateLine Tests
# NOTE: Maybe function is too large, difficult to test.
	# Required Tests:
		# Verify list is returned.
	# Extra Tests:
		# "" Test (If run once with NA data point, what's the return?)
## END: generateLine Tests


## START: survivalRateCaller Tests
## NOTE: survivalRateCaller tests depend on survivalRate(...). If survivalRate(...) breaks,
## these tests will also be affected.

## NOTE: If more ...fakeBuilders(...) are created, organize in different file.
# columnAndDataFakeBuilder <- function(fakePassengerId, fakeColumn, fakeData) {
# 	fake = data.frame(fakeData)
# 	names(fake)[1] = as.character(fakeColumn)
# 	row.names(fake) = fakePassengerId

# 	return(fake)
# }

survivalRateCallerReturnsDoubleForDiscreteData <- function(testColumn, testData, testQuantitativeVariableType) {
	if(typeof(survivalRateCaller(testColumn, testData, testQuantitativeVariableType)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}

survivalRateCallerReturnsDoubleForContinuousDataGreaterThanFive <- function(testColumn, testData, testQuantitativeVariableType) {
	if(typeof(survivalRateCaller(testColumn, testData, testQuantitativeVariableType)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}

survivalRateCallerReturnsDoubleForContinuousDataLessThanFive <- function(testColumn, testData, testQuantitativeVariableType) {
	if(typeof(survivalRateCaller(testColumn, testData, testQuantitativeVariableType)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}
## END: survivalRateCaller Tests


## START: assembleLine Tests
	# Required Tests:
		# Verify character is returned.
	#Extra Tests:
		# Verify returned character contains a comma between other chars.
## END: assembleLine Tests


## START: survivalPrediction Tests
survivalPredictionReturns0IftotalSurvialRateIsLessThanThreshold <- function(testTotalSurvivalRate) {
	if(survivalPrediction(testTotalSurvivalRate) == 0) {
		return(TRUE)
	}

	return(FALSE)
}

survivalPredictionReturns1IftotalSurvivalRateIsGreaterThanThreshold <- function(testTotalSurvivalRate) {
	if(survivalPrediction(testTotalSurvivalRate) == 1) {
		return(TRUE)
	}

	return(FALSE)
}
## END: survivalPrediction Tests


## START: generateFile Tests
	# Required Tests:
		# Verify csv file is returned.
## END: generateFile Tests


# Test Suite
runTests <- function() {
	runSurvivalRateTests()
}

runTests()