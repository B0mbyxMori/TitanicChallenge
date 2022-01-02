# trainingSet <- read.csv("../Datasets/train.csv")
researchModel <- source("ResearchModel.r")

# NOTE: Remove repeated code and review code.
# TO DO: Printed statement display function name with pass/fail information.
# TO DO: Separate functions.
runSurvivalRateTests <- function() {
	print(survivalRateNoArgumentsReturnsDouble())
	print(survivalRateCharacterColumnCharacterTestVariable1ReturnsDouble(character(), character()))
	print(survivalRateCharacterColumnDoubleTestVariable1ReturnsDouble(character(), double()))
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
	print(selectLineReturnsList())
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
selectDataSetLineReturnsList <- function() {
	if(typeof(selectDataSetLine()) == "list") {
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
	# Required Tests:
		# UPDATE: After adding survivalRate(...) call logic.
## END: survivalRateCaller Tests


## START: assembleLine Tests
	# Required Tests:
		# Verify character is returned.
	#Extra Tests:
		# Verify returned character contains a comma between other chars.
## END: assembleLine Tests


## START: survivalPrediction Tests
	# Required Tests:
		# Verify 0 is returned if totalSurvivalRate is less than 49.99.
		# Verify 1 is returned if totalSurivivalRate is greater or equal to 49.99.
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