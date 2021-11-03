# trainingSet <- read.csv("../Datasets/train.csv")
researchModel <- source("ResearchModel.r")

# NOTE: Remove repeated code and review code.
# TO DO: Printed statement display function name with pass/fail information.
# TO DO: Separate functions.
runSurvivalRateTests <- function() {
	print(survivalRateNoArgumentsReturnsDouble())
	print(survivalRateCharacterColumnCharacterTestVariable1ReturnsExpectedString(character(), character()))
	print(survivalRateCharacterColumnDoubleTestVariable1ReturnsExpectedString(character(), double()))
	print(survivalRateCharacterColumnNotCharacterOrDoubleTestVariable1ReturnsExpectedError(character(), (!double() | !character())))
	print(survivalRateNotCharacterColumnDoubleTestVariable1ReturnsExpectedError(!character(), double()))
	print(survivalRateCharacterColumnDoubleTestVariable1DoubleTestVariable2ReturnsExpectedString(character(), double(), double()))
	print(survivalRateCharacterColumnNotDoubleTestVariable1DoubleTestVariable2ReturnsExpectedError(character(), !double(), double()))
	print(survivalRateCharacterColumnDoubleTestVariable1NotDoubleTestVariable2ReturnsExpectedError(character(), double(), !double()))
	print(survivalRateNotCharacterColumnDoubleTestVariable1DoubleTestVariable2ReturnsExpectedError(!character(), double(), double()))
	print(overallSurvivalRateReturnsDouble())
	print(discreteSurvivalRateReturnsDouble(character(), character())) # Assumes testVariable1 is character for now.
	print(continuousSurvivalRateReturnsDouble(character(), double(), double()))
}

## START: survivalRate Tests
survivalRateNoArgumentsReturnsDouble <- function() {
	if(typeof(survivalRate()) == "double") {
		return(TRUE)
	}

	return(FALSE)
}

# NOTE: Function name will change to "...ReturnsDouble" as ResearchModel grows.
survivalRateCharacterColumnCharacterTestVariable1ReturnsExpectedString <- function(testColumn, testVariable1) {
	if(survivalRate(testColumn, testVariable1) == "discreteSurvivalRate called.") {
		return(TRUE)
	}

	return(FALSE)
}

# NOTE: Function name will change to "...ReturnsDouble" as ResearchModel grows.
survivalRateCharacterColumnDoubleTestVariable1ReturnsExpectedString <- function(testColumn, testVariable1) {
	if(survivalRate(testColumn, testVariable1) == "discreteSurvivalRate called.") {
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

survivalRateCharacterColumnDoubleTestVariable1DoubleTestVariable2ReturnsExpectedString <- function(testColumn, testVariable1, testVariable2) {
	if(survivalRate(testColumn, testVariable1, testVariable2) == "continuousSurvivalRate called.") {
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


## START: overallSurvivalRate Tests
# NOTE: Kinda pointless as it's just testing an external function's return (mean).
overallSurvivalRateReturnsDouble <- function() {
	if(typeof(overallSurvivalRate()) == "double") {
		return(TRUE)
	}

	return(FALSE)
}
## END: overallSurvivalRate Tests


## START: discreteSurvivalRate Tests
discreteSurvivalRateReturnsDouble <- function(testColumn, testVariable1) {
	if(typeof(discreteSurvivalRate(testColumn, testVariable1)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}
## END: discreteSurvivalRate Tests


## START: continuousSurvivalRate Tests
continuousSurvivalRateReturnsDouble <- function(testColumn, testVariable1, testVariable2) {
	if(typeof(continuousSurvivalRate(testColumn, testVariable1, testVariable2)) == "double") {
		return(TRUE)
	}

	return(FALSE)
}
## END: continuousSurvivalRate Tests


# Test Suite
runTests <- function() {
	runSurvivalRateTests()
}

runTests()