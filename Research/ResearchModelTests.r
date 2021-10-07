# trainingSet <- read.csv("../Datasets/train.csv")
researchModel <- source("ResearchModel.r")

# NOTE: Remove repeated code and review code.

runSurvivalRateTests <- function() {
	print(survivalRateNoArgumentsReturnsDouble())
	print(survivalRateCharacterColumnCharacterTestVariable1ReturnsExpectedString(character(), character()))
	print(survivalRateCharacterColumnDoubleTestVariable1ReturnsExpectedString(character(), double()))
	print(survivalRateCharacterColumnNotCharacterOrDoubleTestVariable1ReturnsExpectedString(character(), (!double() | !character())))
	print(survivalRateNotCharacterColumnDoubleTestVariable1ReturnsExpectedString(!character(), double()))
	print(survivalRateCharacterColumnDoubleTestVariable1DoubleTestVariable2ReturnsExpectedString(character(), double(), double()))
	print(survivalRateCharacterColumnNotDoubleTestVariable1DoubleTestVariable2ReturnsExpectedString(character(), !double(), double()))
	print(survivalRateCharacterColumnDoubleTestVariable1NotDoubleTestVariable2ReturnsExpectedString(character(), double(), !double()))
	print(survivalRateNotCharacterColumnDoubleTestVariable1DoubleTestVariable2ReturnsExpectedString(!character(), double(), double()))

}

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

# NOTE: Function name will change to "...ThrowsError" as ResearchModel grows.
survivalRateCharacterColumnNotCharacterOrDoubleTestVariable1ReturnsExpectedString <- function(testColumn, testVariable1) {
	if(survivalRate(testColumn, testVariable1) == "Bad argument(s).") {
		return(TRUE)
	}

	return(FALSE)
}

# NOTE: Function name will change to "...ThrowsError" as ResearchModel grows.
survivalRateNotCharacterColumnDoubleTestVariable1ReturnsExpectedString <- function(testColumn, testVariable1) {
	if(survivalRate(testColumn, testVariable1) == "Bad argument(s).") {
		return(TRUE)
	}

	return(FALSE)
}

# NOTE: Function name will change to "...ReturnsDouble" as ResearchModel grows.
survivalRateCharacterColumnDoubleTestVariable1DoubleTestVariable2ReturnsExpectedString <- function(testColumn, testVariable1, testVariable2) {
	if(survivalRate(testColumn, testVariable1, testVariable2) == "continuousSurvivalRate called.") {
		return(TRUE)
	}

	return(FALSE)
}

# NOTE: Function name will change to "...ThrowsError" as ResearchModel grows.
survivalRateCharacterColumnNotDoubleTestVariable1DoubleTestVariable2ReturnsExpectedString <- function(testColumn, testVariable1, testVariable2) {
	if(survivalRate(testColumn, testVariable1, testVariable2) == "Bad argument(s).") {
		return(TRUE)
	}

	return(FALSE)
}

# NOTE: Function name will change to "...ThrowsError" as ResearchModel grows.
survivalRateCharacterColumnDoubleTestVariable1NotDoubleTestVariable2ReturnsExpectedString <- function(testColumn, testVariable1, testVariable2) {
	if(survivalRate(testColumn, testVariable1, testVariable2) == "Bad argument(s).") {
		return(TRUE)
	}

	return(FALSE)
}

# NOTE: Function name will change to "...ThrowsError" as ResearchModel grows.
survivalRateNotCharacterColumnDoubleTestVariable1DoubleTestVariable2ReturnsExpectedString <- function(testColumn, testVariable1, testVariable2) {
	if(survivalRate(testColumn, testVariable1, testVariable2) == "Bad argument(s).") {
		return(TRUE)
	}

	return(FALSE)
}

# Test Suite
runTests <- function() {
	runSurvivalRateTests()
}

runTests()