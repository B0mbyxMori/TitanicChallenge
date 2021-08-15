# GenericReturnStats class will be parent for all ReturnStats classes.

# Responsibilities:
# Object will be used as a container for stats information.
# Will contain methods to clean and pretty print object's data.

setClass("GenericReturnStats", representation(
	overallSurvivalRate = "numeric",
	variableSurvivalRate = "numeric",
	variableKeySurvivalRate = "numeric",
	populationPrecentage = "numeric",
	biasScore = "numeric",
	standardDeviation = "numeric")
)