# GenericReturnStats class will be parent for all ReturnStats classes.

setClass("GenericReturnStats", representation(
	overallSurvivalRate = "numeric",
	variableKeySurvivalRate = "numeric",
	populationPrecentage = "numeric",
	biasScore = "numeric",
	standardDeviation = "numeric")
)