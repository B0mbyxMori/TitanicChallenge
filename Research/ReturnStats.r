# ReturnStats class will be produced by a GenericSurvivalStats child class.

setClass("ReturnStats", representation(
	overallSurvivalRate = "numeric",
	variableKeySurvivalRate = "numeric",
	populationPrecentage = "numeric",
	biasScore = "numeric",
	standardDeviation = "numeric")
)