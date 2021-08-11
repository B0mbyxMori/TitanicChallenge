source("../Research/Generics/GenericSurvivalStats.r")

# ContinuousVariableSurvivalStats class will accept a continuous variable, high key, and low key.
# Returns a ReturnsStats object.

setClass("ContinuousVariableSurvivalStats", representation(
	lowKey = "numeric",
	highKey = "numeric"),
	contains = "GenericSurvivalStats"
)