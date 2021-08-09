# UPDATE: Create GenericSurvivalStats parent class.

# ContinuousVariableSurvivalStats class accepts a continuous variable, a lowKey, and a highKey.
# Returns a ReturnStats object.

setClass("ContinuousVariableSurvivalStats", representation(
	variable = "character",
	lowKey = "numeric",
	highKey = "numeric")
)