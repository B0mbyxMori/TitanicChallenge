source("Research/Generics/GenericSurvivalStats.r")

# DiscreteVariableSurvivalStats class accepts a discrete variable and key; returns a ReturnStats object.

# ADD: Object validation method - will need to verify string variable and string key.
# NOTE: Is error throwing/catching available in R?

setClass("DiscreteVariableSurvivalStats", representation(
	key = "character"),
	contains = "GenericSurvivalStats"
)

# NOTE: Find right R data structure
# Make list of discrete variables
# Make list of keys