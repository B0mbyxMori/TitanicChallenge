# DiscreteVariableSurvivalRate class accepts a discrete variable and key, and returns survival rate.

# ADD: Object validation method - will need to verify string variable and string key.
# NOTE: Is error throwing/catching available in R?

setClass("DiscreteVariableSurvivalRate", representation(variable = "character", key = "character", rate = "numeric"))

# NOTE: Find right R data structure
# Make list of discrete variables
# Make list of keys