trainingSet <- read.csv("../Datasets/train.csv")

# Assemble:

# survivalRate - Reports survival rate
	# No arguments - returns survival rate of entire dataset
	# Column, character (for discrete data) - returns survival rate of character.
		# For example, survivalRate(Sex, female) would return the survival rate of
		# all females in column.
	# Columnm, low_numeric, high_numeric (for continuous data) - returns survival rate
		# for passengers that fall within the low_numeric and high_numeric range. For example,
		# survivalRate(Fare, 2, 50) will return the survival rate of all passengers
		# that paid fare between and including $2 and $50 dollars.


# populationPrecentage - Reports the precentage of the population
	# Column, character - returns the population precentage of character. For example,
		# survivalRate(Sex, female) returns the precentage of females that were on-board.
	# Columnm, low_numeric, high_numeric - returns the population precentage of
		# passengers that fall within the given range. survivalRate(Fare, 2, 50) will
		# return the population of passagers that paid fair from $2 to $50.

# confidenceScore - Reports a precentage that guesses how bias the data point may be. The
# higher the number, the more likely the data point may be bias.

# survivalRateDifference - Reports the difference of the specific survival rate from 
# overall survival rate.

survivalRate <- function(column = NULL, variable1 = NULL, variable2 = NULL) {
	if(missing(column) && missing(variable1) && missing(variable2)) {
		return(overallSurvivalRate())
	}

	if(missing(variable2)
		&& typeof(column) == "character"
		&& (typeof(variable1) == "character" || typeof(variable1) == "double")) {
		return("discreteSurvivalRate called.")
	}

	if(typeof(column) == "character"
		&& (typeof(variable1) == "character" || typeof(variable1) == "double")
		&& (typeof(variable2) == "character" || typeof(variable2) == "double")) {
		return("continuousSurvivalRate called.")
	}

	return("Bad arugment(s).")
}

overallSurvivalRate <- function() {
	mean(trainingSet$Survived) * 100
}

# typeof(overallSurvivalRate)

# survivalRate()
# survivalRate("something", 2)
# survivalRate("something", "something", "something")
# survivalRate("something", 2, 1)
# survivalRate("something", 2L, 1L)

# summary(trainingSet)

# 1. PassengerId
# 2. Survived
# 3. Pclass
# 4. Name
# 5. Sex
# 6. Age
# 7. SibSp
# 8. Parch
# 9. Ticket
# 10. Fare
# 11. Cabin
# 12. Embarked