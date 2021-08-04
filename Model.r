# Reads training dataset to terminal; verifying dataset is accessible.
read.csv("Datasets/train.csv")

# Goal:
# Guess who survived; build a predictive model of who survived based on data given.

# Data available:
# PassengerId
# Survived (0, 1)
# Pclass (1, 2, 3) - ticket class
# Name (string)
# Sex (M, F)
# Age (natural number, unless child under 1 or an estimated age, displayed as xx.5)
# SibSp - number of siblings aboard
# Parch - number of parents and/or children aboard, NOTE: Some children only boarded w/ nanny resulting in a 0
# Ticket (ticket number)
# Fare (dollar amount) - likely related to ticket class
# Cabin - NOTE: missing cabins likely indicate lower class passengers (looks like 2 and 3rd class based on spot check)
# Embarked (C, Q, S)