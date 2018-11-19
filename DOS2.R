install.packages('pacman')
pacman::p_load('cluster', 'mclust', 'e1071', 'fpc', 'dplyr', 'readxl', 'sqldf', 'tidyr', 'purrr')

# a simple function to report means by group
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}

loblaw_raw_data <- readxl::read_excel('./Grocery Segmentation Raw.xlsx', sheet = 'Loblaw_Data_Collection')
str(loblaw_raw_data)
summary(loblaw_raw_data)
sum(is.na(loblaw_raw_data))

# Let's change those ugly column names...
names(loblaw_raw_data)[7:36] <- c("GrocerySurvey", "LowPrices", "Freshness", "ChoiceVariety", "Healthiness", "OrganicAlternatives", 
                                  "ConvenientStoreLayout", "StoreLocation", "ProductQuality", "ServiceQuality", "ReturnPolicy", 
                                  "Cleanliness", "Busyness", "ActivePerson", "InActivePerson", "PhysicallyFit", "EatHealthy", 
                                  "HealthyPerson", "EatPoorly", "ProportionFoodThatAreFruitsAndVegetables", "HoursOfExerciseLastWeek", 
                                  "NeighbourhoodClass", "Gender", "Age", "FamilySize", "MaritalStatus", "Occupation", 
                                  "OccupationDescription", "HighestDegree", "HouseholdAnnualIncome")


# Just in case, data.frame got messed up somewhere downstream, there is a easier way to get restarted. 
loblaw_raw_data_1 <- sqldf("select LowPrices, Freshness, ChoiceVariety, Healthiness, OrganicAlternatives, ConvenientStoreLayout, StoreLocation, ProductQuality, ServiceQuality, ReturnPolicy, Cleanliness, Busyness, 
                      ActivePerson, InActivePerson, PhysicallyFit, EatHealthy, HealthyPerson, EatPoorly, ProportionFoodThatAreFruitsAndVegetables, HoursOfExerciseLastWeek, NeighbourhoodClass, MaritalStatus, Gender, Age, FamilySize, Occupation, HighestDegree, HouseholdAnnualIncome from loblaw_raw_data")
summary(loblaw_raw_data_1)
sum(is.na(loblaw_raw_data_1)) # No NAs, and these are the variables we think that worth further exploration

loblaw_raw_data_base <- sqldf("select LowPrices, Freshness, ChoiceVariety, Healthiness, OrganicAlternatives, ConvenientStoreLayout, StoreLocation, ProductQuality, ServiceQuality, ReturnPolicy, Cleanliness, Busyness from loblaw_raw_data")
loblaw_raw_data_attributes <- sqldf("select ActivePerson, InActivePerson, PhysicallyFit, EatHealthy, HealthyPerson, EatPoorly, ProportionFoodThatAreFruitsAndVegetables, HoursOfExerciseLastWeek, NeighbourhoodClass, MaritalStatus, Gender, Age, FamilySize, Occupation, HighestDegree, HouseholdAnnualIncome from loblaw_raw_data")

### The reasoning for the level compression is two fold ###
# 1. Avoid 0 in the level transformation to keep consistent with all the other categorical variables (they all start with 1)
# 2. Only use numbers here rather than factors because we trying to do k-mean clustering

# lowerEducation = 1, higherEducation = 2
loblaw_raw_data_attributes$HighestDegree <- ifelse(loblaw_raw_data_attributes$HighestDegree == 2, 1, 2)

# student = 1, non-student = 2
loblaw_raw_data_attributes$occupationCategory <- ifelse(loblaw_raw_data_attributes$occupationCategory == 5, 1, 2)

# couple = 1, single = 2
loblaw_raw_data_attributes$maritalStatus <- ifelse(loblaw_raw_data_attributes$maritalStatus %in% c(1,2), 1, 2)

### attempted clustering part, does not make much sense to me, probably doing something wrong here ###
wss <- (nrow(loblaw_raw_data_attributes) - 1) * sum(apply(loblaw_raw_data_attributes, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(loblaw_raw_data_attributes, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "within groups sum of squares")

set.seed(20190510)
loblaw_raw_data_attributes_kmeans <- kmeans(loblaw_raw_data_attributes, centers = 9)

# inspect it
seg.summ(loblaw_raw_data_attributes, loblaw_raw_data_attributes_kmeans$cluster)

clusplot(loblaw_raw_data_attributes, loblaw_raw_data_attributes_kmeans$cluster, color = T, shade = T, labels = 9, lines = 0, main = "K-means cluster plot")
