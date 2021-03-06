install.packages('pacman')
pacman::p_load('cluster', 'mclust', 'e1071', 'fpc', 'dplyr', 'readxl', 'sqldf', 'tidyr', 'purrr', 'ggplot2')

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
loblaw_raw_data_descriptors <- sqldf("select ActivePerson, InActivePerson, PhysicallyFit, EatHealthy, HealthyPerson, EatPoorly, ProportionFoodThatAreFruitsAndVegetables, HoursOfExerciseLastWeek, NeighbourhoodClass, MaritalStatus, Gender, Age, FamilySize, Occupation, HighestDegree, HouseholdAnnualIncome from loblaw_raw_data")

### The reasoning for the level compression is two fold ###
# 1. Avoid 0 in the level transformation to keep consistent with all the other categorical variables (they all start with 1)
# 2. Only use numbers here rather than factors because we trying to do k-mean clustering

# lowerEducation = 1, higherEducation = 2
loblaw_raw_data_descriptors$HighestDegree <- ifelse(loblaw_raw_data_descriptors$HighestDegree == 2, 1, 2)

# student = 1, non-student = 2
loblaw_raw_data_descriptors$Occupation <- ifelse(loblaw_raw_data_descriptors$Occupation == 5, 1, 2)

# couple = 1, single = 2
loblaw_raw_data_descriptors$MaritalStatus <- ifelse(loblaw_raw_data_descriptors$MaritalStatus %in% c(1,2), 1, 2)

#----------- A different way to run k-means & Sihouette Analysis -------

#---- Try Justin's features ----
loblaw_raw_data_base$StoreValue <- (loblaw_raw_data_base$LowPrices + 
                                            loblaw_raw_data_base$Freshness + 
                                            loblaw_raw_data_base$ChoiceVariety + 
                                            loblaw_raw_data_base$StoreLocation + 
                                            loblaw_raw_data_base$ProductQuality + 
                                            loblaw_raw_data_base$ReturnPolicy) / 6

loblaw_raw_data_base$StoreComfort <- (loblaw_raw_data_base$ServiceQuality + 
                                        loblaw_raw_data_base$Cleanliness + 
                                        loblaw_raw_data_base$Busyness + 
                                        loblaw_raw_data_base$ConvenientStoreLayout) / 4 

loblaw_raw_data_base$StoreHealth <- (loblaw_raw_data_base$Healthiness + 
                                       loblaw_raw_data_base$OrganicAlternatives) / 2

model_ready_df <- subset(loblaw_raw_data_base, select = c(StoreValue, StoreComfort, StoreHealth))

tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(model_ready_df, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) + geom_line() + scale_x_continuous(breaks = 1:10)

sil_width <- map_dbl(2:10, function(k){
  model <- pam(x = model_ready_df, k = k)
  model$silinfo$avg.width
})

sil_df <- data.frame(
  k = 2:10, 
  sil_width = sil_width
)

ggplot(sil_df, aes(x = k, y = sil_width)) + geom_line() + scale_x_continuous(breaks = 2:10)

# it seems k = 2 is the best according to Sihouette Analysis
loblaw_raw_data_base_model <- kmeans(model_ready_df, centers = 2)

# inspect it 
seg.summ(model_ready_df, loblaw_raw_data_base_model$cluster)
seg.summ(loblaw_raw_data_descriptors, loblaw_raw_data_base_model$cluster)
clusplot(model_ready_df, loblaw_raw_data_base_model$cluster, color = T, shade = T, labels = 2, lines = 0, main = "K-means cluser plot")
