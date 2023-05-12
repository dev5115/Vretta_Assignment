### Question to answer
#Identify records which are discrepant in the alignment of `is_nr=1` 
#and `none(response_raw[entryId].isResponded=true)`. 
#Use detected pattern to suggest a possible cause.

install.packages('devtools')
devtools::install_github("bergant/jsondiff")
library(jsonlite)
library(readr)
library(dplyr)
library(jsondiff)

#importing the data
data <- read.csv('/Users/devikasunil/Downloads/test_attempt_question_responses-202209061938.csv')
head(data)

# Extract isResponded value from response_raw column 
#and save to a new column json_isResponded
data$json_isResponded <- sapply(data$response_raw, function(x) {
  json_data <- fromJSON(x)
  # Extract the desired value from the JSON data
  value <- json_data$"3"$isResponded
  return(value)
})

# Create a new column based on two conditions: is_nr == 1 and json_isResponded is False
data$both_conditions <- ifelse(data$is_nr ==1 & data$json_isResponded == FALSE, TRUE, FALSE)

#data both conditions are met
conditions_satisfied <- filter(data,both_conditions == TRUE)
head(conditions_satisfied)

#data where conditions are not met
conditions_notsatisfied <- filter(data, both_conditions == FALSE)

# JSON data strings
json1 <- conditions_satisfied[1,3]
json2 <- conditions_notsatisfied[1,3]

# Convert JSON strings to R objects
data1 <- fromJSON(json1)
data2 <- fromJSON(json2)

# Compare JSON objects
diff <- jsondiff(data1, data2)

# Print the differences
print(diff)

#find the difference for the second row of the satisfied and notsatisfied json files
json3 <- conditions_satisfied[2,3]
json4 <- conditions_notsatisfied[2,3]

data3 <- fromJSON(json3)
data4 <- fromJSON(json4)

diff2 <- jsondiff(data3,data4)

print(diff2)

#find the difference for the third row of the satisfied and notsatisfied json files
json5 <- conditions_satisfied[3,3]
json6 <- conditions_notsatisfied[3,3]

data5 <- fromJSON(json5)
data6 <- fromJSON(json6)

diff3 <- jsondiff(data5,data6)

print(diff3)

#When I checked the difference between response row of satisfied and notsatisfied json files
#Under root "3" 'isFilled' value changes to True from False 
#Under root "3" 'isKeyIdUsed' value is added False
#under root "3" 'isCorrect' value changes to True
#under root "3" 'isResponded' value changes to True
#under root "3" 'isStarted' value changes to True
#Simplifiedstatetargets are added which have id:'Answer'
#My finding is that when both conditions (is_nr == 1 and isResponded is False)
#is not satisfied
#means the assessment is filled and the student respond to the assessment


#Saving the discrepant data in the alignment of `is_nr=1` 
#and `none(response_raw[entryId].isResponded=true)` into csv file
write.csv(conditions_notsatisfied, file = "discrepant_data.csv", row.names = FALSE)