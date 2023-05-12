#Level 1:
#Identify anomalous scores (score does not correspond to response given) and provide list of corrections.

library(readr)
library(dplyr)

#importing the data
data <- read.csv('/Users/devikasunil/Downloads/SubmittedQuestionResponses-202209061924.csv')
head(data)

summary(data)
str(data)

#filter data where question_id is 17404
filtered_data <- filter(data, question_id == 17404)
head(filtered_data)

#select the necessary columns only
necessary_columns <- select(filtered_data, response,score)
head(necessary_columns)

# Group data and calculate summaries
grouped_data <- necessary_columns %>%
  group_by(response, score) 

head(grouped_data)

#summarize the data when grouped by response and score
summarized_df <- count(grouped_data,response,score,sort = TRUE)
head(summarized_df)

#score column counted
score_column <- select(filtered_data,score) %>%
  group_by(score)
score_summary <- count(score_column,score,sort = TRUE)
head(score_summary)

#Response value when the score value is 2
response_2 <- filter(select(summarized_df,response),score ==2)
print(response_2)

#Response value when the score value is 1.333
response_1.33 <- filter(select(summarized_df,response),score == 1.33333)
print(response_1.33)

#Assumption
#There are 4 values for the original column score
#The highest value of the question_id 17404 is 2
#There is only one response value for the score 2
#Assume that the response value of the score 2 is the correct answer for the question_id -17404
#By comparing the values of the score 2 and other response then understand where is the difference
#Number of correct answer can be calculated 9-number of differences. 9 is the max number of correct answers
#score value can be calculated by number of correct answers/9*2 will give the value
#Create a new column for filtered data which contains new score

# Create a new column "new_score" using response
correct_response <-c("1=>(EMPTY)", "1=>2", "2=>(EMPTY)", "2=>5", "3=>3", "3=>1", "4=>4", "5=>(EMPTY)", "6=>6")

# Define the formula function
calc_formula <- function(response) {
  (9 - length(setdiff(strsplit(response, ", ")[[1]], correct_response))) / 9 * 2
}

# Apply the formula and create a new column
filtered_data <- filtered_data %>%
  mutate(new_score = sapply(response, calc_formula))

#Check where new_score donot match with score values
#Create a new column "ComparisonResult" by comparing each row of "score" with "new_score"
filtered_data$ComparisonResult <- ifelse(round(filtered_data$score,digits = 2) == round(filtered_data$new_score, digits = 2), "Correct", "Incorrect")

#filter where comparisonResult is Incorrect
incorrect_scores = filter(filtered_data, ComparisonResult=='Incorrect')

#incorrect_scores contains the anomalous data based on response

#Saving the anomalous data into csv file
write.csv(incorrect_scores, file = "anomalous_data.csv", row.names = FALSE)

#consolidate the list of the changes to be made
changes_df <- select(incorrect_scores,score,new_score)
head(changes_df)

changes_needed <- changes_df %>%
  group_by(score,new_score)
changes_needed_1 <- count(changes_needed,new_score,sort = TRUE)
changes_needed_1

#saving the list of score and new_score : score have to be changed to new_score
write.csv(changes_needed_1, file = "changes_needed.csv", row.names = FALSE)



