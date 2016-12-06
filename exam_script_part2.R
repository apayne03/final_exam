library(tidyverse)
library(apaTables)

raw_data <- read_csv(file = "exam_data_f16.csv")

raw_data <- read_csv(file = "exam_data_f16.csv", na=c("", "NA", "-999", "-888"))

# CATEGORICAL VARIABLES TO FACTORS
categorical_variables <- select(raw_data, gender)
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1, "Female"=2)

# HANDLING ITEM DATA

agreeableness <- select(raw_data, A1, A2, A3, A4, A5)
conscientiousness <- select(raw_data, C1, C2, C3, C4, C5)
performance <- select(raw_data, JP1, JP2, JP3, JP4, JP5)
education <- select(raw_data, education)
age <- select(raw_data, age)

psych::describe(agreeableness)
is_bad_value <- agreeableness<1 | agreeableness>6
agreeableness[is_bad_value] <- NA
psych::describe(agreeableness)

psych::describe(conscientiousness)

psych::describe(performance)

agreeableness <- mutate(agreeableness, A1=7-A1)
conscientiousness <- mutate(conscientiousness, C4=7-C4)
conscientiousness <- mutate(conscientiousness, C5=7-C5)
performance <- mutate(performance, JP1=7-JP1)
performance <- mutate(performance, JP2=7-JP2)


agreeableness <- psych::alpha(as.data.frame(agreeableness), check.keys = FALSE)$scores
conscientiousness <- psych::alpha(as.data.frame(conscientiousness), check.keys = FALSE)$scores
performance <- psych::alpha(as.data.frame(performance), check.keys = FALSE)$scores

analytic_data <- cbind(categorical_variables, age, agreeableness, conscientiousness, performance)

write_csv(analytic_data, path = "final_exam_analytic_data.csv")


# START

my.data <- read_csv("final_exam_analytic_data.csv")

apa.cor.table(my.data, filename = "Table1.doc", table.number = 1)

psych::pairs.panels(as.data.frame(my.data))
