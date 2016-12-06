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


# CORRELATION TABLE & CHECK

my.data <- read_csv("final_exam_analytic_data.csv")

analytic_data_no_gender <- my.data %>% select(-gender)

apa.cor.table(analytic_data_no_gender, filename = "Table1.doc", table.number = 1)

psych::pairs.panels(as.data.frame(analytic_data_no_gender))


# REGRESSION - total 

block1 = lm(performance~conscientiousness, data = my.data)
block2 = lm(performance~conscientiousness + agreeableness, data = my.data)
apa.reg.table(block1, block2, filename = "Table2.doc", table.number = 2)

# REGRESSION - men 

my.data.men <- filter(my.data, gender=="Male")

block3 = lm(performance~conscientiousness, data = my.data.men)
block4 = lm(performance~conscientiousness + agreeableness, data = my.data.men)
apa.reg.table(block3, block4, filename = "Table3.doc", table.number = 3)

# REGRESSION - women 

my.data.women <- filter(my.data, gender=="Female")

block5 = lm(performance~conscientiousness, data = my.data.women)
block6 = lm(performance~conscientiousness + agreeableness, data = my.data.women)
apa.reg.table(block5, block6, filename = "Table4.doc", table.number = 4)
