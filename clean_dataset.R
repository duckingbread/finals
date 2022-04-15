library(dplyr)
library(tidyverse)


unique(df$Gender)
df <- read.csv("./finals/survey.csv", header = TRUE, sep = ",")

summary(df)


#removing outliers
df$Age <- as.integer(df$Age)
df <- df %>% filter(Age > 17, Age < 70)


##@checking for missing values
percentmissing = function (x){ sum(is.na(x))/length(x) * 100}
missing = apply(df, 1, percentmissing)
table(missing)


#remove comment field
df <- df %>% select(-c(comments, state))

#removing rows with less than 5%missing values
replace = subset(df, missing <= 5)

apply(replace, 2, percentmissing)


#replacing other null values using mice library
library(mice)
tempnomiss = mice(replace)
df = complete(tempnomiss, 1)
apply(df, 2, percentmissing)


#replacing values
df$self_employed[is.na(df$self_employed)] <- 'No'

#correctin country names
df$Country = gsub('united states.*', 'united states of america', df$Country)
df$Country = replace_na(df$Country, 'united states of america')

## correting gender
unique(df$Gender)
Male <- c("Male ","male", "M", "Mail", "Cis Male", "Mail","Malr", "Male-ish", "Male ", "Make", "Male", "msle")
Female <- c("Female ","Female","woman","Female","Female (cis)", "Cis Female", "female","F", "Female (trans)")
Queer <- c("non-binary","fluid","Nah", "Genderqueer","Androgyne")
df$Gender <- as.factor(ifelse(df$Gender %in% Male,"Male",ifelse(df$Gender %in% Female,"Female","Non-Binary")))

#Using the new vectors we make the proper distribution of gender
MH_data$Gender <- as.factor(ifelse(MH_data$Gender %in% Male,"male",ifelse(MH_data$Gender %in% Female,"female","queer")))

unique(df$no_employees)

#factor no of employees
unique(df$no_employees)
df$no_employees = factor(df$no_employees, levels = c('1-5', '6-25', '26-100', '100-500', '500-1000', 'more than 1000'))
df%>% 
  group_by(no_employees) %>%
  summarise(no_rows = length(no_employees))
df$no_employees = replace_na(df$no_employees, '26-100')


df$work_interfere = factor(df$work_interfere, levels = c('Never', 'Rarely', 'Sometimes', 'Often'))
df%>% 
  group_by(work_interfere ) %>%
  summarise(no_rows = length(work_interfere ))
df$work_interfere = replace_na(df$work_interfere, 'Sometimes')


write.csv(df, "survey_cleaned.csv")

