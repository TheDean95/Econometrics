#Jeffrey Dean
#Econometrics 322 Project 1

#1
# Find age given birth year and 2013 present day

library(lubridate)

text_score_table <- read.table("text_score_econometrics_project.txt", sep="\t", header=TRUE)

text_score_table

birth_years <- text_score_econometrics_project$birth_year

birth_years

age <- 2013 - birth_years

age

age_sq <-(age)^2

age_sq

#2
#OLS regression on math scores given studying time, cognitive ability, age, and square of age.

OLS_regression_math_scores <- lm(text_score_econometrics_project$math_score ~ text_score_econometrics_project$studying_time + 
                                   text_score_econometrics_project$cognitive_ability + age + age_sq, data =  text_score_econometrics_project)

summary(OLS_regression_math_scores)
confint(OLS_regression_math_scores, level = 0.95)

#3
#dummy variablesfor grade 9 students, high income student, and education level of mother and father 

grade9 <- as.numeric(text_score_econometrics_project$grade == 9)
grade9

high_income <- as.numeric(text_score_econometrics_project$income_status > 3)
high_income

mother_higher_edu <- as.numeric(text_score_econometrics_project$mother_edu >= 7)
mother_higher_edu

father_higher_edu <- as.numeric(text_score_econometrics_project$father_edu >= 7)
father_higher_edu

#4 
#proportion of students in grade 9, proportion of students that are female, proportion of students older than 13
#proportion of students from a high income family 

all_students_grade <- table(text_score_econometrics_project$grade)
students_grade_7_9 <- all_students_grade[names(all_students_grade)==7]+all_students_grade[names(all_students_grade)
                                                                                          ==9]
students_grade_9 <- all_students_grade[names(all_students_grade)==9]

all_students_grade
students_grade_9
students_grade_7_9

prop_grade_9 <- students_grade_9/students_grade_7_9
prop_grade_9

library(MASS)

prop_grade_9_frac <- fractions(prop_grade_9, cycles = 10, max.denominator = 17723)
prop_grade_9_frac

all_students_fem <- table(text_score_econometrics_project$female)
students_gender_fem_0_1 <- all_students_fem[names(all_students_fem)==1]+all_students_fem[names(all_students_fem)==0]
students_gender_fem_1 <- all_students_fem[names(all_students_fem)==1]

all_students_fem
students_gender_fem_0_1
students_gender_fem_1

prop_gender_fem <- students_gender_fem_1/students_gender_fem_0_1
prop_gender_fem

prop_gender_fem_frac <- fractions(prop_gender_fem, cycles = 10, max.denominator = 17723)
prop_gender_fem_frac

all_students_age <- table(age)
all_students_age

student_age_11_12 <- all_students_age[names(all_students_age)==11]+all_students_age[names(all_students_age)==12]
student_age_13_17 <- all_students_age[names(all_students_age)==13]+all_students_age[names(all_students_age)==14]+
all_students_age[names(all_students_age)==15]+all_students_age[names(all_students_age)==16]+
all_students_age[names(all_students_age)==17]
student_age_11_12
student_age_13_17
student_age_11_17 <-student_age_11_12 + student_age_13_17
student_age_11_17
student_age_14_17 <-all_students_age[names(all_students_age)==14]+
all_students_age[names(all_students_age)==15]+all_students_age[names(all_students_age)==16]+
all_students_age[names(all_students_age)==17]
student_age_14_17

prop_student_age_14_17 <- student_age_14_17/student_age_11_17
prop_student_age_14_17

prop_student_age_14_17_frac <- fractions(prop_student_age_14_17, cycles = 10, max.denominator = 17723)
prop_student_age_14_17_frac

all_students_income_status <- table(text_score_econometrics_project$income_status)
all_students_income_status

student_income_all_1_2 <-all_students_income_status[names(all_students_income_status)==1]+all_students_income_status[names(all_students_income_status)==2]
student_income_all_3_4 <-all_students_income_status[names(all_students_income_status)==3]+all_students_income_status[names(all_students_income_status)==4] 
student_income_all_5 <-all_students_income_status[names(all_students_income_status)==5]
student_income_all_1_2
student_income_all_3_4
student_income_all_5

student_income_all <- student_income_all_1_2 + student_income_all_3_4 + student_income_all_5
student_income_all

student_income_all_4 <- all_students_income_status[names(all_students_income_status)==4] 
student_income_all_4_5 <- student_income_all_4 + student_income_all_5
student_income_all_4_5

prop_student_income_4_5 <- student_income_all_4_5/student_income_all
prop_student_income_4_5

prop_student_income_4_5_frac <- fractions(prop_student_income_4_5, cycles = 10, max.denominator = 17723)
prop_student_income_4_5_frac

#5
#Build OLS regression using Q2, gender variables, and dummy variables generated.  Use robust standard error
#for the regression. 

OLS_regression_math_scores_2 <- lm(text_score_econometrics_project$math_score ~ text_score_econometrics_project$studying_time + 
                                     text_score_econometrics_project$cognitive_ability + age + age_sq+text_score_econometrics_project$female +
                                   text_score_econometrics_project$male + grade9 + high_income + mother_higher_edu + father_higher_edu, data =  text_score_econometrics_project)
library(lmtest)
library(sandwich)
standard_error_OLS_math_scores <- coeftest(OLS_regression_math_scores_2, vcov = vcovHC(OLS_regression_math_scores_2, type="HC1"))

OLS_regression_math_scores_2
standard_error_OLS_math_scores

confint(OLS_regression_math_scores_2)

#6
#Develop two OLS regressions with studying	time,	cognitive	ability, age	and	the	square	of	
#age as	explanatory	variables on English scores for females and for males.

age
age_sq

female_age <- subset(text_score_econometrics_project, female==1, select=c(birth_years-2013))
female_age

female_age_sq <- (female_age)^2
female_age_sq

english_score_f <- subset(text_score_econometrics_project, female == 1, select=c(english_score))
english_score_f

studying_time_f <- subset(text_score_econometrics_project, female == 1, select=c(studying_time))
english_score_f

cognitive_ability_f <- subset(text_score_econometrics_project, female == 1, select=c(cognitive_ability))
cognitive_ability_f

numeric_english_score_f <- as.numeric(unlist(english_score_f))
numeric_english_score_f

numeric_studying_time_f <- as.numeric(unlist(studying_time_f))
numeric_studying_time_f

numeric_cognitive_ability_f<- as.numeric(unlist(cognitive_ability_f))
numeric_cognitive_ability_f

numeric_female_age_f<- as.numeric(unlist(female_age))
numeric_female_age_f

numeric_female_age_sq_f<- as.numeric(unlist(female_age_sq))
numeric_female_age_sq_f

regression_1_f <- lm(numeric_english_score_f ~ numeric_studying_time_f + numeric_cognitive_ability_f, 
                     data=text_score_econometrics_project)
regression_1_f

t_test_regression_1_f <- t.test(regression_1_f)

summary(regression_1_f)

confint(regression_1_f, level = 0.95)

male_age <- subset(text_score_econometrics_project, male==1, select=c(birth_years-2013))
male_age

male_age_sq <- (male_age)^2
male_age_sq

english_score_m <- subset(text_score_econometrics_project, male == 1, select=c(english_score))
english_score_m

studying_time_m <- subset(text_score_econometrics_project, male == 1, select=c(studying_time))
english_score_m

cognitive_ability_m <- subset(text_score_econometrics_project, male == 1, select=c(cognitive_ability))
cognitive_ability_m

numeric_english_score_m <- as.numeric(unlist(english_score_m))
numeric_english_score_m

numeric_studying_time_m <- as.numeric(unlist(studying_time_m))
numeric_studying_time_m

numeric_cognitive_ability_m<- as.numeric(unlist(cognitive_ability_m))
numeric_cognitive_ability_m

numeric_female_age_m<- as.numeric(unlist(male_age))
numeric_female_age_m

numeric_female_age_sq_m<- as.numeric(unlist(male_age_sq))
numeric_female_age_sq_m

regression_1_m <- lm(numeric_english_score_m ~ numeric_studying_time_m + numeric_cognitive_ability_m, 
                     data=text_score_econometrics_project)
regression_1_m

t_test_regression_1_m <- t.test(regression_1_m)

summary(regression_1_m)

confint(regression_1_m, level = 0.95)

#7
#Adding # of siblings and dummy variables, find influences on male vs female

sibling_number_f <- subset(text_score_econometrics_project, female == 1, select=c(sibling_number))
sibling_number_f

numeric_sibling_number_f <- as.numeric(unlist(sibling_number_f))
numeric_sibling_number_f

grade9_f <- subset(text_score_econometrics_project, female == 1 & grade == 9, select=c(grade9))
grade9_f

numeric_grade9_f <- as.numeric(unlist(grade9_f))
numeric_grade9_f

high_income_f <- subset(text_score_econometrics_project, female == 1 & income_status > 3)
high_income_f

numeric_high_income_f <- as.numeric(unlist(high_income_f))
numeric_high_income_f

mother_higher_edu_f <- subset(text_score_econometrics_project, female == 1 & mother_edu >= 7)
mother_higher_edu_f 

numeric_mother_higher_edu_f <- as.numeric(unlist(mother_higher_edu_f))
numeric_mother_higher_edu_f

father_higher_edu_f <- subset(text_score_econometrics_project, female == 1 & father_edu >= 7)
father_higher_edu_f 

numeric_father_higher_edu_f <- as.numeric(unlist(father_higher_edu_f))
numeric_father_higher_edu_f

female_group <- subset(text_score_econometrics_project, female ==1)
female_group

regression_2_f <- lm(female_group$english_score ~ female_group$cognitive_ability + female_group$studying_time + 
                       female_group$sibling_number, data=text_score_econometrics_project)
regression_2_f


t_test_regression_2_f <- t.test(regression_2_f)

summary(regression_2_f)

library(lmtest)
library(sandwich)

coeftest(regression_2_f, vcov = vcovHC(regression_2_f, type="HC1"))

confint(regression_2_f, level = 0.95)

male_group <- subset(text_score_econometrics_project, male ==1)
male_group

regression_2_m <- lm(male_group$english_score ~ male_group$cognitive_ability + male_group$studying_time + 
                       male_group$sibling_number, data=text_score_econometrics_project)
regression_2_m

t_test_regression_2_m <- t.test(regression_2_m)

summary(regression_2_m)

library(lmtest)
library(sandwich)

coeftest(regression_2_m, vcov = vcovHC(regression_2_m, type="HC1"))

confint(regression_2_m, level = 0.95)

#8
#Find correlation between cognitive ability and math scores.  Find correlation between cognitive ability 
#and english scores.

math_score_corr <- cor(text_score_econometrics_project$cognitive_ability, text_score_econometrics_project$math_score)
english_score_corr <- cor(text_score_econometrics_project$cognitive_ability, text_score_econometrics_project$english_score)

math_score_corr
english_score_corr

#9
# Find if the mean of math scores differ between male and female students.

math_score_female <- subset(text_score_econometrics_project, female ==1,
                  select=c(math_score))

math_score_male <- subset(text_score_econometrics_project, male ==1,
                         select=c(math_score))

math_score_male
math_score_female

numeric_math_score_male <- as.numeric(unlist(math_score_male))
numeric_math_score_female <- as.numeric(unlist(math_score_female))

numeric_math_score_female
numeric_math_score_male

avg_male_score_math <- mean(numeric_math_score_male)
avg_female_score_math <-mean(numeric_math_score_female)

avg_female_score_math
avg_male_score_math

diff_of_means_gender_math <- t.test(numeric_math_score_female, numeric_math_score_male, var.equal = TRUE)
diff_of_means_gender_math

#10
# Find if the mean of english scores differ between male and female students. 

english_score_female <- subset(text_score_econometrics_project, female ==1,
                            select=c(english_score))

english_score_male <- subset(text_score_econometrics_project, male ==1,
                          select=c(english_score))

english_score_male
english_score_female

numeric_english_score_male <- as.numeric(unlist(english_score_male))
numeric_english_score_female <- as.numeric(unlist(english_score_female))

numeric_english_score_female
numeric_english_score_male

avg_male_score_english <- mean(numeric_english_score_male)
avg_female_score_english <-mean(numeric_english_score_female)

avg_male_score_english
avg_female_score_english

diff_of_means_gender_english <- t.test(numeric_english_score_female, numeric_english_score_male, var.equal = TRUE)
diff_of_means_gender_english
