# import all the necessary library
library(tidyverse) 
library(dplyr)
library(ggplot2)
# read the csv file and import the data set
stroke <- read.csv("healthcare-dataset-stroke-data.csv")
# QUESTION 6 (A)
stroke_copy = subset(stroke, select = -c(id)) # drop the id column
str(stroke_copy)  # get the data types for each variables in the data frame
# Convert the data type into a suitable data type
stroke$gender <- as.factor(stroke$gender)
stroke$age <- as.integer(stroke$age)
stroke$hypertension <- as.factor(stroke$hypertension)
stroke$heart_disease <- as.factor(stroke$heart_disease)
stroke$ever_married <- as.factor(stroke$ever_married)
stroke$work_type <- as.factor(stroke$work_type)
stroke$Residence_type <- as.factor(stroke$Residence_type)
stroke$bmi <- as.numeric(stroke$bmi)
stroke$smoking_status <- as.factor(stroke$smoking_status)
stroke$stroke <- as.factor(stroke$stroke)
# Summary of the data set
summary(stroke)

# QUESTION 7
# create box plot for Age and Stroke
X <- c(stroke$stroke)
Y <- c(stroke$age)
ggplot(data=stroke, mapping = aes(x = X, y = Y)) + geom_boxplot() + theme_bw() +
  labs(x="Stroke",y="Age",title="Relationship Between Stroke and Age", subtitle = "")

# create box plot for BMI and Stroke
X <- c(stroke$stroke)
Y <- c(stroke$bmi)
ggplot(data=stroke, mapping = aes(x = X, y = Y)) + geom_boxplot() + theme_bw() +
  labs(x="Stroke",y="BMI",title="Relationship Between Stroke and BMI", subtitle = "")

# QUESTION 8
# get the number of people that fulfills the requirements
male_heart <- nrow(filter(stroke, heart_disease==1 & gender=="Male")) # for males with heart attack
male_hyper <- nrow(filter(stroke, hypertension==1 & gender=="Male")) # for males with hypertension
male_stroke <- nrow(filter(stroke, stroke==1 & gender=="Male"))      # for males with stroke
female_heart <- nrow(filter(stroke, heart_disease==1 & gender =="Female")) # for females with heart attack
female_hyper <- nrow(filter(stroke, hypertension==1 & gender=="Female"))  # for females with hypertension
female_stroke <- nrow(filter(stroke, stroke==1 & gender=="Female"))   # for females with stroke

# create a data frame that will be used for plotting
Genders <- c("Female","Female", "Female", "Male", "Male", "Male")
num_of_pupils <- c(female_heart,female_hyper,female_stroke, male_heart,male_hyper,male_stroke) 
Diseases <- c("Heart Attack","Hypertension", "Stroke","Heart Attack","Hypertension", "Stroke")
plot_data <- data.frame(Diseases,Genders,num_of_pupils) # data frame created

# the grouped bar chart is plotted
ggplot(aes(x=Diseases, y=num_of_pupils, group=Genders, fill=Genders), data = plot_data) +
  geom_bar(position='dodge',stat='identity') +scale_y_continuous(expand=c(0,0)) + theme_classic() +
  ggtitle("Number of patients with heart attack, hypertension and stroke according to gender") + 
  xlab("Diseases") + ylab("Number of pupils")

# get the number of males that has all 3 diseases
disease3_male <-filter(stroke, heart_disease==1 & hypertension==1 & stroke==1 & gender=="Male")
male_freq <- nrow(disease3_male) # count the number of males 
# get the number of males that has all 3 diseases
disease3_female <- filter(stroke, heart_disease==1 & hypertension==1 & stroke==1 & gender=="Female")
female_freq <- nrow(disease3_female) # count the number of males
Genders <- c("Male", "Female")
mf_freq <- c(male_freq,female_freq)
# plot out the bar graph for it 
barplot(mf_freq, names.arg=Genders, xlab="Gender", ylab="Number of patients",
        main="Number of patients with heart disease, 
  hypertension and stroke against gender", col=rainbow(length(mf_freq)))

# QUESTION 9
# to assign gender and work_type in datas in table form
data1 = subset(stroke, select = -c(id,age,hypertension,heart_disease,ever_married,Residence_type,bmi,smoking_status,stroke,avg_glucose_level) )
data <- data1[-c(3117),] # to remove the data that contains in the row 3117 which contains gender type other
work_table = table(data$gender,data$work_type)  
work_table <- work_table [-c(3),]     # to drop the column that has the work_type other
final_table = prop.table(work_table,1) # assign proportional value for each work_type that is stored in the table.
# plot the graph
barplot(final_table,beside = TRUE, main="Graph of work type against gender", ylab = "Proportion of gender",col=c("blue","red"), legend=rownames(final_table),ylim=c(0,0.8))

# QUESTION 10
#Store the average glucose level data in y variable 
#store the age data on x variable
y <- c(stroke$avg_glucose_level)
x <- c(stroke$age)

#Plot a scatter plot with a smoothing curve
ggplot(stroke_copy , aes(x=age,y=avg_glucose_level)) + geom_point(color="steelblue")+ geom_smooth()+
  labs(x = "Age", y = "average glucose level", title = "Relationship between average glucose level and age", subtitle = "")

