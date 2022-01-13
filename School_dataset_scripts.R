# import all the necessary library
library(xlsx)
library(ggplot2)
library(dplyr)  

#Import data 
school_pupils <- read.xlsx("Government_Schools_Pupils_Teachers_2017-2018.xlsx",sheetName="Sheet1")
#QUESTION 1
# get the summary of the data
summary(school_pupils) # before conversion
# question 1(a)
#Convert the variables to their suitable datatype 
school_pupils$School.stage <- as.factor(school_pupils$School.stage)
school_pupils$State <- as.factor(school_pupils$State)
school_pupils$District.Education.Office <- as.factor(school_pupils$District.Education.Office)
school_pupils$Year <- as.factor(school_pupils$Year)
school_pupils$School.type <- as.factor(school_pupils$School.type)
school_pupils$Sex <- as.factor(school_pupils$Sex)
school_pupils$Number.of.pupils <- as.integer(school_pupils$Number.of.pupils)
school_pupils$Number.of.teachers <- as.integer(school_pupils$Number.of.teachers)
summary(school_pupils) # after conversion

# question 1(b): identify variables that has missing values && state total number of missing values
# check which variables has missing values
print(any(is.na(school_pupils$School.stage))) # False --> No null values detected in this variable
print(any(is.na(school_pupils$Number.of.teachers))) # True --> Null values are detected in this variable
# get the total number of missing values
summary(school_pupils[is.na(school_pupils$Number.of.pupils), c("Number.of.teachers", "Number.of.pupils")])
# get the sum of the missing values
cat("The Sum of the missing value:",sum(is.na(school_pupils)))
#Identify the number of rows before removing the missing values.
cat("The number of rows before dropping missing value:",sum(nrow(school_pupils)))
# remove the missing  values
school_pupils <- na.omit(school_pupils)  
#Identify the number of rows after removing the missing values.
cat("The number of rows after dropping missing value:",sum(nrow(school_pupils)))
# question 1(c): rename all the variables name
names(school_pupils) <- c('school_stage', 'state' , 'district' , 'year' , 'school_type', 
                          'gender' , 'number_of_pupils' , 'number_of_teachers')

#QUESTION 2
#-----> PRIMARY SCHOOL <--------
# PIE CHART 
# store the sum of the number of pupils filtered by primary school and saved in the 
# variable that holds year 2017 and 2018 respectively. 
year_2017 <- sum(filter(school_pupils, year == 2017 & school_stage == 'Primary school' )$number_of_pupils)
year_2018 <- sum(filter(school_pupils, year == 2018 & school_stage == 'Primary school' )$number_of_pupils)

#store the sum of number of pupils filtered by year primary school in a data frame 
prim_pupils <- c(year_2017,year_2018)
prim_year <- c('2017','2018')
#Build and display in pie chart
pie(prim_pupils,prim_pupils,main="The number of primary school pupils by year 2017 & 2018",
    col=rainbow(length(prim_pupils)),cex.lab =3)
legend("topright",title="YEAR", par(cex = 1.5),prim_year,fill=rainbow(length(prim_pupils)))


#create two bar charts to show the number of primary school pupils by states in 2017 and 2018
#Bar chart 
#2017
#Filter the data set to only obtain year 2017 and school_stage to Primary school 
year2017_prim <- filter(school_pupils, year=="2017" & school_stage=="Primary school")
year2017_prim <- year2017_prim[,c("state","number_of_pupils")] #Only obtain the state and number of pupils category into the data frame 
group_state_pupil <- aggregate(number_of_pupils~., year2017_prim, FUN = sum) #sum the number of pupils based on the state
#Plot the bar graph for number of pupils in primary school in 2017 against states 
barplot(group_state_pupil$number_of_pupils, names.arg=group_state_pupil$state, xlab="State Names", ylab="Number of Pupils",
        main="The number pupils in primary schools by states in 2017", col=rainbow(length(group_state_pupil$number_of_pupils)))

#2018
#Filter the data set to only obtain year 2018 and school_stage to Primary school 
year2018_prim <- filter(school_pupils, year=="2018" & school_stage=="Primary school")
year2018_prim <- year2018_prim[,c("state","number_of_pupils")] #Only obtain the state and number of pupils category into the data frame 
group_state_pupil <- aggregate(number_of_pupils~., year2018_prim, FUN = sum) #sum the number of pupils based on the state
#Plot the bar graph for number of pupils in primary school in 2018 against states
barplot(group_state_pupil$number_of_pupils, names.arg=group_state_pupil$state, xlab="State Names", ylab="Number of Pupils",
        main="The number of primary schools by states in 2018", col=rainbow(length(group_state_pupil$number_of_pupils)))


#-----> SECONDARY SCHOOL <--------
#Pie Chart
# store the sum of the number of pupils filtered by primary school and saved in the 
# variable that holds year 2017 and 2018 respectively. 
year_2017 <- sum(filter(school_pupils, year == 2017 & school_stage == 'Secondary school' )$number_of_pupils)
year_2018 <- sum(filter(school_pupils, year == 2018 & school_stage == 'Secondary school' )$number_of_pupils)
sec_pupils<- c(year_2017,year_2018) # Save the calculated sum of the number of pupils in a data frame
sec_year <- c('2017','2018') #Set the yea 2017 and 2018 as x variable in a data frame 
#Plot the pie chart
pie(sec_pupils,sec_pupils,main="The number of Secondary school pupils by year 2017 & 2018",
    col=rainbow(length(sec_pupils)))
legend("topright",title="YEAR", par(cex = 3.0),sec_year,fill=rainbow(length(sec_pupils)))

#create two bar charts to show the number of primary school pupils by states in 2017 and 2018
#Bar Chart
#2017
#Filter the data set to only obtain year 2017 and school_stage to Secondary School 
year2017_sec <- filter(school_pupils, year=="2017" & school_stage=="Secondary school")
year2017_sec <- year2017_sec[,c("state","number_of_pupils")] # Only obtain the state and number of pupils category into the data frame 
group_state_pupil <- aggregate(number_of_pupils~., year2017_sec, FUN = sum)#sum the number of pupils based on the state
#Plot the bar graph for number of pupils in secondary school in 2017 against states
barplot(group_state_pupil$number_of_pupils, names.arg=group_state_pupil$state, xlab="State Names", ylab="Number of Pupils",
        main="The number of Pupils in Secondary schools by states in 2017", col=rainbow(length(group_state_pupil$number_of_pupils)))

#Filter the data set to only obtain year 2017 and school_stage to Secondary School 
year2018_sec <- filter(school_pupils, year=="2018" & school_stage=="Secondary school")
year2018_sec <- year2018_sec[,c("state","number_of_pupils")] #Only obtain the state and number of pupils category into the data frame 
group_state_pupil <- aggregate(number_of_pupils~., year2018_sec, FUN = sum)#sum the number of pupils based on the state
#Plot the bar graph for number of pupils in Primary school in 2018 against states
barplot(group_state_pupil$number_of_pupils, names.arg=group_state_pupil$state, xlab="State Names", ylab="Number of Pupils",
        main="The number of Pupils in Secondary schools by states in 2018", col=rainbow(length(group_state_pupil$number_of_pupils)))

#QUESTION 3
fem_pupils_states_2017 <- filter(school_pupils, year==2017 & gender=='Female')    # filter the data frame by searching for year 2017 & female
fem_pupils_states_2017 <- fem_pupils_states_2017[,c("state","number_of_pupils")]
female_state_pupil <- aggregate(number_of_pupils~., fem_pupils_states_2017, FUN = sum)

male_pupils_states_2017 <- filter(school_pupils, year==2017 & gender=='Male')     # filter the data frame by searching for year 2017 & female
male_pupils_states_2017 <- male_pupils_states_2017[,c("state","number_of_pupils")]
male_state_pupil <- aggregate(number_of_pupils~., male_pupils_states_2017, FUN = sum)

x<-rep("Male",times=16)   # repeat the word male 16 times and store into the x variable
male<-c(x) #assign the x variable into vector male
male_state_pupil$Gender <- male # add another column named Gender into the data frame of male_state_pupil

x<-rep("Female",times=16)  # repeat the word male 16 times and store into the x variable
female<-c(x)   #assign the x variable into vector male
female_state_pupil$Gender <- female # add another column named Gender into the data frame of male_state_pupil

data1 <- rbind(male_state_pupil,female_state_pupil)  #bind both male_state_pupil data frame and female_state_pupil data frame into data1

# plot out the grouped bar graph
ggplot(aes(x=state, y=number_of_pupils, group=Gender, fill=Gender), data = data1) +
  geom_bar(position='dodge',stat='identity') +scale_y_continuous(expand=c(0,0)) + theme_classic() +
  ggtitle("The number of total male and female pupils by states in 2017") + 
  xlab("States") + ylab("Number of pupils")

#2018
fem_pupils_states_2018 <- filter(school_pupils, year==2018 & gender=='Female')    # filter the data frame by searching for year 2018 & female
fem_pupils_states_2018 <- fem_pupils_states_2018[,c("state","number_of_pupils")]
female_state_pupil <- aggregate(number_of_pupils~., fem_pupils_states_2018, FUN = sum)

male_pupils_states_2018 <- filter(school_pupils, year==2018 & gender=='Male')    # filter the data frame by searching for year 2018 & male
male_pupils_states_2018 <- fem_pupils_states_2018[,c("state","number_of_pupils")]
male_state_pupil <- aggregate(number_of_pupils~., male_pupils_states_2018, FUN = sum)

x<-rep("Male",times=16)   # repeat the word male 16 times and store into the x variable
male<-c(x) #assign the x variable into vector male
male_state_pupil$Gender <- male # add another column named Gender into the data frame of male_state_pupil

x<-rep("Female",times=16)  # repeat the word male 16 times and store into the x variable
female<-c(x)   #assign the x variable into vector male
female_state_pupil$Gender <- female # add another column named Gender into the data frame of male_state_pupil

data1 <- rbind(male_state_pupil,female_state_pupil)    #bind both male_state_pupil data frame and female_state_pupil data frame into data1

# plot out the grouped bar graph
ggplot(aes(x=state, y=number_of_pupils, group=Gender, fill=Gender), data = data1) +
  geom_bar(position='dodge',stat='identity') + scale_y_continuous(expand=c(0,0)) + theme_classic() +
  ggtitle("The number of total male and female pupils by states in 2018") + 
  xlab("States") + ylab("Number of pupils")

#QUESTION 4
total_Perak17 <- sum(filter(school_pupils, year==2017 & state =='Perak')$number_of_pupils) # filter out and get the sum of pupils from Perak, 2017
total_Perak18 <- sum(filter(school_pupils, year==2018 & state =='Perak')$number_of_pupils) # filter out and get the sum of pupils from Perak, 2018
total_Perak <-c(total_Perak17,total_Perak18) # assign the values into the var
year_q4 <- c("2017","2018")
pie(total_Perak,total_Perak,main="The number of pupils in Perak",col=rainbow(length(total_Perak))) 
legend("topright",title="YEAR",year_q4,cex=0.9,fill=rainbow(length(total_Perak)))

year_2017_perak <- school_pupils[school_pupils$year == "2017" & school_pupils$state == "Perak",] # filter out and get data for 2017 and from Perak only
year_2017_perak <- year_2017_perak[, c("gender","number_of_pupils")] # get the gender and number of pupils only
group_gender_pupil17 <- aggregate(number_of_pupils~., year_2017_perak, FUN = sum) # group them by gender
year_2018_perak <- school_pupils[school_pupils$year == "2018" & school_pupils$state == "Perak",] # filter out and get data for 2017 and from Perak only
year_2018_perak <- year_2018_perak[, c("gender","number_of_pupils")] # get the gender and number of pupils only
group_gender_pupil18 <- aggregate(number_of_pupils~., year_2018_perak, FUN = sum) # group them by gender

# assign the values for the plotting
Gender <- c("Male","Female","Male", "Female")
Years <- c("2017", "2017", "2018", "2018")
num_of_pupils <- c(group_gender_pupil17$number_of_pupils[2],group_gender_pupil17$number_of_pupils[1], 
                   group_gender_pupil18$number_of_pupils[2],  group_gender_pupil18$number_of_pupils[1])
plot_data <- data.frame(Years,Gender,num_of_pupils) # the frame for plotting is created
# plot the graph
ggplot(aes(x=Years, y=num_of_pupils, group=Gender, fill=Gender), data = plot_data) +
  geom_bar(position='dodge',stat='identity') +scale_y_continuous(expand=c(0,0)) + theme_classic() +
  ggtitle("Number of pupils in Perak at year 2017 and 2018") + 
  xlab("Years") + ylab("Number of pupils")

#QUESTION 5
year_2018 <- school_pupils[school_pupils$year == "2018",] # filter and get only the year 2018
year_2018 <- year_2018[, c("district","number_of_pupils")] # year_2018 will only have 'district' and 'number of pupils' now
group_dist_pupil <- aggregate(number_of_pupils~., year_2018,FUN = sum) # sums up the number of pupils according to the districts 
group_dist_pupil <- group_dist_pupil[order(group_dist_pupil$number_of_pupils, decreasing = "TRUE"),] # sort the group_dist_pupil in descending order
ten_district <- group_dist_pupil[1:10,] # get the top 10 district
district_q5 <- c('W.P. Kuala Lumpur', 'Hulu Langat' , 'Klang' , 'Johor Bahru' , 'Gombak' , 'Pasir Gudang' , 'Petaling Perdana' , 'Kinta Utara'
                 , 'Seremban' , 'Kuala Muda')
# the bar plot
barplot(ten_district$number_of_pupils, names.arg=ten_district$district, xlab="District Names", ylab="Total Number of Pupils",
        main="The top 10 districts with highest number of pupils in 2018", col=rainbow(length(ten_district$number_of_pupils)))
# the pie chart [another rep]
pie(ten_district$number_of_pupils, ten_district$number_of_pupils, main="The top 10 districts with highest number of pupils in 2018",
    col = rainbow(length(ten_district$number_of_pupils)))
legend("topright",title="DISTRICT", district_q5 ,cex=0.7, fill = rainbow(length(ten_district$number_of_pupils)))

