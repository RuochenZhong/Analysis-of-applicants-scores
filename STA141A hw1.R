#Ruochen Zhong 912888970

hw1data = readRDS("/Users/apple/Desktop/college_scorecard_2013.rds")

####question 1####
#to check the number of rows and columns of the dataset, and find the number of main campus
dim(hw1data)
summary(hw1data$main_campus)

####question 2####
#to see every variable's catagories 
str(hw1data)
#to caculate the total numbers of every catagory
table(sapply(hw1data, class))

####question 3####
#find total NA numbers
sum(is.na(hw1data))
#check each column's NA numbers
colSums(is.na(hw1data))
#find the column which has largest NA numbers
which.max(colSums(is.na(hw1data)))

#try to find patterns
#get a table of two catagorical variables
pattern1 <- table(is.na(hw1data$avg_sat),hw1data$open_admissions)
#draw a barplot to show the relationship and also add legend to make it clearly
barplot(pattern1, col = c("blue","red"), main = "NAs of avg sat vs. open admission", 
        ylab = "NAs and not NAs of avg sat", xlab = "Open admission")
legend("topright",legend=c("not NAs", "NAs"), fill = c("blue", "red"))

####question 4####
#check each catagory's number
summary(hw1data$ownership)

#create a new varaible who only contain "Public" and "Private" types of ownerships
hw1data$IsPublic<-factor(hw1data$ownership=="Public",labels=c("Private","Public"))

#create a new subset which only contain two variables
highest_degree<-hw1data[c("highest_degree", "IsPublic")]
#make a table of the constitution of the highest degree and transform it to proportion form
degree_table<-table(highest_degree) 
prop.table(degree_table,margin = 2) 
#draw a mosaic plot of that table
mosaicplot(degree_table, las=2, 
           color = TRUE, shade = TRUE, xlab="Highest Degree", 
           ylab="Ownership",main="Proportion of Highest Degree")

####question5####
#to get the median and mean
summary(hw1data$undergrad_pop)
#caculate the deciles of the data
quantile(hw1data$undergrad_pop, prob = seq(0, 1, length = 11), na.rm = TRUE)

#draw boxplot of undergraduate population and the line of deciles and mean, exclude NA and outliers
boxplot(hw1data$undergrad_pop, outline = FALSE, main = "boxplot of undergraduate population",
        ylab = "Population",col = "green") 
abline(h = quantile(hw1data$undergrad_pop,seq(0, 1, 0.1),na.rm = TRUE), col = 'red',lty = 20)
abline(h = mean(hw1data$undergrad_pop, na.rm = TRUE),col='blue')

####question6####
#create a subset which only contain observations of those 5 populous states
five_states <- hw1data[hw1data$state%in%c("CA","TX","NY","IL","FL"),]
five_states <- droplevels(five_states)
#compare their in-state tuition cost by boxplot
boxplot(five_states$tuition ~ five_states$state, xlab = "state", 
        ylab = "in-state tuition", main = "5 most populous states' in-state tuitions")
#compare their out-of-state tuition cost by boxplot
boxplot(five_states$tuition_nonresident ~ five_states$state, xlab = "state", 
        ylab = "out-of-state tuition", main = "5 most populous states' out-of-state tuitions")

####question7#### partA
# find which row has largest sat and get that row's variable "name"
which.max(hw1data$avg_sat)
hw1data[105,]$name

####question7#### partB
#find which row has largest udnergraduate population and also check that row's name and open admissions
which.max(hw1data$undergrad_pop)
hw1data[2371,]$name
hw1data[2371,]$open_admissions

####7#### partC
#create a subset which only contain Public schools
public_university <- subset(hw1data, hw1data$ownership %in% c("Public"))
#find the observation with minimum average family income and its zipcode
which.min(public_university$avg_family_inc)
public_university[348,]$zip

####question7#### partD
#check whether the school with largest graduate population is same as school in partB
which.max(hw1data$grad_pop)
hw1data[248,]$name

####question8####parta
#create a subset
schools <- subset(hw1data, hw1data$ownership %in% c("For Profit") & hw1data$primary_degree %in% c("Bachelor"))

#draw density line of two variables in one graph to see their distribution
plot(density(schools$revenue_per_student), main = "revenue vs. spending  per student"
     , xlab = "dollars", ylim = c(0, 0.00030))
lines(density(schools$spend_per_student), col = 'red', lty = 3)
legend("topright",c("revenue", "spending"),col=c(1,2), lty=c(1,2))

#fitting those 2 varaibles into a regression model and draw a scatter plot with regression line
reg1 <- lm (schools$spend_per_student ~ schools$revenue_per_student)
summary(reg1)
plot(schools$revenue_per_student, schools$spend_per_student, xlab = "revenue per student", 
     ylab = "spend per student", main = "regression model of spending vs. revenue")
abline(reg1, col = 'red')

####question8#### partb
#set all NA in this subset equal to zero
schools[is.na(schools)] <- 0 
#create variable "total_net_income", the unit is thousands
schools$total_net_income <- ((schools$revenue_per_student - schools$spend_per_student) * (schools$undergrad_pop + schools$grad_pop)) / (1000)

#create a new data frame which only contains 2 variables, rank all observations by total_net_income
schools_income <- data.frame(schools$name, schools$total_net_income)
income_order <- order(schools_income$schools.total_net_income, decreasing = TRUE)
schools_income <- schools_income[income_order,]
#show top 5
head(schools_income, 5)

####question9#### parta
#fitting those 2 varaibles into a regression model and draw a scatter plot with regression line
reg2 <- lm(hw1data$admission ~ hw1data$avg_sat)
plot(hw1data$avg_sat, hw1data$admission, xlab = "average sat", ylab = "admission", 
     main= "relationship between avg_sat and admission")
abline(v=1200, col="blue")
abline(reg2, col = "red", lty = 3)

#create a new variable to split data, and update it
hw1data$group <- (hw1data$avg_sat >= 1200)
hw1data$group.update <- ifelse(is.na(hw1data$group), FALSE, hw1data$group)

#construct two subsets
sat_lower <- subset(hw1data, hw1data$avg_sat < 1200)
sat_higher <- subset(hw1data, hw1data$avg_sat >= 1200)

#draw the plot and regression line for group whose sat lower than 1200
plot(sat_lower$avg_sat, sat_lower$admission, xlab = "average sat", ylab = "admission", 
     main= "Relation for average sat lower than 1200")
reg3 <- lm(sat_lower$admission ~ sat_lower$avg_sat)
abline(reg3, col = "red")

#draw the plot and regression line for group whose sat higher than 1200
plot(sat_higher$avg_sat, sat_higher$admission, xlab = "average sat", ylab = "admission", 
     main= "Relation for average sat higher than 1200")
reg4 <- lm(sat_higher$admission ~ sat_higher$avg_sat)
abline(reg4, col = "red")

####question9#### partB(A) 
#view a rough distribution of two groups
summary(hw1data$med_10yr_salary[hw1data$group.update == "TRUE"])
summary(hw1data$med_10yr_salary[hw1data$group.update == "FALSE"])

#using boxplot to show their difference 
par(mfrow=c(1,2))
boxplot(hw1data$med_10yr_salary[hw1data$group.update == "TRUE"], 
        ylab = "dollars",main = "higher avg_sat group", col = "blue")
boxplot(hw1data$med_10yr_salary[hw1data$group.update == "FALSE"], 
        ylab ="dollars",main = "lower avg_sat group", col = "red")

####question9#### partB(B) 
#create a new variable
hw1data$race_combined <- hw1data$race_asian + hw1data$race_white

#view a rough distribution of two groups
summary(hw1data$race_combined[hw1data$group.update == "TRUE"])
summary(hw1data$race_combined[hw1data$group.update == "FALSE"])

#draw scatter plot to check their difference
par(mfrow=c(1,2))
plot(hw1data$race_combined[hw1data$group.update == "TRUE"],
     ylab = "percentage of combined", main = "combined percentage of higher avg_sat group" )
abline(h = 0.7097, col = 'red')
plot(hw1data$race_combined[hw1data$group.update == "FALSE"],
     ylab = "percentage of combined",main = "combined percentage of lower avg_sat group")
abline(h = 0.5685, col = 'red')

####question9#### partB(C) 
#create a new variable 
hw1data$grad_student_rate <- (hw1data$grad_pop)/(hw1data$undergrad_pop + hw1data$grad_pop)

#view a rough distribution of two groups
summary(hw1data$grad_student_rate[hw1data$group.update == "TRUE"])
summary(hw1data$grad_student_rate[hw1data$group.update == "FALSE"])

#draw scatter plot to check their difference
par(mfrow=c(1,2))
plot(hw1data$grad_student_rate[hw1data$group.update == "TRUE"],
     ylab = "percentage of graduate", main = "percentage of higher avg_sat group" )
abline(h = 0.2887, col = 'red')
plot(hw1data$grad_student_rate[hw1data$group.update == "FALSE"],
     ylab = "percentage of graduate",main = "percentage of lower avg_sat group")
abline(h = 0.2329, col = 'red')


####question9#### partC
#for each relationship, check their dependence by table

#a
relation1<-hw1data[c("group.update", "open_admissions")]
table1<-table(relation1)
table1

#b
relation2 <- hw1data[c("group.update", "main_campus")]
table2 <- table(relation2)
table2

#c
relation3 <- hw1data[c("group.update", "ownership")]
table3 <- table(relation3)
table3

#d
# create a new variable first, then create a table
hw1data$unique_branch <- (hw1data$branches == "1")
relation4 <- hw1data[c("group.update","unique_branch")]
table4 <- table(relation4)
table4


####question10#### part A
#create a regression model
reg5 <- lm(hw1data$avg_10yr_salary ~ hw1data$avg_family_inc)
summary((reg5))

# draw the regression line
par(mfrow=c(1,1))
plot(hw1data$avg_family_inc, hw1data$avg_10yr_salary, xlab = "average family income",
     ylab = "average salarys after starting college 10 years", main = "10 years salary vs. family income")
abline(reg5,col = 'red')

# split the dots by a line by observation
abline(h = 100000, col = 'blue')

#create a new variable to split data into two groups, update it 
hw1data$group2 <- (hw1data$avg_10yr_salary > 100000)
hw1data$group2.update <- ifelse(is.na(hw1data$group2), FALSE, hw1data$group2)

#create two subsets
high_salary <- subset(hw1data, hw1data$group2.update == "TRUE")
lower_salary <- subset(hw1data, hw1data$group2.update == "FALSE")

#study some difference between those two subsets
summary(high_salary$avg_sat)
summary(lower_salary$avg_sat)
par(mfrow=c(1,2))
boxplot(high_salary$avg_sat, ylab = "average sat score",
        main = "high salary group's avg sat", col = 'green')
boxplot(lower_salary$avg_sat, ylab = "average sat score",
        main = "low salary group's avg sat", col = 'red')

####question10b####
#create a catagorical variable, if avg_sat >= 1300, it equals 1, if avg_sat < 1300, it equals 0
hw1data$catego <- ifelse((hw1data$avg_sat >= 1300), 1, 0)
#make it from numeric to catagorical for caculation 
hw1data$catego <- as.factor(hw1data$catego)
summary(hw1data$catego)

#add this catagorical variable to the previous regression to create a new regression
reg_modified <- lm(avg_10yr_salary ~ catego + avg_family_inc, data = hw1data)
#check the difference between the new and old regression
summary(reg_modified)
summary(reg5)

