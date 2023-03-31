#read in data
data<-read.csv(file.choose(), header = TRUE)

#view and checking the properties of the data
View(data)
dim(data)
str(data)
#--------------------------------------------------------------------------------------
#importing the necessary library
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
#install.packages("scales")
library(scales)
#--------------------------------------------------------------------------------------
# Exploratory Data Analysis


#gender
pie <- ggplot(data, aes(x = "", fill = Gender)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  ggtitle("Response By Gender") +
  theme_void()

# Add labels to the pie chart
pie + geom_text(aes(label = ..count..),
                stat = "count",
                position = position_stack(vjust = 0.5))


# Preference
pie <- ggplot(data, aes(x = "", fill = Which.of.the.two.methods.would.you.prefer.)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  ggtitle("Preference Chart") +
  labs(fill = "Preference")+
  theme_void()

# Add labels to the pie chart
pie + geom_text(aes(label = scales::percent(..count../sum(..count..))), 
            stat = "count", 
            position = position_stack(vjust = 0.5))

#Gender by level (stacked bar)

ggplot(data, aes(x = factor(Level), fill = factor(Gender))) +
  geom_bar() +
  labs(title = "Stundents Gender By Level",
       x = "Level",
       y = "Number Of Stundents") +
  scale_fill_discrete(name = "Gender")

#Preference by Gender and LEvel

ggplot(data, aes(x = Which.of.the.two.methods.would.you.prefer., fill = Gender)) +
  geom_bar(position = "dodge") +
  facet_grid(. ~ Level) +
  labs( title = "     Preference by Gender and Level", x = "Method", y = "Number Of Students", fill = "Gender") +
  stat_count(geom = "text", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5)



#Preference by Department
ggplot(data, aes(x = factor(Department), fill = factor(Which.of.the.two.methods.would.you.prefer.)))+
  geom_bar()+ 
  labs(title = "        Students' Preference By Gender", x= 'Department',
       y= "Number of Stundents") + scale_fill_discrete(name = "Method")


#Age distribution by preference 

ggplot(data, aes(x = factor(Age), fill = factor(Which.of.the.two.methods.would.you.prefer.)))+
  geom_bar()+ 
  labs(title = "        Students' Preference By Age Group", x= 'Age',
       y= "Number of Stundents") + scale_fill_discrete(name = "Method")


# BOX Plot for amount Spent on the method.
dat <- data.frame(
  
  Physical = data$The.average.amount.spent.in.a.week.on.physical.classes.including.transportation,
  Virtual = data$The.average.amount.spent.on.VLE.including.data.charges.per.week)

# Reshape data into long format
data_long <- gather(dat, key = "column", value = "value", Physical, Virtual)
# Create combined box plot
ggplot(data_long, aes(x = '', y = value)) +
  geom_boxplot() +
  facet_wrap(~ column, scales = "free_y") +
  labs(x = "Method", y = "") +
  ggtitle("Box plot for Amount Spent On Physical and Virtual Classes")


#group gpa by preference

data <- data %>%
  mutate(CGPA = (GPA.during.physical.classes + GPA.during.VLE ) / 2) %>%
  mutate(CGPA = case_when(
    CGPA >= 4.5 ~ "First Class",
    CGPA >= 3.5 & CGPA < 4.5  ~ "Second Class Upper",
    CGPA >= 2.5 & CGPA < 3.5  ~ "Second Class Lower Division",
    CGPA >= 1.5 & CGPA <2.5  ~ "Third Class",
    CGPA < 1.5  ~ "Pass",
  ))

ggplot(data, aes(x = CGPA, fill = Which.of.the.two.methods.would.you.prefer.)) +
  geom_bar() +
  labs(title = "        Preference By CGPA", x= 'CGPA', fill = 'Preference',
       y= "Number of Stundents") +
  geom_text(aes(label = ..count..),
                  stat = "count",
                  position = position_stack(vjust = 0.5))

#first_class per Department
first_class = data[data$CGPA == 'First Class',]
first_class

ggplot(first_class, aes(x= Department))+geom_bar()+
  labs(title = "        First Class Per Department",
       y= "Number of Stundents", color = 'red') +
  geom_text(aes(label = ..count..),
            stat = "count",
            position = position_stack(vjust = 0.5))


# Statistical Analysis
#Removing outlier

# Identify outlier values using boxplot
outlier <- boxplot(data$The.average.amount.spent.in.a.week.on.physical.classes.including.transportation)$out

# Remove rows with outlier values (The Amount ROWs)
mydata <- subset(data, !The.average.amount.spent.in.a.week.on.physical.classes.including.transportation %in% outliers) 
outlier2 <- boxplot(mydata$The.average.amount.spent.on.VLE.including.data.charges.per.week)$out

# Remove rows with outlier values
mydata <- subset(mydata,!The.average.amount.spent.on.VLE.including.data.charges.per.week %in% outlier2)
dim(mydata)

#Summary of Amount after removing outlier
summary(mydata$The.average.amount.spent.in.a.week.on.physical.classes.including.transportation)
summary(mydata$The.average.amount.spent.on.VLE.including.data.charges.per.week)

mode(mydata$The.average.amount.spent.in.a.week.on.physical.classes.including.transportation)
mode(mydata$The.average.amount.spent.on.VLE.including.data.charges.per.week)

mode

# Paired T Test for GPA
t.test(data$GPA.during.physical.classes, data$GPA.during.VLE, paired = TRUE, conf.level = 0.95)

#Difference of two means for amount

t.test(mydata$The.average.amount.spent.in.a.week.on.physical.classes.including.transportation , data$The.average.amount.spent.on.VLE.including.data.charges.per.week , paired = FALSE, conf.level = 0.95)

#Anova
#Performance by Department
sts =subset(data,Department=='Statistics')
mts =subset(data,Department=='Mathematics')
chm =subset(data,Department=='Chemistry')
phs =subset(data,Department=='Physics')
csc =subset(data,Department=='Computer Science')


#physical
department = c(rep('sts', 50), rep('mts', 50),rep('chm', 50),rep('phs', 50)
               ,rep('csc', 50))
gpa = c(sts$GPA.during.physical.classes,mts$GPA.during.physical.classes,chm$GPA.during.physical.classes,
        phs$GPA.during.physical.classes,csc$GPA.during.physical.classes)
dept = data.frame(department,gpa)
dept_analysis<- aov(gpa~department, data = dept)
summary(dept_analysis)

#virtual

department = c(rep('sts', 50), rep('mts', 50),rep('chm', 50),rep('phs', 50)
               ,rep('csc', 50))
gpa_v = c(sts$GPA.during.VLE,mts$GPA.during.VLE,chm$GPA.during.VLE,
          phs$GPA.during.VLE,csc$GPA.during.VLE)
dept_v = data.frame(department,gpa_v)
dept_analysis_v<- aov(gpa_v~department, data = dept_v)
summary(dept_analysis_v)



