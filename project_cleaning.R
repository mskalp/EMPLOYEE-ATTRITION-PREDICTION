
#Libraries
install.packages('rio') 
library(rio)
install.packages('mice') 
library(mice)
install.packages('skimr') 
library(skimr)
install.packages('missMDA') 
library(missMDA)
install.packages('tidyverse') 
library(tidyverse) #stringr ggplot2 dplyr
install.packages("summarytools")
library(summarytools)
install.packages("Rcpp")
library(Rcpp)
install.packages("caret")
library(caret)

#Read input file
link = 'aug_train.csv'
df <- import(link)
############## EDA#################
# check first 2observations
head(df,2)

#Check the dimensions
#observations:19158 ,variables: 14
dim(df)

#Summary of the data
summary(df)

#There are missing values(empty string) present for 8 variables.gender,enrolled_university,education_level
#major_discipline,company_size,company_type,last_new_job
#and in experience.There are no spaces or NA
#present in other 6 variables.So atleast 6 
#variable values are present for all observations
#and enrolle_id is unique id.so there are no duplicate observations
summarytools::view(dfSummary(df))

#all values are not displayed in summary report 
#for experience and city variables.So checking
#unique values seperately.There are missing value 
#in experience variable.
unique(df$experience)
unique(df$city)

#Examine missing values.There are many missing values(empty string) 
#present for 8 variables.But its not getting displayed
#here as its all character type variables

md.pattern(df,
           rotate.names = T)
skim(df)
#atleast 6 variable values are present for all observations
#So we need to analyze missing values for variables.
#replace spaces with NA
                                         # Duplicate data frame
df1<-df
df1[df1 == ""] <- NA                    # Replace blank by NA
#Use skimr to categorize variables and get info
#on your dataset
skim(df1)

Skimmedaug_Train <- skimr::skim(df1)

Skimmedaug_Train %>% select(skim_variable, n_missing) %>%
  filter(n_missing != 0) %>%
  ggplot(aes(
    x = fct_reorder(skim_variable, n_missing),
    y = n_missing,
    label = n_missing,
    fill = skim_variable
  )) +
 # geom_col() +
  geom_bar(stat="identity", color="brown",fill="brown3")+
  geom_text(hjust = -0.3,
         color = "black",
        fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 7000)) +
  theme_classic()+
  xlab('Variables') + 
  ylab('Missing value count') +
  theme(legend.position = "none")

#we already know for all observations,values present
#atleast for 6 variables including enrollee_id.So we can not delete
#any records.We need to fill the missing values
cond = complete.cases(df1) == T
df[cond,]->out1

#before filling values we need to check duplicates
##A2. Check for duplicated values
cond = duplicated(x = df, fromLast = FALSE) == T
df[cond,]

#Next line duplicated
cond = duplicated(df, fromLast = T) == T
df[cond,]
#No duplicate values present

#We are going to fill missed values
#Do a column by column cleaning

#1 enrollee_id
#Data type is correct because is an integer type
#enrolle_id is a unique id, so its count should match the total observations
#and its matching.
#Statistical summary
summary(df$enrollee_id)
length(df$enrollee_id)
unique(df$enrollee_id)

#2.city
##Data type is correct because is a character type
#Check categories
unique(df$city)
#Check # of characters
cond = nchar(df$city) <= 5
df[cond,]$city

#3.city_development_index
##Data type is correct because is a number type
summary(df$city_development_index)#no outliers

#4.gender
##Data type is correct because is a character type
#Check categories--missing values present
unique(df$gender)
#replace blank with NA
cond = df$gender == ''
df[cond,]$gender <- NA

#5.relevent_experience
##Data type is correct because is a character type
#Check categories
unique(df$relevent_experience)

#6.enrolled_university
##Data type is correct because is a character type
#Check categories--missing values present
unique(df$enrolled_university)
#replace blank with NA
cond = df$enrolled_university == ''
df[cond,]$enrolled_university <- NA

#7.education_level
##Data type is correct because is a character type
#Check categories--missing values present
unique(df$education_level)
#replace blank with NA
cond = df$education_level == ''
df[cond,]$education_level <- NA

#8.major_discipline
##Data type is correct because is a character type
#Check categories--missing values present
unique(df$major_discipline)
#replace blank with NA
cond = df$major_discipline == ''
df[cond,]$major_discipline <- NA

#9.experience
##Data type is correct because is a character type
#Check categories--missing values present
unique(df$experience)
#replace blank with NA
cond = df$experience == ''
df[cond,]$experience <- NA

#10.company_size
##Data type is correct because is a character type
#Check categories--missing values present
unique(df$company_size)
#replace blank with NA
cond = df$company_size == ''
df[cond,]$company_size <- NA

#11.company_type
##Data type is correct because is a character type
#Check categories--missing values present
unique(df$company_type)
#replace blank with NA
cond = df$company_type == ''
df[cond,]$company_type <- NA

#12.last_new_job
##Data type is correct because is a character type
#Check categories--missing values present
unique(df$last_new_job)
#replace blank with NA
cond = df$last_new_job == ''
df[cond,]$last_new_job <- NA

#13.training hours
##Data type is correct because is a integer type
#can change it to number
summary(df$training_hours)#
#outlier check
#max valu is high compared to mean
#825 observations where value is greater than 200
#so not considering as outliers
cond = df$training_hours >= 200
df[cond,]->out2

#14.target
##Data type is correct because is a number type
#can change it to integer if we want
summary(df$target)#

summarytools::view(dfSummary(df))
#Now using imputeMCA to fill ann missing values for 8 variables
#last_new_job,company_type,company_size
#experience,major_discipline,education_level
#enrolled_university,gender

columnstofill = c('last_new_job','company_type','company_size',
                  'experience','major_discipline','education_level',
                  'enrolled_university','gender')
head(df[,columnstofill],3)


filledcolumns <- imputeMCA(df[,columnstofill])

#Looking inside results
filledcolumns <- filledcolumns$completeObs
#Convert filledcolumns to DF before overwriting
filledcolumns <- as.data.frame(filledcolumns)

#Replace columnstofill with filledcolumns
df[,columnstofill] <- filledcolumns

summarytools::view(dfSummary(df))
####################

#Remove enrolle_id and city 
#Enrolle_id is unique
#Removing city.
#A variable named city_development_index is also present in table.
#Each city has only one development index.
#There are 123 categories of city present and its making algorithms slow.
#Also when checked,this variable is not helping the prediction

df <- df[,-c(1,2)]
#before filling values we need to check duplicates
##A2. Check for duplicated values
cond = duplicated(x = df, fromLast = FALSE) == T
df[cond,]

#Next line duplicated
cond = duplicated(df, fromLast = T) == T
df[cond,]
#summarytools::view(dfSummary(df))
#97 duplicate present now as we deleted unique id
df <- unique(df)
#observation:19040  variable: 12
dim(df)


#####################

##Make your output (target) a categorical variable
df$target <- as.factor(df$target)

##dependent variable:target,posiible output,1 -quit company 0-stay company
freq_bg <- table(df$target)
#Convert table to a dataframe
freq_brand_df <- as.data.frame(freq_bg)
freq_brand_df 
#plot it using ggplot
#Rename
colnames(freq_brand_df) <- c('target','freq')
ggplot(data = freq_brand_df,
       aes(x = target,
           y = freq,
           fill = target)) + 
  geom_col(position = 'dodge') +
  scale_fill_manual(values=c("red4", "black"))+
   xlab('Target') + 
    ylab('Count') +
  ggtitle('Target Variable Analysis') +
  theme_classic()

########EDA############################################

#Analyzing 12 variables against output(target)

#Convert variable relevent_experience to factor
cols <- c("relevent_experience")
df[cols] <- lapply(df[cols], factor)  ## as.factor() could also be used


#1.gender vs target
#Create a freq table of 'gender' and 'target'
freq_bg <- table(df$gender,df$target)
#Convert table to a dataframe
freq_brand_df <- as.data.frame(freq_bg)
freq_brand_df 
#Rename
colnames(freq_brand_df) <- c('gender','target','freq')

#plot it using ggplot
ggplot(data = freq_brand_df,
       aes(x = target,
           y = freq,
           fill = gender)) + 
  geom_col(position = 'dodge') +
  ggtitle('gender/target') +
  theme_classic()

#2.relevent_experience vs target
#Create a freq table of 'relevent_experience' and 'target'
freq_bg <- table(df$relevent_experience,df$target)
#Convert table to a dataframe
freq_brand_df <- as.data.frame(freq_bg)
freq_brand_df 
#Rename
colnames(freq_brand_df) <- c('relevent_experience','target','freq')

#plot it using ggplot
ggplot(data = freq_brand_df,
       aes(x = target,
           y = freq,
           fill = relevent_experience)) + 
  geom_col(position = 'dodge') +
  ggtitle('relevent_experience/target') +
  theme_classic()

#3.enrolled_university vs target
#Create a freq table of 'enrolled_university' and 'target'
freq_bg <- table(df$enrolled_university,df$target)
#Convert table to a dataframe
freq_brand_df <- as.data.frame(freq_bg)
freq_brand_df 
#Rename
colnames(freq_brand_df) <- c('enrolled_university','target','freq')

#plot it using ggplot
ggplot(data = freq_brand_df,
       aes(x = target,
           y = freq,
           fill = enrolled_university)) + 
  geom_col(position = 'dodge') +
  ggtitle('enrolled_university/target') +
  theme_classic()

#4.education_level vs target
#Create a freq table of 'education_level' and 'target'
freq_bg <- table(df$education_level,df$target)
#Convert table to a dataframe
freq_brand_df <- as.data.frame(freq_bg)
freq_brand_df 
#Rename
colnames(freq_brand_df) <- c('education_level','target','freq')

#plot it using ggplot
ggplot(data = freq_brand_df,
       aes(x = target,
           y = freq,
           fill = education_level)) + 
  geom_col(position = 'dodge') +
  # xlab('') + 
  #  ylab('Preferences') +
  ggtitle('education_level/target') +
  theme_classic()

#5.major_discipline vs target
#Create a freq table of 'major_discipline' and 'target'
freq_bg <- table(df$major_discipline,df$target)
#Convert table to a dataframe
freq_brand_df <- as.data.frame(freq_bg)
freq_brand_df 
#Rename
colnames(freq_brand_df) <- c('major_discipline','target','freq')

#plot it using ggplot
ggplot(data = freq_brand_df,
       aes(x = target,
           y = freq,
           fill = major_discipline)) + 
  geom_col(position = 'dodge') +
  ggtitle('major_discipline/target') +
  theme_classic()


#6.experience vs target
#Create a freq table of 'major_discipline' and 'target'
freq_bg <- table(df$experience,df$target)
#Convert table to a dataframe
freq_brand_df <- as.data.frame(freq_bg)
freq_brand_df 
#Rename
colnames(freq_brand_df) <- c('experience','target','freq')

#plot it using ggplot
ggplot(data = freq_brand_df,
       aes(x = target,
           y = freq,
           fill = experience)) + 
  geom_col(position = 'dodge') +
  # xlab('') + 
  #  ylab('Preferences') +
  ggtitle('experience/target') +
  theme_classic()
##################
#7.company_size vs target
#Create a freq table of 'company_size' and 'target'
freq_bg <- table(df$company_size,df$target)
#Convert table to a dataframe
freq_brand_df <- as.data.frame(freq_bg)
freq_brand_df 
#Rename
colnames(freq_brand_df) <- c('company_size','target','freq')

#plot it using ggplot
ggplot(data = freq_brand_df,
       aes(x = target,
           y = freq,
           fill = company_size)) + 
  geom_col(position = 'dodge') +
  # xlab('') + 
  #  ylab('Preferences') +
  ggtitle('company_size/target') +
  theme_classic()
#8.company_type vs target
#Create a freq table of 'company_type' and 'target'
freq_bg <- table(df$company_type,df$target)
#Convert table to a dataframe
freq_brand_df <- as.data.frame(freq_bg)
freq_brand_df 
#Rename
colnames(freq_brand_df) <- c('company_type','target','freq')

#plot it using ggplot
ggplot(data = freq_brand_df,
       aes(x = target,
           y = freq,
           fill = company_type)) + 
  geom_col(position = 'dodge') +
  # xlab('') + 
  #  ylab('Preferences') +
  ggtitle('company_type/target') +
  theme_classic()
#9.last_new_job vs target
#Create a freq table of 'last_new_job' and 'target'
freq_bg <- table(df$last_new_job,df$target)
#Convert table to a dataframe
freq_brand_df <- as.data.frame(freq_bg)
freq_brand_df 
#Rename
colnames(freq_brand_df) <- c('last_new_job','target','freq')

#plot it using ggplot
ggplot(data = freq_brand_df,
       aes(x = target,
           y = freq,
           fill = last_new_job
   #        color="brown"
           )) + 
 # geom_bar()+
  geom_col(position = 'dodge') +
  #scale_fill_manual(values=c("red4", "brown4","red3","brown3","brown2","brown1"))+
 
  # xlab('') + 
  #  ylab('Preferences') +
  ggtitle('last new job by target') +
  theme_classic()

#10.training_hours vs target

df %>% select(training_hours, target) %>%
  ggplot(aes(x = training_hours)) +
  geom_histogram(bins = 10, color = "brown", fill = "brown3") +
  theme_classic()+
  facet_wrap(vars(target)) +
  labs(
    title = "Training hours by Target ",
    x = "Training hours",
    y = "Count"
  )

#11.city_development_index vs target
df %>% select(city_development_index, target) %>%
  ggplot(aes(x = city_development_index)) +
  geom_histogram(bins = 10, color = "brown", fill = "brown3") +
  theme_classic()+
  facet_wrap(vars(target)) +
  labs(
    title = "City development index by Target ",
    x = "city development index",
    y = "Count"
  )

summarytools::view(dfSummary(df))

export(df,'outfile2.csv')





