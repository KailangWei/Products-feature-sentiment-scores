# Set the directory
getwd()
setwd("C:/Users/14702/OneDrive/Desktop")
getwd()


#Import Libraries
library(sentimentr)
library(dplyr)
library(readxl)
library(tokenizers)
library(janeaustenr)
library(tidytext)
library(tidyr)
library(tm)
library(data.table)
library(NLP)
library(topicmodels)
library(tidy)
library(broom)
library(stringr)
library(tidyverse)


#Import the category data
cat_data=read.csv("cat_data.csv")

#Exploration
ncol(cat_data)
names(cat_data)

#naming the first column to key
names(cat_data)[1] <-"key"
names(cat_data)[20] <-"products"

#Removing certain columns
cat_data = select(cat_data,-c(sku_number,brand_binary ,caption, SBDsum, model_cate,max_mc, m_cate))

class(cat_data)
cat_data = unique(cat_data[c("sku", "text")])

distinct(cat_data, c("sku", "text"), .keep_all = TRUE)
###################################################Topics###########################################################################
#List the topics you extracted here

topics_new = c("battery" ,"tool" ,"power" ,"drill" ,"price" ,"quality" ,"weight" ,"charger" ,"driver" ,"impact" ,"light" ,"blower" ,"purchase",
                "capacity" ,"cord" ,"adapter" ,"button" ,"press" ,"nail" ,"lithium" ,"output" ,"warranty" ,"indicator" ,"purchase" ,"grinder",
                "runtime" ,"chain" ,"mower" ,"wrench" ,"impact" ,"system" ,"balance" ,"longevity" ,"usage" ,"upgrade" ,"cycle" ,"check" ,"condition",
               "steel" ,"kobalt" ,"display" ,"feature" ,"gauge" ,"level" ,"vacuum" ,"space" ,"extension" ,"couple" ,"plastic" ,"reliable" ,"metal",
               "board" ,"torque" ,"dryer" ,"length" ,"convenience" ,"force" ,"heavy" ,"time" ,"speed" ,"design" ,"trigger" ,"material" ,"pressure" ,"experience" ,"perform",
               "drill","battery", "power", "impact","torque","speed","drill","screw","motor","chuck","blower","trigger" , "wrench" , "stall" , "driver" ,"setting",
               "warranty", "service","router","plastic","task" ,"purpose","performace","switch","capacity" ,"blade","grinder" ,"design" ,"nozzle") 

topics_new = c("measure")
  
#how many new topics 
length(topics_new)

#Remove duplicates
topics_new=unique(topics_new)

#Number of new topics
length(topics_new)

#Creating columns for topics for category data
no_of_columns_cat =ncol(cat_data)

#create column names
for(i in 1:length(topics_new)){
  cat_data$a <- NA
  names(cat_data)[no_of_columns_cat+i] <- topics_new[i]
}

#####################################################################################################################################


#Remove NAs 
cat_data = cat_data[!is.na(cat_data$text),]

#tokenize the sentences
cat_data1 <- cat_data %>% 
  tidytext::unnest_tokens(input=text, output=sentence, token="sentences")

#Not needed , but just like that. Remove NAs 
cat_data1 = cat_data1[!is.na(cat_data1$sentence),]

#Compute Sentiment Scores
cat_data1$score = sentiment_by(cat_data1$sentence)$ave_sentiment

#Populate scores across column for every row
cat_data1[is.na(cat_data1)] <- cat_data1$score

#selecting only required columns
cat_data1 = cat_data1[ , c(topics_new,"sentence","key" )]

#Pivoting the data
test <- cat_data1 %>%
  pivot_longer(topics_new, names_to = "Features" , values_to = "Scores")

#Creating a match column
test$match <- str_detect(test$sentence, test$Features)

#Keeping only True values
test <- test[test$match==TRUE,]

#Clubbing back to review level
test <- test %>%
  group_by(key,Features) %>%
  mutate(mean_score = mean(Scores,na.rm=T))

#Merging the data
df = merge(test,cat_data, by.x="key" ,by.y ="key"  ,all.x= TRUE,all.y= TRUE)

#Remove topics columns
df[,topics_new] <-NULL
df = na.omit(df)
df$overall_sentiment <- sentiment_by(as.character(df$text))$ave_sentiment
df$Scores <-NULL

df$key <-NULL
df$match <-NULL

#write.csv(df, "final.csv")

#//////////////////////////////Appending//////////////////////////////////////////////////////////////////////////////////////////
final=read.csv("final.csv")
final= final[ ,-1]

#Append
data= rbind(final ,df)

#let's adjust the scores
data$mean_score[data$mean_score < -1] = -1
data$mean_score[data$mean_score >1] = 1

total_sentiment=data$overall_sentiment
total_sentiment[total_sentiment >1] <- 1
total_sentiment[total_sentiment < -1] = -1

data$overall_sentiment =total_sentiment

#Write out the final data
write.csv(data, "final.csv")
