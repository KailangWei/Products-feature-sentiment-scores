#setting directory
getwd()
setwd("C:/Users/14702/OneDrive/Desktop")
getwd()

#Importing Libraries
library(readr)
library(dplyr)

#Importing Data
tools <- read_csv("C:/Users/14702/OneDrive/Desktop/tool_data.csv")

#Look at the categories and their counts
categories_count = tools%>%
count(category)%>%
  arrange(desc(n))

#choose your category
cat_name= "wet & dry vacuums"


cat_data1 = tools%>%
  filter(category==cat_name)

write.csv(cat_data1, "cat_data1.csv")
