library(tidyverse)
library(maps)
library(sp)
library(geojsonio)
library(geojsonlint)
library(leaflet) 

lmAsummary<-summary(lmA)
table(lmAsummary)

## Install the required package with:
## install.packages("RSocrata")

library("RSocrata")

df <- read.socrata(
  "https://data.cdc.gov/resource/3rge-nu2a.json",
  app_token = Sys.getenv("apiToken"))



social<- fromJSON("https://data.cdc.gov/resource/9hdi-ekmb.json")

ggplot(Deaths_Data, aes(x=mmwryear, y=Deaths_Data[,CDCvars[1]])) + 
  geom_col()

longYear<-summary_wrapper("Covid 19 Deaths", "Report Year")

longSum<-sum%>%
  pivot_longer(cols=2:7, names_to = "Summary_Type", values_to = "Number_Of_Deaths")

#Bar graphs by grouping
ggplot(Deaths_Data, aes(x=mmwryear, y=Covid_19_Deaths)) + 
  geom_col()

ggplot(longSum, aes(x=Age_Group, y=Number_Of_Deaths)) + 
  geom_col(aes(fill=Summary_Type), position="dodge")

ggplot(longYear, aes(x=mmwryear, y=Number_Of_Deaths)) + 
  geom_col(aes(fill=Summary_Type), position="dodge")


ggplot(Deaths_Data, aes(x=mmwryear, y=Covid_19_Deaths)) + 
  geom_col(aes(fill=Age_Group), position="dodge")


ggplot(Deaths_Data, aes(x=mmwryear, y=Covid_19_Deaths)) + 
  geom_col(aes(fill=mean(Covid_19_Deaths)), position="dodge")

names(CDCvars[1])
CDCvars["Total Deaths (All Causes)"]
names(CDCvars["Covid 19 Deaths"])

print(paste(c("Summary Table of", names(CDCvars[1]))))

#Histogram of deaths per week
ggplot(Deaths_Data, aes(x=Covid_19_Deaths)) + 
  geom_histogram(binwidth = 1000)


#Boxplot of deaths per week
ggplot(Deaths_Data, aes(x=mmwryear, y=Deaths_Total)) + 
  geom_boxplot()

#Scatterplot
ggplot(Deaths_Data, aes(x=Pneumonia_Deaths, y=Covid_19_Deaths)) +
  geom_point(aes(color=Deaths_Total))


#Line graph
ggplot(Deaths_Data, aes(x=End_Week, y=Covid_19_Deaths)) +
  geom_line()

#Facteing
ggplot(Deaths_Data, aes(x=End_Week, y=Covid_19_Deaths, color=mmwrweek)) +
  geom_line() +
  facet_wrap(facets = vars(Age_Group))


