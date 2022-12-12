library(tidyverse)
library(maps)
library(sp)
library(geojsonio)
library(geojsonlint)
library(leaflet) 

#test<-data.frame(matrix(ncol=length(Deaths_Data), nrow = 1))

#colnames(test)<-c(1:13)

generateBox <- function(title, status="primary", inputId, label) {
  headerBox <- box(
    title = title,
    width = NULL,
    solidHeader = TRUE,
    status = status,
    lm_vars_text,
    checkboxGroupInput(
      inputId = inputId,
      label = label,
      choices = names(modelVars)
    )
  )
  
  return (headerBox)
}

#UI
tabPanel("Prediction",
         fluidRow(
           #Input prediction values
           column(
             width = 3,
             box(
               title="What model would you like to use for your perdictions?",
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               selectInput(
                 inputId = "model",
                 label = "Pick your model",
                 choices = c("MLR Model", "Regression Tree Model", "Random Forest Model")
               )
             ),
             box(
               uiOutput("predictVariables")
             ),
             box(
               title = "Predict Using Your MLR Model",
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               selectInput(
                 inputId = "week",
                 label = "Report Week",
                 choices = 1:52
               ),
               selectInput(
                 inputId = "year",
                 label = "Report Year",
                 choices = c(2020, 2021, 2022)
               ),
               selectInput(
                 inputId = "jurisdiction",
                 label = "Jurisdiction",
                 choices = state.name
               ),
               selectInput(
                 inputId = "age",
                 label = "Age Group",
                 choices = c("All Ages",
                             "0-17 years",
                             "18-64 years",
                             "65 years and over")
               ),
               sliderInput(
                 inputId = "deathTotal",
                 label = "Total Deaths (All Causes)",
                 min = 0,
                 max = 100000,
                 value = 0
               ),
               sliderInput(
                 inputId = "pneumonia",
                 label = "Pneumonia Deaths",
                 min = 0,
                 max = 10000,
                 value = 0
               ),
               sliderInput(
                 inputId = "flu",
                 label = "Influenza Deaths",
                 min = 0,
                 max = 3000,
                 value = 0
               ),
               actionButton(
                 inputId = "predict",
                 label = "Make Prediction",
                 width = "100%",
                 class = "btn-info",
                 icon = icon("magnifying-glass-chart")
               )
             )
           ),
           column(
             width = 9,
             box(
               title = "Values Used to Calculate Predictions for Covid (MLR Model)",
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               textOutput(outputId = "predictionsTable"
               )
             )
           )
           
         ))
)),


#server
tabPanel("Prediction",
         fluidRow(
           #Input prediction values
           column(
             width = 3,
             box(
               uiOutput("predictVariables")
             ),
             box(
               title = "Predict Using Your MLR Model",
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               selectInput(
                 inputId = "week",
                 label = "Report Week",
                 choices = 1:52
               ),
               selectInput(
                 inputId = "year",
                 label = "Report Year",
                 choices = c(2020, 2021, 2022)
               ),
               selectInput(
                 inputId = "jurisdiction",
                 label = "Jurisdiction",
                 choices = state.name
               ),
               selectInput(
                 inputId = "age",
                 label = "Age Group",
                 choices = c("All Ages",
                             "0-17 years",
                             "18-64 years",
                             "65 years and over")
               ),
               sliderInput(
                 inputId = "deathTotal",
                 label = "Total Deaths (All Causes)",
                 min = 0,
                 max = 100000,
                 value = 0
               ),
               sliderInput(
                 inputId = "pneumonia",
                 label = "Pneumonia Deaths",
                 min = 0,
                 max = 10000,
                 value = 0
               ),
               sliderInput(
                 inputId = "flu",
                 label = "Influenza Deaths",
                 min = 0,
                 max = 3000,
                 value = 0
               ),
               actionButton(
                 inputId = "predict",
                 label = "Make Prediction",
                 width = "100%",
                 class = "btn-info",
                 icon = icon("magnifying-glass-chart")
               )
             )
           ),
           column(
             width = 9,
             box(
               title = "Values Used to Calculate Predictions for Covid (MLR Model)",
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               textOutput(outputId = "predictionsTable"
               )
             )
           )
           
         ))
)),

































strsplit(as.character(treeMV), "+")

str(treeMV)

print(treeMV)

summary(lmA)

table(lmTS)
lmTS<-create_test_stats(lmA, ts)

lmA<-create_MLR(trainingData = tr)

s<-create_split(15)
tr<-create_train_set(Deaths_Model_Set, s)
ts<-create_test_set(Deaths_Model_Set, s)

lmAPred<-predict(lmA, ts)
postResample(lmAPred, obs = ts$Covid_19_Deaths)
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


