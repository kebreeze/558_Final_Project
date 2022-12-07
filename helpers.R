library(jsonlite)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)
library(rpart)
library(randomForest)
library(caret)
#Contacting CDC's API. There are three possible endpoints to contact, "Total_Deaths", "Flu_Vaccination" and "Covid_Vaccination".

get_endpoint_info<- function(endpointName){
  #Accessing API KEY stored in .Renviron
  apiToken<-Sys.getenv("apiToken")
  #Accessing baseURL stored in .Renviron
  baseURL<-Sys.getenv("baseURL")
  #Generating endpointCode for specified endpoints stored in .Renviron
  endpointCode<- 
    if(endpointName=="Total_Deaths"){
    Sys.getenv("total_deaths")
  } else if(endpointName=="Flu_Vaccination"){
    Sys.getenv("flu_vaccination")
  } else if(endpointName=="Covid_Vaccination"){
    Sys.getenv("covid_vaccination")
  }
  
  searchURL<- paste0(baseURL, endpointCode, "?", "$limit=50000&", "$$app_token=", apiToken)
  #At this point we have built our URL query string given the user input of endpointName. If we were returning data from the "activities"Total_Deaths" endpoint the searchURL that the function is generating at this point looks like the code below:

    #"https://data.cdc.gov/resource/ynw2-4viq.json?$$app_token=YOUR_APP_TOKEN(KEY)"
  
  #We then use this URL to return the data we want from our API using the fromJSON function from the jsnolite package.
  endpointDataset<- fromJSON(searchURL)

  return(endpointDataset)
}

Total_Deaths<-get_endpoint_info("Total_Deaths")

Flu_Vaccination<-get_endpoint_info("Flu_Vaccination")

Covid_Vaccination<-get_endpoint_info("Covid_Vaccination")




#Function to clean data retrieved from the "Total_Deaths" endpoint

Clean_Deaths_Data<-function(endpointDataset){
  endpointDataset%>%
    mutate(
      Data_As_Of = as.Date(Total_Deaths$data_as_of, "%m/%d/%y"),
      Start_Week = as.Date(Total_Deaths$start_week, "%m/%d/%y"),
      End_Week = as.Date(Total_Deaths$end_week, "%m/%d/%y"),
      Age_Group = factor(Total_Deaths$age_group),
      Covid_19_Deaths = as.numeric(Total_Deaths$covid_19_deaths),
      Deaths_Total = as.numeric(Total_Deaths$total_deaths),
      Pneumonia_Deaths = as.numeric(Total_Deaths$pneumonia_deaths),
      Influenza_Deaths = as.numeric(Total_Deaths$influenza_deaths),
      Pneumonia_Or_Influenza_Deaths = as.numeric(Total_Deaths$pneumonia_or_influenza),
      Pneumonia_Influenza_Or_Covid_Deaths = as.numeric(Total_Deaths$pneumonia_influenza_or_covid),
      )%>%
    #Removing variables that were changed above, as well as variables that will not be necessary for our analysis
    select(!c("data_as_of", "week_ending_date", "start_week", "end_week", "age_group", "covid_19_deaths", "total_deaths", "pneumonia_deaths", "influenza_deaths", "pneumonia_or_influenza", "pneumonia_influenza_or_covid", "footnote", "group", "indicator"))
}

Deaths_Data<- Clean_Deaths_Data(Total_Deaths)






###############################################################
#### DATA EXPLORATION PAGE CODE###############################

###################Numerical Summaries#######################


#getSummary will return summary statistics based on user input for summary_variable and grouping.
get_summary<- function(summary_variable, groups_by){

#Allowing user input for variable to use for summary data. CDCvars is a dictionary of key value pairs saved in dict.R 
  death_variable<-
    CDCvars[summary_variable]

#Allowing user input for how to group summary data. groupings is a dictionary of key value pairs saved in dict.R
  group_summary_by<-
    groupings[groups_by]
  
   summary_data<- 
#Allowing user to see overall summary by selecting "All Groups".
     if(groups_by=="All Groups"){
       Deaths_Data%>%
         na.omit()%>%
         summarize(
           "Minimum Deaths Per Week" = min(.data[[death_variable]]),
           "Q1 Deaths Per Week" = quantile(.data[[death_variable]], 0.25),
           "Median Deaths Per Week" = median(.data[[death_variable]]),
           "Mean Deaths Per Week" = round(mean(.data[[death_variable]])),
           "Q3 Deaths Per Week" = quantile(.data[[death_variable]], 0.75),
           "Maximum Deaths Per Week" = max(.data[[death_variable]]),
           "Count" = n())

#Allowing user to see summary by subgroup selected.
     } else {
       Deaths_Data%>%
         na.omit%>%
         group_by(.data[[group_summary_by]])%>%
         summarize(
           "Minimum Deaths Per Week" = min(.data[[death_variable]]),
           "Q1 Deaths Per Week" = quantile(.data[[death_variable]], 0.25),
           "Median Deaths Per Week" = median(.data[[death_variable]]),
           "Mean Deaths Per Week" = round(mean(.data[[death_variable]])),
           "Q3 Deaths Per Week" = quantile(.data[[death_variable]], 0.75),
           "Maximum Deaths Per Week" = max(.data[[death_variable]]),
           "Count" = n())
     }

  return(summary_data)
}



sum<-get_summary("Covid 19 Deaths", "Report Year")

#Function to create long format data
long_format_summary<- function(summary_data){
  summary_data%>%
    pivot_longer(cols=2:7, names_to = "Summary_Type", values_to = "Number_Of_Deaths")
}


longSum<-long_format_summary(sum)


#Wrapper function to create long format summaries based on variable and grouping
summary_wrapper<-function(variable, by_group){
  sum<- get_summary(variable, by_group)
  long_format_sum<-long_format_summary(sum)
}

Sum<-summary_wrapper("Covid 19 Deaths", "Age")


get_bar_plot<- function(variable, graphGroups){
  #Allowing user input for variable to use for summary data. CDCvars is a dictionary of key value pairs saved in dict.R 
  death_variable<-
    CDCvars[variable]
  
  #Allowing user input for how to group summary data. groupings is a dictionary of key value pairs saved in dict.R    
  groupBy<-
    groupings[graphGroups]
  
  deathsPlot<- 
    #Allowing user to see overall summary by selecting "All Groups".
    if(graphGroups=="All Groups"){
      ggplot(Deaths_Data, aes(x=.data[[death_variable]])) +
        geom_bar()
    } else {
      ggplot(Deaths_Data, aes(x=.data[[death_variable]])) +
        geom_bar(aes(fill=.data[[graphGroups]]))
      }

  return(deathsPlot)
}









ggplot(Deaths_Data, aes(x=CDCvars[1])) +
  geom_bar(aes(fill=Deaths_Data[,groupings[1]]))

Deaths_Data[,CDCvars[1]]

Deaths_Data[,groupings[["Age"]]]

groupings["Age"]

groupings[["Age"]]



########WORKING ON CODE FOR STATE LOCATION DATA###############################################
# stateLocation<-as_tibble(state.name)
# 
# stateInfo<-cbind(stateLocation, state.center, state.region)
# 
# stateInfoRename<- stateInfo%>%
#   mutate(
#     jurisdiction = stateInfo$value,
#     Latitude = stateInfo$y,
#     Longitude = stateInfo$x,
#     Area = stateInfo$value,
#     Region = stateInfo$state.region
#   ) %>%
#   select(!c(value, y, x, state.region))
#in stateInfo, latitude is y and longitude is x


#Assigning HHS Regions 

#################INTERACTIVE MAP###############################
  
  mapStates<- map("state", fill = TRUE, plot = FALSE)

baseMap<-leaflet(data = mapStates)%>%
  addTiles()



  leaflet(data = mapStates)%>%
    addTiles() %>%
    addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

  
  m = leaflet() %>% addTiles()
  df = data.frame(
    lat = rnorm(100),
    lng = rnorm(100),
    size = runif(100, 5, 20),
    color = sample(colors(), 100)
  )
  m = leaflet(df) %>% addTiles()
  m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
  m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))
  
  
  
  library(GGally)
  Deaths_Data%>%
    dplyr::select(Deaths_Total, Covid_19_Deaths, Pneumonia_Deaths, Influenza_Deaths, Pneumonia_Or_Influenza_Deaths, Pneumonia_Influenza_Or_Covid_Deaths)%>%
    ggpairs()
    
  GGally::ggcorr(Deaths_Model_Set, label=TRUE)
  
  
  Deaths_Data%>%
    ggplot(aes(x=mmwrweek, y=Covid_19_Deaths)) +
    geom_point(aes(color=Age_Group))
  

#################MODEL FITTING###############################
#We are going to try to create a model to predict Covid_19_Deaths using variables that do not contain any information specific to covid deaths. We will also remove the Pneumonia_Or_Influenza_Deaths variable as it is highly correlated with other predictor variables for influenza and pneumonia deaths. Before we begin builidng the models we want to remove the variable "Pneumonia_Influenza_Or_Covid_Deaths" as well as removing NA values from the data set

Deaths_Model_Set<- Deaths_Data%>%
    na.omit%>%
    select(!c(Pneumonia_Influenza_Or_Covid_Deaths,  Pneumonia_Or_Influenza_Deaths, Data_As_Of))


##########Creating a train and test set########################
    trainIndex<- createDataPartition(Deaths_Model_Set$Covid_19_Deaths, p = 0.15, list=FALSE)
  deathTrain<- Deaths_Model_Set[trainIndex,]
  deathTest<- Deaths_Model_Set[-trainIndex,]
  
#############MLR Model#####################
  MLRmodel<- train(Covid_19_Deaths ~ ., data=deathTrain,
                   method = "lm",
                   trControl = trainControl(method = "cv"))

  MLRmodel


###################Tree Model###################

  library(rpart)
  
  TreeFit<-rpart(formula=Covid_19_Deaths ~ ., data=deathTrain)

  plot(TreeFit$variable.importance)
  

TreeFit
  plot(TreeFit)
  text(TreeFit)
  

#################Random Forest Model###########
library(randomForest)
  
#  rfFit<- randomForest(Covid_19_Deaths ~., data = deathTrain, importance=TRUE)
  

  rfFit<- train(Covid_19_Deaths ~., data = deathTrain[,-1],
                method = "rf",
                trControl = trainControl(method = "cv"),
                tuneGrid = data.frame(mtry = 1:3))
  
  plot(rfFit)

