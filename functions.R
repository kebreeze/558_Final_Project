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


#This will return the latest data from the CDC's API containing information on Covid 19 deaths
get_covid_data<- function(){
  #Accessing API KEY stored in .Renviron
  apiToken<-Sys.getenv("apiToken")
  #Accessing baseURL stored in .Renviron
  baseURL<-Sys.getenv("baseURL")
  #Generating endpointCode for specified endpoints stored in .Renviron
  endpointCode<- Sys.getenv("total_deaths")
  
  searchURL<- paste0(baseURL, endpointCode, "?", "$limit=50000&", "$$app_token=", apiToken)

  #Return raw data from our API using the fromJSON function from the jsnolite package.
  covid_Raw_Dataset<- fromJSON(searchURL)
  
  #Clean data 
  covid_clean_Data<-
    covid_Raw_Dataset%>%
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
      #Remove variables that were changed above or that will not be necessary for our analysis
      select(!c("data_as_of", "week_ending_date", "start_week", "end_week", "age_group", "covid_19_deaths", "total_deaths", "pneumonia_deaths", "influenza_deaths", "pneumonia_or_influenza", "pneumonia_influenza_or_covid", "footnote", "group", "indicator"))%>%
    as_tibble()
  
  
  return(covid_clean_Data)
}

Deaths_Data<-get_covid_data()



########################Data Exploration Code###########################

##############Numerical Summaries

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



####################Graphical Summaries


get_bar_plot("Covid 19 Deaths", "By Year Grouped By Age")

get_bar_plot<- function(variable, TypeOfBar){
  #Allowing user input for variable to use for bar graph. CDCvars is a dictionary of key value pairs saved in dict.R 
  death_variable<-
    CDCvars[variable]
  
  #Bar Plots based on user inputs
  deaths_bar<-
    
    if(TypeOfBar == "By Year Only"){
      ggplot(Deaths_Data, aes(x=mmwryear, y=.data[[death_variable]])) + 
        geom_col() +
        labs(
          x="Report Year",
          y=variable
        )
      
    } else if(TypeOfBar == "By Age Only"){
      ggplot(Deaths_Data, aes(x=Age_Group, y=.data[[death_variable]])) + 
        geom_col() +
        labs(
          x="Age Group",
          y=variable
        )
      
    } else if(TypeOfBar == "By Year Grouped By Age"){
      ggplot(Deaths_Data, aes(x=mmwryear, y=.data[[death_variable]])) + 
        geom_col(aes(fill = Age_Group), position = "dodge") + 
        labs(
          x="Report Year",
          y=variable
        ) +
        scale_fill_discrete(name="Age Group")
      
    } else if(TypeOfBar == "By Age Grouped By Year"){
      ggplot(Deaths_Data, aes(x=Age_Group, y=.data[[death_variable]])) + 
        geom_col(aes(fill = mmwryear), position = "dodge") + 
        labs(
          x="Age Group",
          y=variable
        ) +
        scale_fill_discrete(name="Report Year")
    }
  
  
  
  # 
  # deathsPlot<- 
  #   #Allowing user to see overall summary by selecting "All Groups".
  #   if(graphGroups=="All Groups"){
  #     ggplot(Deaths_Data, aes(x=.data[[death_variable]])) +
  #       geom_bar()
  #   } else {
  #     ggplot(Deaths_Data, aes(x=.data[[death_variable]])) +
  #       geom_bar(aes(fill=.data[[graphGroups]]))
  #   }
  
  return(deaths_bar)
}




