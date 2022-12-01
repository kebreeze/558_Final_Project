library(jsonlite)
library(tidyverse)
library(maps)
library(sp)
library(geojsonio)
library(geojsonlint)
#Contacting CDC's API. There are three possible endpoints to contact, "Total_Deaths", "Flu_Vaccination" and "Covid_Vaccination".

getEndpointInfo<- function(endpointName){
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

Total_Deaths<-getEndpointInfo("Total_Deaths")

Flu_Vaccination<-getEndpointInfo("Flu_Vaccination")

Covid_Vaccination<-getEndpointInfo("Covid_Vaccination")




#Function to clean data retrieved from the "Total_Deaths" endpoint

Clean_Deaths_Data<-function(endpointDataset){
  endpointDataset%>%
    mutate(
      Data_As_Of = as.Date(Total_Deaths$data_as_of, "%m/%d/%y"),
      Start_Week = as.Date(Total_Deaths$start_week, "%m/%d/%y"),
      End_Week = as.Date(Total_Deaths$end_week, "%m/%d/%y"),
      Week_Ending_Date = as.Date(substr(Total_Deaths$week_ending_date, 1, 10)),
      Age_Group = factor(Total_Deaths$age_group),
      Covid_19_Deaths = as.numeric(Total_Deaths$covid_19_deaths),
      Deaths_Total = as.numeric(Total_Deaths$total_deaths),
      Pneumonia_Deaths = as.numeric(Total_Deaths$pneumonia_deaths),
      Influenza_Deaths = as.numeric(Total_Deaths$influenza_deaths),
      Pneumonia_Or_Influenza_Deaths = as.numeric(Total_Deaths$pneumonia_or_influenza),
      Pneumonia_Influenza_Or_Covid_Deaths = as.numeric(Total_Deaths$pneumonia_influenza_or_covid),
      )%>%
    #Removing variables that were changed above, as well as variables that will not be necessary for our analysis
    select(!c("data_as_of", "week_ending_date", "start_week", "end_week", "age_group", "covid_19_deaths", "total_deaths", "pneumonia_deaths", "influenza_deaths", "pneumonia_or_influenza", "pneumonia_influenza_or_covid", "footnote"))
}

Deaths_Data<- Clean_Deaths_Data(Total_Deaths)






###############################################################
#### DATA EXPLORATION PAGE CODE###############################

###################Numerical Summaries#######################


##########Helper functions

#getVar() allows to return variable name based on user input
getVar<- function(variable){
  death_variable<-
    CDCvars[variable]
}

#getGrouping() allows to return grouping variable name based on user input
getGrouping<- function(groupByVariable){
  groupsBy<-
    groupings[groupByVariable]
}



####################Numerical Summaries

getSummary<- function(summary_variable, grouping){

#Allowing user input for variable to use for summary data. 
  death_variable<-
    getVar(summary_variable)
  # death_variable<- 
  #   if(summary_variable=="Covid 19 Deaths"){
  #     "Covid_19_Deaths"
  #   } else if(summary_variable=="Total Deaths (All Causes)"){
  #     "Deaths_Total"
  #   } else if(summary_variable=="Pneumonia Deaths"){
  #     "Pneumonia_Deaths"
  #   } else if(summary_variable=="Influenza Deaths"){
  #     "Influenza_Deaths"
  #   } else if(summary_variable=="Pneumonia Or Influenza Deaths"){
  #     "Pneumonia_Or_Influenza_Deaths"
  #   } else if(summary_variable=="Pneumonia, Influenza Or Covid 19 Deaths"){
  #     "Pneumonia_Influenza_Or_Covid_Deaths"
  #   }

#Allowing user input for how to group summary data    
  groupSummaryBy<-
     if(grouping=="Age"){
       "Age_Group"
     } else if(grouping=="Jurisdiction"){
       "jurisdiction"
     } else if(grouping=="Report Week"){
       "mmwrweek"
     } else if(grouping=="Report Year"){
       "mmwryear"
     }
  
   summaryData<- 
#Allowing user to see overall summary by selecting "All Groups".
     if(grouping=="All Groups"){
       Deaths_Data%>%
         summarise(
           Mean_Deaths_Per_Week = mean(.data[[death_variable]], na.rm=TRUE), 
           Median_Deaths_Per_Week = median(.data[[death_variable]], na.rm = TRUE),
           Maximum_Deaths_Per_Week = max(.data[[death_variable]], na.rm = TRUE))
#Allowing user to see summary by subgroup selected.
     } else {
       Deaths_Data%>%
         group_by(.data[[groupSummaryBy]])%>%
         summarise(
           Mean_Deaths_Per_Week = mean(.data[[death_variable]], na.rm=TRUE), 
           Median_Deaths_Per_Week = median(.data[[death_variable]], na.rm = TRUE),
           Maximum_Deaths_Per_Week = max(.data[[death_variable]], na.rm = TRUE))
     }

  return(summaryData)
}



sum<-getSummary("Covid 19 Deaths", "Age")











########WORKING ON CODE FOR STATE LOCATION DATA
stateLocation<-as_tibble(state.name)

stateInfo<-cbind(stateLocation, state.center, state.region)

stateInfoRename<- stateInfo%>%
  mutate(
    jurisdiction = stateInfo$value,
    Latitude = stateInfo$y,
    Longitude = stateInfo$x,
    Area = stateInfo$value,
    Region = stateInfo$state.region
  ) %>%
  select(!c(value, y, x, state.region))
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