library(jsonlite)
library(tidyverse)
library(maps)
library(sp)
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

###############################################################
#### DATA EXPLORATION PAGE CODE###############################

summary(Deaths_Data$Deaths_Total)

joined<-full_join(Deaths_Data, stateInfoRename)

Deaths_Data_Regions<-joined%>%
  mutate(
    Area_Covered = if(Area%in%)
  )



#################INTERACTIVE MAP###############################
  
  mapStates<- map("state", fill = TRUE, plot = FALSE)

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