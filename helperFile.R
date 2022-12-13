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




#############################Importing data and cleaning data functions#############

#This will return the latest data from the CDC's API containing information on Covid 19 deaths. This code will return up to 50,000 observations from the dataset. This code will be run once when a user accesses the app, which will allow the dataset to be retrieved from the CDC's API
get_covid_data <- function() {
  apiToken <-
    Sys.getenv("apiToken")#Accessing API KEY stored in .Renviron
  baseURL <-
    Sys.getenv("baseURL")#Accessing baseURL for the covid deaths endpoint stored in .Renviron
  searchURL <-
    paste0(baseURL, "?", "$limit=50000&", "$$app_token=", apiToken)
  
  covid_Raw_Dataset <-
    fromJSON(searchURL)#Return raw data from our API using the fromJSON function from the jsnolite package and the searchURL created.
  
  #Clean data
  covid_clean_Data <-
    covid_Raw_Dataset %>%
    mutate(
      Data_As_Of = as.Date(covid_Raw_Dataset$data_as_of, "%m/%d/%y"),
      Start_Week = as.Date(covid_Raw_Dataset$start_week, "%m/%d/%y"),
      End_Week = as.Date(covid_Raw_Dataset$end_week, "%m/%d/%y"),
      Age_Group = factor(covid_Raw_Dataset$age_group),
      Covid_19_Deaths = as.numeric(covid_Raw_Dataset$covid_19_deaths),
      Deaths_Total = as.numeric(covid_Raw_Dataset$total_deaths),
      Pneumonia_Deaths = as.numeric(covid_Raw_Dataset$pneumonia_deaths),
      Influenza_Deaths = as.numeric(covid_Raw_Dataset$influenza_deaths),
      Pneumonia_Or_Influenza_Deaths = as.numeric(covid_Raw_Dataset$pneumonia_or_influenza),
      Pneumonia_Influenza_Or_Covid_Deaths = as.numeric(covid_Raw_Dataset$pneumonia_influenza_or_covid),
    ) %>%
    #Remove variables that were changed above or that will not be necessary for our analysis
    select(
      !c(
        "data_as_of",
        "week_ending_date",
        "start_week",
        "end_week",
        "age_group",
        "covid_19_deaths",
        "total_deaths",
        "pneumonia_deaths",
        "influenza_deaths",
        "pneumonia_or_influenza",
        "pneumonia_influenza_or_covid",
        "footnote",
        "group",
        "indicator"
      )
    ) %>%
    na.omit() %>% #remove observations with NA values
    as_tibble()
  
  return(covid_clean_Data)
}
#########################
###############
#################FIX THIS WHEN API IS WORKING
###############
#Store the results of get_covid_data() function in a tibble named Deaths_Data
Deaths_Data<-get_covid_data()



#######################Creating a dataset for later model building##############
#We are going to create a model to predict Covid_19_Deaths using variables that do not contain any information specific to covid deaths(Pneumonia_Influenza_Or_Covid_Deaths), highly correlated variables (Pneumonia_Or_Influenza_Deaths) that could interfere with model building, and variables that are redundant or not relevant to building a predictive model (Data_As_Of, Start_Week, End_Week). 

create_model_dataset<-function(deathsDatasetFull){
  deathsDatasetFull%>%
    select(!c(Pneumonia_Influenza_Or_Covid_Deaths,  
              Pneumonia_Or_Influenza_Deaths, Data_As_Of, Start_Week, End_Week))
}

#Store the results of create_model_dataset in a tibble named Deaths_Model_Data that will be used in the model building stage
Deaths_Model_Set<-create_model_dataset(Deaths_Data)


########################Data Exploration Code###########################

##############Numerical Summaries

#create_summary will return summary statistics based on user input for summary_variable and grouping.
create_summary <- function(summary_variable, groups_by) {
  #Allowing user input for variable to use for summary data. CDCvars is a dictionary of key value pairs saved in dict.R
  death_variable <-
    CDCvars[summary_variable]
  
  #Allowing user input for how to group summary data. groupings is a dictionary of key value pairs saved in dict.R
  group_summary_by <-
    groupings[groups_by]
  
  summary_data <-
    #Allowing user to see overall summary by selecting "All Groups".
    if (groups_by == "All Groups") {
      Deaths_Data %>%
        na.omit() %>%
        summarize(
          "Minimum Deaths Per Week" = min(.data[[death_variable]]),
          "Q1 Deaths Per Week" = quantile(.data[[death_variable]], 0.25),
          "Median Deaths Per Week" = median(.data[[death_variable]]),
          "Mean Deaths Per Week" = round(mean(.data[[death_variable]])),
          "Q3 Deaths Per Week" = quantile(.data[[death_variable]], 0.75),
          "Maximum Deaths Per Week" = max(.data[[death_variable]]),
          "Count" = n()
        )
      #Allowing user to see summary by subgroup selected.
    } else {
      Deaths_Data %>%
        na.omit %>%
        group_by(.data[[group_summary_by]]) %>%
        summarize(
          "Minimum Deaths Per Week" = min(.data[[death_variable]]),
          "Q1 Deaths Per Week" = quantile(.data[[death_variable]], 0.25),
          "Median Deaths Per Week" = median(.data[[death_variable]]),
          "Mean Deaths Per Week" = round(mean(.data[[death_variable]])),
          "Q3 Deaths Per Week" = quantile(.data[[death_variable]], 0.75),
          "Maximum Deaths Per Week" = max(.data[[death_variable]]),
          "Count" = n()
        )
    }
  return(summary_data)
}


#create_center_summary will return measures of center (mean, median) based on user input for summary_variable and grouping.
create_center_summary <- function(summary_variable, groups_by) {
  #Allowing user input for variable to use for summary data. CDCvars is a dictionary of key value pairs saved in dict.R
  death_variable <-
    CDCvars[summary_variable]
  
  #Allowing user input for how to group summary data. groupings is a dictionary of key value pairs saved in dict.R
  group_summary_by <-
    groupings[groups_by]
  
  summary_data <-
    #Allowing user to see overall summary by selecting "All Groups".
    if (groups_by == "All Groups") {
      Deaths_Data %>%
        na.omit() %>%
        summarize(
          "Median Deaths Per Week" = median(.data[[death_variable]]),
          "Mean Deaths Per Week" = round(mean(.data[[death_variable]]))
        )
      #Allowing user to see summary by subgroup selected.
    } else {
      Deaths_Data %>%
        na.omit %>%
        group_by(.data[[group_summary_by]]) %>%
        summarize(
          "Median Deaths Per Week" = median(.data[[death_variable]]),
          "Mean Deaths Per Week" = round(mean(.data[[death_variable]]))
        )
    }
  return(summary_data)
}


#create_spread_summary will return measures of spread (standard deviation, IQR) based on user input for summary_variable and grouping.
create_spread_summary <- function(summary_variable, groups_by) {
  #Allowing user input for variable to use for summary data. CDCvars is a dictionary of key value pairs saved in dict.R
  death_variable <-
    CDCvars[summary_variable]
  
  #Allowing user input for how to group summary data. groupings is a dictionary of key value pairs saved in dict.R
  group_summary_by <-
    groupings[groups_by]
  
  summary_data <-
    #Allowing user to see overall summary by selecting "All Groups".
    if (groups_by == "All Groups") {
      Deaths_Data %>%
        na.omit() %>%
        summarize(
          "Standard Deviation Deaths Per Week" = sd(.data[[death_variable]]),
          "IQR Deaths Per Week" = IQR(.data[[death_variable]])
        )
      #Allowing user to see summary by subgroup selected.
    } else {
      Deaths_Data %>%
        na.omit %>%
        group_by(.data[[group_summary_by]]) %>%
        summarize(
          "Standard Deviation Deaths Per Week" = sd(.data[[death_variable]]),
          "IQR Deaths Per Week" = IQR(.data[[death_variable]])
        )
    }
  return(summary_data)
}


#create_count_summary will return counts statistics (number of values, number of distinct values) based on user input for summary_variable and grouping.
create_count_summary <- function(summary_variable, groups_by) {
  #Allowing user input for variable to use for summary data. CDCvars is a dictionary of key value pairs saved in dict.R
  death_variable <-
    CDCvars[summary_variable]
  
  #Allowing user input for how to group summary data. groupings is a dictionary of key value pairs saved in dict.R
  group_summary_by <-
    groupings[groups_by]
  
  summary_data <-
    #Allowing user to see overall summary by selecting "All Groups".
    if (groups_by == "All Groups") {
      Deaths_Data %>%
        na.omit() %>%
        summarize(
          "n" = n(),
          "n distinct" = n_distinct(.data[[death_variable]])
        )
      #Allowing user to see summary by subgroup selected.
    } else {
      Deaths_Data %>%
        na.omit %>%
        group_by(.data[[group_summary_by]]) %>%
        summarize(
          "n" = n(),
          "n distinct" = n_distinct(.data[[death_variable]])
        )
    }
  return(summary_data)
}
#############################Graphical Summaries


###############get_bar_plot() function

create_bar_plot<- function(variable, TypeOfBar){
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
  return(deaths_bar)
}



###############create_scatter() function

create_scatter<-function(x, y, color){
  varX<-varLabels[x]
  varY<-varLabels[y]
  colorByVar<-varLabels[color]
  
  ggplot(Deaths_Data, aes(x=.data[[varX]], y=.data[[varY]])) +
    geom_point(aes(color=.data[[colorByVar]]))
}



###########create_histogram() function
create_histogram <- function(x, bin) {
  histVar <- histVars[x]
  
  ggplot(Deaths_Data, aes(x = .data[[histVar]])) +
    geom_histogram(binwidth = bin)
}






###########################MODEL FITTING###############################



##########Code to create a train and test set########################
#Code to split the data to create trainIndex
create_split<-function(percent){
  set.seed(123)
  trainIndex<- createDataPartition(Deaths_Model_Set$Covid_19_Deaths, p = percent/100, list=FALSE)
}

#Code to create train set
create_train_set<-function(Deaths_Model_Set, trainIndex){
  deathTrain<- Deaths_Model_Set[trainIndex,]
}

#Code to create test set
create_test_set<-function(Deaths_Model_Set, trainIndex){
  deathTest<- Deaths_Model_Set[-trainIndex,]
}


# #############Code to Create the MLR Model allowing for input of a training data set and the variables to include in the model#####################

#####create_formula() helper function to generate a formula to use in our model building functions based on user input from the UI#####
create_formula<-function(varsSelected=NULL){
  chosenVars<-
    modelVars[varsSelected]
  
  responseVar<- "Covid_19_Deaths"
  predictorVars<- chosenVars
  #creating formulas based on user input. If user does not input variables the function will create a formula containing all variables by default.
  formulaVar<-
    if(is.null(varsSelected)){
      as.formula(Covid_19_Deaths ~ .)
    } else {
      as.formula(paste(responseVar, paste(predictorVars, collapse = " + "), sep = " ~ "))
    }
  return(formulaVar)
}

#######create_MLR Function for server use
create_MLR<-function(varsInput=NULL, trainingData){
  modelFormula<- create_formula(varsInput)
  #Use train() function from caret package to create MLR model based on UI input. 
  MLRmodel<- train(modelFormula, 
                   data=trainingData,
                   method = "lm",
                   trControl = trainControl(method = "cv"))
  return(MLRmodel)
}





# df<- data.frame(
#   mmwrweek = input$week,
#   mmwryear = input$year,
#   jurisdiction = input$jurisdiction,
#   Age_Group = input$age,
#   Deaths_Total = input$deathTotal,
#   Pneumonia_Deaths = input$pneumonia,
#   Influenza_Deaths = input$flu
# )



######create model df for use with ui on predict page

# df<- data.frame(
#    mmwrweek = ,
#    mmwryear = ,
#    jurisdiction = ,
#    Age_Group = ,
#    Deaths_Total = ,
#    Pneumonia_Deaths = ,
#    Influenza_Deaths = 
#  )



create_predict_ui<- function(modelVarsInputs){
  dfLength<- length(modelVarsInputs)
  
  df<- data.frame(modelVarsInputs)
  
  wide<- df%>%
    pivot_wider(names_from = modelVarsInputs, values_from = modelVarsInputs)
}

df<-create_predict_ui(c("cat", "dog", "fish"))


##################Tibble for comparing fit stats on training set
# 
# trainingStats<- tibble(model=c("MLRmodel"), RMSE=c(MLRmodel$results$RMSE), Rsquared=c(MLRmodel$results$Rsquared))
# 
####################create_tree() function to create tree model based on user input###################

create_tree<- function(varsInput=NULL, trainingData){
  modelFormula<- create_formula(varsInput)
  #Use train() function from the caret package to create tree model based on UI input. 
  tree_model<- train(modelFormula, 
                     data=trainingData,
                     method="rpart",
                     tuneGrid = data.frame(cp = c(0, 0.001, 0.01)),
                     trControl = trainControl(method = "cv"))
  return(tree_model)
}


# #################create_forest() function to create Random Forest Model###########

# #  rfFit<- randomForest(Covid_19_Deaths ~., data = deathTrain, importance=TRUE)
create_forest<- function(varsInput=NULL, trainingData){
  modelFormula<- create_formula(varsInput)
  #Use train() function from the caret package to create random forest model based on UI input. 
  rf_model<- train(modelFormula, 
                   data=trainingData,
                   method = "rf",
                   trControl = trainControl(method = "cv"),
                   tuneGrid = data.frame(mtry = 1:3))
  return(rf_model)
}

######create_test_stats() function to generate stats on test data for comparison
create_test_stats<- function(model, testSet){
  Predictions<-predict(model, testSet)
  testStats<-postResample(Predictions, obs = testSet$Covid_19_Deaths)
  return(testStats)
}


###################################Prediction Tab functions########################


predictCovid<-function(model, dataFrameInput){
  newData<-
    data.frame(
      mmwrweek = input$week,
      mmwryear = input$year,
      jurisdiction = input$jurisdiction,
      Age_Group = input$age,
      Deaths_Total = input$deathTotal,
      Pneumonia_Deaths = input$pneumonia,
      Influenza_Deaths = input$flu
    )
  return(newData)
}