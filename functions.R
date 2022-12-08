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

#This will return the latest data from the CDC's API containing information on Covid 19 deaths. This code will return up to 50,000 observations from the dataset. This code will be run once when a user accesses the app, which will allow the dataset to be retrieved from the CDC's API
get_covid_data<- function(){
  apiToken<-Sys.getenv("apiToken")#Accessing API KEY stored in .Renviron
  baseURL<-Sys.getenv("baseURL")#Accessing baseURL stored in .Renviron
  endpointCode<- Sys.getenv("total_deaths")#Generating endpointCode for specified endpoints stored in .Renviron
  searchURL<- paste0(baseURL, endpointCode, "?", "$limit=50000&", "$$app_token=", apiToken)
  
  covid_Raw_Dataset<- fromJSON(searchURL)#Return raw data from our API using the fromJSON function from the jsnolite package and the searchURL created.
  
  #Clean data 
  covid_clean_Data<-
    covid_Raw_Dataset%>%
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
      )%>%
      #Remove variables that were changed above or that will not be necessary for our analysis
      select(!c("data_as_of", "week_ending_date", "start_week", "end_week", "age_group", "covid_19_deaths", "total_deaths", "pneumonia_deaths", "influenza_deaths", "pneumonia_or_influenza", "pneumonia_influenza_or_covid", "footnote", "group", "indicator"))%>%
    na.omit()%>%#remove observations with NA values
    as_tibble()
  
  return(covid_clean_Data)
}

#Store the results of get_covid_data() function in a tibble named Deaths_Data
Deaths_Data<-get_covid_data()

####Creating a dataset for later model building##############
#We are going to create a model to predict Covid_19_Deaths using variables that do not contain any information specific to covid deaths(Pneumonia_Influenza_Or_Covid_Deaths). We will also remove the Pneumonia_Or_Influenza_Deaths variable as it is highly correlated with other predictor variables for influenza and pneumonia deaths. 

create_model_dataset<-function(deathsDatasetFull){
  deathsDatasetFull%>%
    select(!c(Pneumonia_Influenza_Or_Covid_Deaths,  
              Pneumonia_Or_Influenza_Deaths, Data_As_Of))
}

#Store the results of create_model_dataset in a tibble named Deaths_Model_Data that will be used in the model building stage
Deaths_Model_Set<-create_model_dataset(Deaths_Data)


########################Data Exploration Code###########################

##############Numerical Summaries

#The create_summaries() function will take in a character vector of desired summaries that the user requests and return the appropriate values.
#create_summaries<-function(summaries_requested, summary_variable, groups_by){
  #Allowing user input for variable to use for summary data. CDCvars is a dictionary of key value pairs saved in dict.R 
#  death_variable<-
#    CDCvars[summary_variable]

  #Allowing user input for how to group summary data. groupings is a dictionary of key value pairs saved in dict.R
#  group_summary_by<-
#    groupings[groups_by]
  
#  sumDF<-as_tibble()

#    for (variable in summaries_requested){
#      if(variable=="Minimum Deaths Per Week"){
    #     Minimum<- Deaths_Data%>%
    #       summarize("Minimum Deaths Per Week" = min(.data[[death_variable]]))
    #   } else {
    #     print("not minimum")
    #   }
    #   # sumType<- summaryCheckBox[[variable]]
    #   # sumVec<- c(sumVec, sumType)}
    # } 
#  checkbox<-paste(sumVec, collapse = ", ")
  # summary_data<- 
  #   #Allowing user to see overall summary by selecting "All Groups".
  #   if(groups_by=="All Groups"){
  #     Deaths_Data%>%
  #       na.omit()%>%
  #       summarize(.data, checkbox)
  #     
  #     #Allowing user to see summary by subgroup selected.
  #   } else {
  #     Deaths_Data%>%
  #       na.omit%>%
  #       group_by(.data[[group_summary_by]])%>%
  #       summarize(.data, checkbox)
  #   }
   #return(checkbox)
#}

# create_summaries(summaries_requested = sumVec)
# summaryCheckBox[["Minimum Deaths Per Week"]]

# sumVec<-c("Minimum Deaths Per Week", "Q1 Deaths Per Week")
# create_summaries(summaries_requested = "Minimum Deaths Per Week", summary_variable="Covid 19 Deaths")

#getSummary will return summary statistics based on user input for summary_variable and grouping.
create_summary<- function(summary_variable, groups_by){
  
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



###############get_scatter() function
# ggplot(Deaths_Data, aes(x=Pneumonia_Deaths, y=Covid_19_Deaths)) +
#   geom_point(aes(color=Deaths_Total))
# 
# get_scatter<- function(xVariable, yVariable, colorVariable){
#   #Allowing user input for variable to use for x in scatter plot. CDCvars is a dictionary of key value pairs saved in dict.R 
#   x_variable<-
#     CDCvars[xVariable]
#   
#   #Allowing user input for variable to use for y in scatter plot. 
#   y_variable<-
#     CDCvars[yVariable]
#   
#   
#   #Allowing user input for variable to use for colorVariable in scatter plot. 
#   color_variable<-
#     CDCvars[xVariable]
#   
#   
#   #Scatter Plots based on user inputs
#   deaths_bar<-
#     
#     if(TypeOfBar == "By Year Only"){
#       ggplot(Deaths_Data, aes(x=.data[[death_variable]], y=.data[[death_variable]])) + 
#         geom_col() +
#         labs(
#           x="Report Year",
#           y=variable
#         )
#       
#     } else if(TypeOfBar == "By Age Only"){
#       ggplot(Deaths_Data, aes(x=Age_Group, y=.data[[death_variable]])) + 
#         geom_col() +
#         labs(
#           x="Age Group",
#           y=variable
#         )
#       
#     } else if(TypeOfBar == "By Year Grouped By Age"){
#       ggplot(Deaths_Data, aes(x=mmwryear, y=.data[[death_variable]])) + 
#         geom_col(aes(fill = Age_Group), position = "dodge") + 
#         labs(
#           x="Report Year",
#           y=variable
#         ) +
#         scale_fill_discrete(name="Age Group")
#       
#     } else if(TypeOfBar == "By Age Grouped By Year"){
#       ggplot(Deaths_Data, aes(x=Age_Group, y=.data[[death_variable]])) + 
#         geom_col(aes(fill = mmwryear), position = "dodge") + 
#         labs(
#           x="Age Group",
#           y=variable
#         ) +
#         scale_fill_discrete(name="Report Year")
#     }
#   return(deaths_scatter)
# }












#################MODEL FITTING###############################



##########Creating a train and test set########################
#Split the data to create trainIndex
create_split<-function(percent){
  trainIndex<- createDataPartition(Deaths_Model_Set$Covid_19_Deaths, p = percent/100, list=FALSE)
  }

#trainIndex<-create_split(15)

#Create train set
create_train_set<-function(Deaths_Model_Set, trainIndex){
  deathTrain<- Deaths_Model_Set[trainIndex,]
}

#trainSet<-create_train_set(Deaths_Model_Set, trainIndex)
#Create test set
create_test_set<-function(Deaths_Model_Set, trainIndex){
  deathTest<- Deaths_Model_Set[-trainIndex,]
}

#testSet<-create_test_set(Deaths_Model_Set, trainIndex)
# 
# #############MLR Model#####################
# 
# 
# MLRmodel
# 
# ######MLR Function for server use
create_MLR<-function(varsSelected=NULL, trainingData){
  chosenVars<-
   modelVars[varsSelected]
  
  responseVar<- "Covid_19_Deaths"
  predictorVars<- c(chosenVars)
  #creating formulas based on user input. If user does not input variables the model will be built using all variables by default.
  formulaVar<-
    if(is.null(varsSelected)){
      as.formula(Covid_19_Deaths ~ .)
    } else {
      as.formula(paste(responseVar, paste(predictorVars, collapse = " + "), sep = " ~ "))
    }
  
  MLRmodel<- train(formulaVar, 
                   data=trainingData,
                   method = "lm",
                   trControl = trainControl(method = "cv"))
  return(MLRmodel)
}

modelVars
# # 
lmA<-create_MLR(trainingData=d)

lmA$results

DT::datatable(lmA$results)
# 
# ##################Tibble for comparing fit stats on training set
# 
# trainingStats<- tibble(model=c("MLRmodel"), RMSE=c(MLRmodel$results$RMSE), Rsquared=c(MLRmodel$results$Rsquared))
# 
# ###################Tree Model###################
# 
# library(rpart)
# 
# #TreeFit<-rpart(formula=Covid_19_Deaths ~ ., data=split$deathTrain)
# 
# TreeFit<- train(Covid_19_Deaths ~., data=split$deathTrain,
#                 method="rpart",
#                 trControl = trainControl(method = "cv"))
# 
# 
# min(TreeFit$results$RMSE)
# TreeFit
# 
# printcp(TreeFit)
# 
# TreeFit$results[1,2:3]
# 
# TreeFit$results$Rsquared[1]
# 
# (TreeFit)
# 
# 
# #add fit stats to tibble
# trainingStats<- rbind(trainingStats, c("TreeModel", min(TreeFit$results$RMSE), max(TreeFit$results$Rsquared)))
# 
# TreeFit
# plot(TreeFit)
# text(TreeFit)
# 
# 
# #################Random Forest Model###########
# library(randomForest)
# 
# #  rfFit<- randomForest(Covid_19_Deaths ~., data = deathTrain, importance=TRUE)
# 
# 
# rfFit<- train(Covid_19_Deaths ~., data = split$deathTrain,
#               method = "rf",
#               trControl = trainControl(method = "cv"),
#               tuneGrid = data.frame(mtry = 1:3))
# 
# rfFit$finalModel
# rfFit$results
# 
# #add fit stats to tibble
# trainingStats<- rbind(trainingStats, c("RandomForestModel", min(rfFit$results$RMSE), max(rfFit$results$Rsquared)))
