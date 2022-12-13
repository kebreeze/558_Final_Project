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

print("CURRENT DIRECTORY")
print(getwd())
# source(".Renviron")
 source("text.R")
 source("dict.R")
 source("helperFile.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #########Data Exploration Page Code###########
  ##Summary data tables based on user inputs##
  output$summary<- renderDT(
    {summaryTable<-
      if (input$type=="All Basic Summaries"){
        create_summary(input$varDeath, input$groups)
      } else if(input$type=="Measures of Center"){
        create_center_summary(input$varDeath, input$groups)
      } else if(input$type=="Measures of Spread"){
        create_spread_summary(input$varDeath, input$groups)
      } else if(input$type=="Counts"){
        create_count_summary(input$varDeath, input$groups)
      }
      }, 
    class="cell-border stripe hover", 
    caption = as.character(input$varDeath)
  )
  
  ##Bar Plots based on user inputs##
  output$barPlot<- renderPlot(
    {create_bar_plot(input$varDeathBar, input$barplotType)})
  
  output$scatterPlot<- renderPlot(
    {create_scatter(x=input$scatterx, y=input$scattery, color = input$scatteColor)})
  
  output$histPlot<- renderPlot(
    {create_histogram(x=input$histogram, bin = input$bin)}
  )
  ##########Model Fitting Page Code################
  
  ######Using eventReactive to store outputs from user selections for model inputs when action button runModels is pressed#########
  
  #create index, train and test sets based on user input for percentage to include in training set
  trainIndex <-
    reactive({
      create_split(input$split)
    }) %>%
    bindEvent(input$runModels)

  trainSet<-reactive(
    {create_train_set(Deaths_Model_Set, trainIndex())})%>%bindEvent(input$runModels)
  
  testSet<-reactive(
    {create_test_set(Deaths_Model_Set, trainIndex())})%>%bindEvent(input$runModels)
  
  #create models based on user input when runModels button is clicked
  MLRmodel <- reactive(
    {create_MLR(input$lmVarNames, trainSet())})%>%bindEvent(input$runModels)

  
  treeModel <- reactive(
    {create_tree(input$treeVars, trainSet())})%>%bindEvent(input$runModels)
  
  rfModel <- reactive(
    {create_forest(input$forestVars, trainSet())})%>%bindEvent(input$runModels)


  # #create tables of fit stats and plots for our models
  # #fit for MLR model
   output$selected_lm <- renderDT({datatable(MLRmodel()$results)})
     
     #modelVars[input$lmVarNames])
     
  # #fit for regression tree model
  output$selected_tree <- renderDT({datatable(treeModel()$results)})
   output$tree_plot <- renderPlot({
       plot(rfModel())
     })
  #fit for forest model
   output$selected_forest <- renderDT({
     datatable(rfModel()$results)
     })

############################Performance on test set
   
   
   output$testMLRFit<- renderText({
     print(testMLRResults())})
   
    testMLRResults<- reactive({
      MLRmod<-MLRmodel()
      testingSet<-testSet()
      pred<-round(predict(MLRmod, newdata=testingSet))
      fit<-postResample(pred, obs=testingSet$Covid_19_Deaths)
      return(fit)
    })%>%bindEvent(input$runModels)


    
###############Prediction Tab Code#################
   ######create reactive elements for use with ui for predict page
   

   
   #mmwrweek<- reactive({mmwrweek = input$week})%>%bindEvent()
   
   testPredictDataFrame<-reactive({
     data.frame(matrix(ncol=length(input$lmVarNames), nrow = 1))
   })%>%bindEvent(input$runModels)
   # 
   # 
   output$testPredictDataFrame<-renderDT({
     datatable(testPredictDataFrame())
   })
   
  predictInput <-
    reactive({
      data.frame(
        mmwrweek = input$week,
        mmwryear = input$year,
        jurisdiction = input$jurisdiction,
        Age_Group = input$age,
        Deaths_Total = input$deathTotal,
        Pneumonia_Deaths = input$pneumonia,
        Influenza_Deaths = input$flu
      )
    })%>%bindEvent(input$predict)
    
  deathPredict<- reactive({
      newdata <- predictInput()
      model <- rfModel()
      predictions <- round(predict(model, newdata))
  })%>%bindEvent(input$predict)
  
  #######This works to generate predictions
  output$predictionsTable <- renderText({
    predictions <- deathPredict()
    
    paste("Predicted Covid Deaths", predictions)
  })
  
  

  
  #reactive ui based on input for model variables in model fitting tab
   # weekReact<-reactive({
   #   if ("Report Week" %in% input$lmVarNames){
   #     selectInput(
   #       inputId = "week",
   #       label = "Report Week",
   #       choices = 1:52
   #     )
   #   }
   # })%>%bindEvent(input$predict)


  
  output$predictValueWeek<-renderUI({
    if ("Report Week"%in%(input$lmVarNames)){
      selectInput(
        inputId = "week",
        label = "Report Week",
        choices = 1:52
      )
    }
  })

  output$predictValueYear<-renderUI({
    if("Report Year"%in%(input$lmVarNames)){
      selectInput(
        inputId = "year",
        label = "Report Year",
        choices = c(2020, 2021, 2022))
    }
  }
  )
    
  output$predictValueJurisdiction<-renderUI({
    if("Jurisdiction"%in%(input$lmVarNames)){
    selectInput(
      inputId = "jurisdiction",
      label = "Jurisdiction",
      choices = state.name)
    }
  })
  
  output$predictValueAge<- renderUI({
    if("Age Group"%in%(input$lmVarNames))
    selectInput(
      inputId = "age",
      label = "Age Group",
      choices = c("All Ages",
                  "0-17 years",
                  "18-64 years",
                  "65 years and over")
    )
  })
  
output$predictValueDeathTotal<- renderUI({
  if("Total Deaths (All Causes)"%in%(input$lmVarNames))
  sliderInput(
    inputId = "deathTotal",
    label = "Total Deaths (All Causes)",
    min = 0,
    max = 100000,
    value = 0
  )
})

output$predictValuePneumonia<-renderUI({
  if("Pneumonia"%in%(input$lmVarNames))
  sliderInput(
    inputId = "pneumonia",
    label = "Pneumonia Deaths",
    min = 0,
    max = 10000,
    value = 0
  )
})

output$predictValueFlu<-renderUI({
  if("Influenza Deaths"%in%(input$lmVarNames))
  sliderInput(
    inputId = "flu",
    label = "Influenza Deaths",
    min = 0,
    max = 3000,
    value = 0
  )
})
  
  
  
  #########Data Page Code########################
# The code below relies on click input from the renderDT function to generate the subsetted dataset to return. If the user does not select anything then the full dataset is available for download.

#Creating a reactive data table that will allow users to click to select rows and columns. Users can also use the search feature in the data table to further subset their data. 
  CDC2<- Deaths_Data[,1:13]
  output$CDCdataset<- renderDT(CDC2, server=TRUE, selection=list(target='row+column'))
  
  
  output$test<- renderPrint({
    s<-input$CDCdataset_rows_selected
    c<-input$CDCdataset_columns_selected
    if (length(s)){
      cat('These rows were selected:\n\n')
      cat(s, sep = ', ')
    }
    if (length(c)){
      cat('These columns were selected:\n\n')
      cat(c, sep = ', ')
    }
    })

  

  #Storing user input from the datatable to be used to 
  output$testDownloadDF<- renderDT({
     r<-Deaths_Data[input$CDCdataset_rows_selected, , drop=FALSE]
     c<-r[, input$CDCdataset_columns_selected, drop=FALSE]
    datatable(c)
 })
  
    #Allow users to download the data set based on selections in the data table 
  output$downloadSelect<- downloadHandler(
    filename = function(){paste("CDC_Covid_Deaths_Selected", Sys.Date(), ".csv", sep = "")},
    content = function(file){
      r<-Deaths_Data[input$CDCdataset_rows_selected, , drop=FALSE]
      write.csv(r[, input$CDCdataset_columns_selected, drop=FALSE], file)}
    )
  
  #Allow users to download the full data set
  output$downloadAll<- downloadHandler(
    filename = function(){paste("CDC_Covid_Deaths", Sys.Date(), ".csv", sep = "")},
    content = function(file){
      write.csv(Deaths_Data, file)}
  )
  
  
  
})
