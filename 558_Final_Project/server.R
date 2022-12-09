#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

# source(".Renviron")
# source("text.R")
# source("dict.R")
# source("functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #########Data Exploration Page Code###########
  ##Summary data tables based on user inputs##
  output$summary<- DT::renderDataTable(
    {create_summary(input$varDeath, input$groups)}, 
    class="cell-border stripe", 
    caption = as.character(input$varDeath)
  )
  
  ##Bar Plots based on user inputs##
  output$barPlot<- renderPlot(
    {create_bar_plot(input$varDeathBar, input$barplotType)})
  

  
  ##########Model Fitting Page Code################
  ##User input for variables and actionButton##
  observeEvent(input$runModels,{
    
    #create index, train and test sets based on user input for percentage to include in the training set
    trainIndex<-create_split(input$split)
    trainSet<-create_train_set(Deaths_Model_Set, trainIndex)
    testSet<-create_test_set(Deaths_Model_Set,trainIndex)
    #create models based on user input
    MLRmodel<-create_MLR(input$lmVarNames, trainSet)
    treeModel<-create_tree(input$treeVars, trainSet)
    rfModel<-create_forest(input$forestVars, trainSet)
    #create tables of fit stats and plots for our models
    #fit for MLR model
    output$selected_lm<- DT::renderDataTable(
      (MLRmodel$results)
    )
    #fit for regression tree model
    output$selected_tree<- DT::renderDataTable(
      (treeModel$results)
    )
    output$tree_plot<- renderPlot({
      plot(treeModel)
    })
    #fit for forest model
    output$selected_forest<- DT::renderDataTable(
      (rfModel$results)
    )
    
    output$testFitTable<- DT::renderDataTable({
      create_test_stats(MLRmodel, testSet)
    })
#    (input$lmVarNames)
    # print(input$treeVars)
    # print(paste((input$forestVars), collapse = "+"))
  })
  # output$selected_lm<- renderText({
  #   paste("You have selected", input$lmVarNames) 
  # })
  
  #########Data Page Code########################
  ##Allow users to scroll through dataset
  output$CDCdataset<- DT::renderDataTable(
    Deaths_Data,
    options = list(scrollX=TRUE)
  )
  #Allow users to download the dataset
  output$download<- downloadHandler(
    filename = function(){paste("CDC_Covid_Deaths", Sys.Date(), ".csv", sep = "")},
    content = function(file){write.csv(Deaths_Data, file)}
    )
})
