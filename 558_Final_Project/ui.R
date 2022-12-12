library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinycssloaders)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)

source("text.R")
source("dict.R")


# Define UI for application that displays an about page, a data exploration page, a modeling page (with 3 tabs), and a data page.
shinyUI(fluidPage(
  withMathJax(),
  dashboardPage(
    #Dashboard Title
    dashboardHeader(title = app_title_text),
    #Define sidebar items
    dashboardSidebar(sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("home")),
      menuItem(
        "Application",
        tabName = "app",
        icon = icon("head-side-cough")
      )
    )),
    #Define the body of the app
    dashboardBody(tabItems(
      #First tab content
      tabItem(tabName = "about",
              fluidRow(
                column(
                  width = 6,
                  #box to contain picture
                  box(
                    title = h1(app_title_text),
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    img(
                      src = "covid_pic.jpg",
                      height="50%",
                      width="50%"
                      )
                  ),
                  #box to contain description of purpose
                  box(
                    title = h1("Pursose of this App"),
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    h4(app_purpose_text)
                  )
                ),
                column(
                  width = 6,
                  #box to contain description of data
                  box(
                    title = h1("About the Data"),
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    h4(about_data_text)
                  ),
                  #box to contain description of purpose of each tab
                  box(
                    title = h1("Purpose of Each Tab"),
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    h4(tab_purpose_text)
                  )
                )
              )),
      #App layout
      tabItem(tabName = "app",
              tabsetPanel(
                tabPanel("Data Exploration Page",
                         fluidRow(
                           #Create a column of width 3 to contain boxes for all user input
                           column(
                             width = 3,
                             box(
                               title = "Numerical and Graphical Summaries Input",
                               width = NULL,
                               solidHeader = TRUE,
                               status = "primary",
                               selectInput("type",
                                           label = "What summaries would you like to see?",
                                           choices = c("All Basic Summaries", "Measures of Center", "Measures of Spread", "Counts")),
                               selectInput("varDeath",
                                           label = "Variable to Summarize",
                                           choices = names(CDCvars)),
                               radioButtons(
                                 "groups",
                                 "Select Variable to Group Summaries By Or Choose to Look at Overall Summary Data for All Groups",
                                 choices = c("All Groups", names(groupings)),
                                 selected = "All Groups"
                               )),
                               box(
                                 title = "Bar Graph Options",
                                 width = NULL,
                                 solidHeader = TRUE,
                                 status = "primary",
                                 h4 = "You can create bar plots based on the variable",
                                 selectInput("varDeathBar",
                                             label = "Variable to Plot",
                                             choices = names(CDCvars)),
                                 radioButtons(
                                   "barplotType",
                                   "Select Plot Type",
                                   choices = c(
                                     "By Year Only",
                                     "By Age Only",
                                     "By Year Grouped By Age",
                                     "By Age Grouped By Year"
                                   )
                                 )
                               ),
                               box(
                                 title = "Scatter Plot Options",
                                 width = NULL,
                                 solidHeader = TRUE,
                                 status = "primary",
                                 selectInput("scatterx",
                                   label = "Variable to Plot on X-Axis",
                                   choices = names(varLabels)
                                             ),
                                 selectInput("scattery",
                                             label = "Variable to Plot on Y-Axis",
                                             choices = names(varLabels)
                                 ),
                                 selectInput("scatteColor",
                                             label = "Variable to Color Points By",
                                             choices = names(varLabels)
                                 )
                               ),
                               box(
                                 title = "Histogram",
                                 width = NULL,
                                 solidHeader = TRUE,
                                 status = "primary",
                                 selectInput("histogram",
                                             label = "Variable to Use for Histogram",
                                             choices = names(histVars)
                               ),
                               sliderInput(
                                 inputId = "bin",
                                 label = "Bin Size",
                                 min = 1,
                                 max = 30000,
                                 value = 1000
                               )
                             )
                           ),
                           #Create a column of width 9 to contain boxes for our output based on user input
                           column(
                             width = 9,
                             box(
                               title = "Numerical Summaries Table",
                               width = NULL,
                               solidHeader = TRUE,
                               status = "primary",
                               DTOutput("summary") %>%
                                 withSpinner()
                             ),
                             box(
                               title = "Bar Graph",
                               width = NULL,
                               solidHeader = TRUE,
                               status = "primary",
                               plotOutput("barPlot") %>%
                                 withSpinner(hide.ui = FALSE)
                             ),
                             box(
                               title = "Scatter Plot",
                               width = NULL,
                               solidHeader = TRUE,
                               status = "primary",
                               plotOutput("scatterPlot")
                             ),
                             box(
                               title = "Histogram",
                               width = NULL,
                               solidHeader = TRUE,
                               plotOutput("histPlot")
                             )
                           )
                         )),
                #Modeling page tab
                tabPanel("Modeling Page",
                         tabsetPanel(
                           #Modelling info tab
                           tabPanel(
                             "Modeling Information",
                             box(
                               title = h1("Linear Regression Model"),
                               width = 4,
                               solidHeader = TRUE,
                               status = "primary",
                               helpText(
                                 "Linear regression provides a relatively simple way to predict a quantitative response. In a simple linear regression we use a single predictor $$X$$ to predict a response $Y$. In a simple linear model we have two unknown constants, $Beta_0$ represents the intercept and $Beta_1$ represents the slope. $Beta_0$ is the expected value of $Y$ when $X=0$. $Beta_1$ is the average change in $Y$ that is associated with an increase of one-unit of $X$. In linear regression we use our training data to produce estimated values for $Beta_0$ and $Beta_1$ which can then be used to make predictions on our test data. $$\\alpha+\\beta$$"
                               ),
                               helpText("Some math here $$\\alpha+\\beta$$")
                             ),
                             box(
                               title = h1("Tree Model"),
                               width = 4,
                               solidHeader = TRUE,
                               status = "primary",
                               h4(tree_info_text)
                             ),
                             box(
                               title = h1("Random Forest Model"),
                               width = 4,
                               solidHeader = TRUE,
                               status = "primary",
                               h4(rf_info_text)
                             )
                           ),
                           #Model fitting tab allowing for user input
                           tabPanel("Model Fitting",
                                    fluidRow(
                                      #Creating a column of width 3 to contain all user inputs for model fitting
                                      column(
                                        width = 4,
                                        box(
                                          title = h2("Model Fitting Options"),
                                          width = NULL,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          h4(model_options_text),
                                          #Creating a slider input for proportion of data for training data set
                                          box(
                                            title = "Splitting Our Dataset",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            data_split_text,
                                            sliderInput(
                                              inputId = "split",
                                              label = "Percent to Use in Training",
                                              min = 1,
                                              max = 99,
                                              value = 15,
                                              post = "%"
                                            )
                                          ),
                                          box(
                                            title = "Linear Model Variables",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            lm_vars_text,
                                            checkboxGroupInput(
                                              inputId = "lmVarNames",
                                              label = "Select Variables for MLR Model",
                                              choices = names(modelVars)
                                            )
                                          ),
                                          box(
                                            title = "Regression Tree Model Variables",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            tree_vars_text,
                                            checkboxGroupInput(
                                              inputId = "treeVars",
                                              label = "Select Variables for Regression Tree Model",
                                              choices = names(modelVars)
                                            )
                                          ),
                                          box(
                                            title = "Random Forest Model Variables",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            rf_vars_text,
                                            checkboxGroupInput(
                                              inputId = "forestVars",
                                              label = "Select Variables for Regression Tree Model",
                                              choices = names(modelVars)
                                            )
                                          ),
                                          box(
                                            title = "Click the button to run all models",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            actionButton(
                                              inputId = "runModels",
                                              label = "RUN",
                                              icon = icon("person-running"),
                                              width = "100%",
                                              class = "btn-info"
                                            )
                                          )
                                        )
                                      ),
                                      column(
                                        width = 8,
                                        box(
                                          title = h2("Model Comparison"),
                                          width = NULL,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          box(
                                            title = "Table of Fit Statistics on Training Set",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            DTOutput(outputId = "trainFitTable")
                                          ),
                                          box(
                                            title = "Linear Model Performance on Training Set",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            #DTOutput(outputId = "selected_lm")
                                            DTOutput("selected_lm")
                                          ),
                                          box(
                                            title = "Regression Tree Model Performance on Training Set",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            DTOutput(outputId = "selected_tree"),
                                            plotOutput(outputId = "tree_plot")
                                          ),
                                          box(
                                            title = "Random Forest Model Performance on Training Set",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            DTOutput(outputId = "selected_forest")
                                          ),
                                          box(
                                            title = "Table of Fit Statistics on Test Set",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            DTOutput(outputId = "testFitTable")
                                          )
                                        )
                                      )
                                    )),
                           #Prediction Tab allowing for user input of data values to predict for linear model
                           tabPanel("Prediction",
                                    fluidRow(
                                      #Input prediction values
                                      column(
                                        width = 8,
                                        box(
                                          title="What model would you like to use for your perdictions?",
                                          width = NULL,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          selectInput(
                                            inputId = "modelSelect",
                                            label = "Pick your model",
                                            choices = c("MLR Model", "Regression Tree Model", "Random Forest Model")
                                            ),
                                          uiOutput("predictValueWeek"),
                                          uiOutput("predictValueYear"),
                                          uiOutput("predictValueJurisdiction"),
                                          uiOutput("predictValueAge"),
                                          uiOutput("predictValueDeathTotal"),
                                          uiOutput("predictValuePneumonia"),
                                          uiOutput("predictValueFlu")
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
                                        width = 4,
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
                tabPanel("Data Page",
                         fluidRow(
                           column(
                             width = 3,
                             box(
                               title = "View, Subset and Download the Data",
                               width = NULL,
                               solidHeader = TRUE,
                               status = "primary",
                               downloadButton(
                                 outputId = "download",
                                 class = "btn-info",
                                 width = "100%"
                               )
                             )
                           ),
                           column(
                             width = 9,
                             box(
                               title = "CDC Covid Deaths Dataset",
                               width = NULL,
                               solidHeader = TRUE,
                               status = "primary",
                               DTOutput(outputId = "CDCdataset")
                             ),
                             box(
                               title = "Click the button to See Filtered Dataset",
                               width = NULL,
                               solidHeader = TRUE,
                               status = "primary",
                               actionButton(
                                 inputId = "seeFiltered",
                                 label = "filtered",
                                 icon = icon("person-running"),
                                 width = "100%",
                                 class = "btn-info"
                                 )
                               ),
                             box(
                               title = "CDC Covid Deaths Dataset",
                               width = NULL,
                               solidHeader = TRUE,
                               status = "primary",
                               verbatimTextOutput('test')
                             ),
                             box(
                               DTOutput(
                                 outputId = "testDF")
                             )
                           )
                         ))
              ))
    ))
  )
))