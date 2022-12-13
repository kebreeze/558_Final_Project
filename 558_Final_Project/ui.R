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
                               lm_info_text1,
                               helpText('Where: \\(Y_i\\) = your continuous target or response variable,
                                          \\(x1...\\) = your predictor variable(s), \\(B_0\\) = the intercept, and \\(B_1...\\) = the slope associated with the predictor variable(s).'),
                               lm_info_text2
                             ),
                             box(
                               title = h1("Tree Model"),
                               width = 4,
                               solidHeader = TRUE,
                               status = "primary",
                               tree_info_text
                             ),
                             box(
                               title = h1("Random Forest Model"),
                               width = 4,
                               solidHeader = TRUE,
                               status = "primary",
                               rf_info_text
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
                                            title = "Linear Model Performance on Training Set",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            DTOutput("selected_lm")%>%
                                              withSpinner()
                                          ),
                                          box(
                                            title = "Regression Tree Model Performance on Training Set",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            DTOutput(outputId = "selected_tree")%>%
                                              withSpinner(),
                                            plotOutput(outputId = "tree_plot")%>%
                                              withSpinner()
                                          ),
                                          box(
                                            title = "Random Forest Model Performance on Training Set",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            DTOutput(outputId = "selected_forest")%>%
                                              withSpinner()
                                          ),
                                          box(
                                            title = "Comparing Fit Statistics on Test Set For Our Models",
                                            width = NULL,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            h3("MLR Model Performance on Test Set"),
                                            DTOutput(outputId = "testMLRFit")%>%
                                              withSpinner(),
                                            h3("Regression Tree Model Performance on Test Set"),
                                            DTOutput(outputId = "testTreeFit")%>%
                                              withSpinner(),
                                            h3("Random Forest Model Performance on Test Set"),
                                            DTOutput(outputId = "testRfFit")%>%
                                              withSpinner()
                                          )
                                        )
                                      )
                                    )),
                           #Prediction Tab allowing for user input of data values to predict for linear model
                           tabPanel("Prediction",
                                    fluidRow(
                                      #Input prediction values
                                      column(
                                        width = 3,
                                        box(
                                          title="Make Predictions Using Your MLR Model!",
                                          h4("What values would you like to use for your predictions?"),
                                          width = NULL,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          uiOutput("predictAll"),
                                          uiOutput("predictValueWeek"),
                                          uiOutput("predictValueYear"),
                                          uiOutput("predictValueJurisdiction"),
                                          uiOutput("predictValueAge"),
                                          uiOutput("predictValueDeathTotal"),
                                          uiOutput("predictValuePneumonia"),
                                          uiOutput("predictValueFlu"),
                                          actionButton(
                                            inputId = "predict",
                                            label = "Make Prediction",
                                            width = "100%",
                                            class = "btn-info",
                                            icon = icon("magnifying-glass-chart")
                                          )
                                        ),
                                        # box(
                                        #   title = "Predict Using Your MLR Model",
                                        #   width = NULL,
                                        #   solidHeader = TRUE,
                                        #   status = "primary",
                                        #   selectInput(
                                        #     inputId = "week",
                                        #     label = "Report Week",
                                        #     choices = 1:52
                                        #   ),
                                        #   selectInput(
                                        #     inputId = "year",
                                        #     label = "Report Year",
                                        #     choices = c(2020, 2021, 2022)
                                        #   ),
                                        #   selectInput(
                                        #     inputId = "jurisdiction",
                                        #     label = "Jurisdiction",
                                        #     choices = state.name
                                        #   ),
                                        #   selectInput(
                                        #     inputId = "age",
                                        #     label = "Age Group",
                                        #     choices = c("All Ages",
                                        #                 "0-17 years",
                                        #                 "18-64 years",
                                        #                 "65 years and over")
                                        #   ),
                                        #   sliderInput(
                                        #     inputId = "deathTotal",
                                        #     label = "Total Deaths (All Causes)",
                                        #     min = 0,
                                        #     max = 100000,
                                        #     value = 0
                                        #   ),
                                        #   sliderInput(
                                        #     inputId = "pneumonia",
                                        #     label = "Pneumonia Deaths",
                                        #     min = 0,
                                        #     max = 10000,
                                        #     value = 0
                                        #   ),
                                        #   sliderInput(
                                        #     inputId = "flu",
                                        #     label = "Influenza Deaths",
                                        #     min = 0,
                                        #     max = 3000,
                                        #     value = 0
                                        #   )
                                        # 
                                        # )
                                      ),
                                      column(
                                        width = 9,
                                        box(
                                          title = "Values Used to Calculate Predictions for Covid (MLR Model)",
                                          width = NULL,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          DTOutput(outputId = "testPredictDataFrame"
                                                          )
                                        ),
                                        box(
                                          title = "Values Used to Calculate Predictions for Covid (MLR Model)",
                                          width = NULL,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          textOutput(outputId = "predictionsMessage"
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
                               h4("Search for terms, highlight rows, and highlight columns! When you are ready click the download selected data button below"),
                               downloadButton(
                                 outputId = "downloadSelect",
                                 label = "Download Selected Data",
                                 icon = icon("filter"),
                                 class = "btn-info",
                                 width = "100%"
                               ), 
                               br(),
                               br(),
                               h4("Just want the entire data set? Click the download all data button to download the full data set."),
                               downloadButton(
                                 outputId = "downloadAll",
                                 label = "Download All Data",
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
                               DTOutput(outputId = "CDCdataset")%>%
                                 withSpinner()
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
                                 outputId = "testDownloadDF")
                             )
                           )
                         ))
              ))
    ))
  )
))