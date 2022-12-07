library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinycssloaders)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)


# Define UI for application that displays an about page, a data exploration page, a modeling page (with 3 tabs), and a data page.
shinyUI(fluidPage(
  dashboardPage(
    #Dashboard Title
    dashboardHeader( title = app_title),
    #Define sidebar items
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("home")),
        menuItem("Application", tabName = "app", icon = icon("biohazard"))
        )
      ),
    #Define the body of the app
    dashboardBody(
      tabItems(
        #First tab content
        tabItem(tabName = "about",
                fluidRow(
                  #box to contain picture
                  box(
                    title = h1(app_title),
                    width = 12,
                    img(src="CDC_Flu_Gif.gif")),
                  #box to contain description of purpose
                  box(
                    title = h1("Purpose of This App"),
                    width = 6,
                    solidHeader = TRUE,
                    status = "primary",
                    h4(app_purpose)
                    ),
                  #box to contain description of data
                  box(
                    title = h1("About the Data"),
                    width = 6,
                    solidHeader = TRUE,
                    status = "primary",
                    h4(about_data)
                    ),
                  #box to contain description of purpose of each tab
                  box(
                    title = h1("Purpose of Each Page/Tab"),
                    width = 6,
                    solidHeader = TRUE,
                    status = "primary",
                    h4(tab_purpose)
                    ),
                  ),
                ),
        #App layout
        tabItem(tabName = "app",
                tabsetPanel(
                  tabPanel("Data Exploration Page",
                           fluidRow(
                             #Create a column of width 3 to contain boxes for all user input
                             column(width = 3,
                                    box(title = "Numerical and Graphical Summaries Input", 
                                        width=NULL, 
                                        solidHeader = TRUE,
                                        status = "primary",
                                        selectInput("varDeath", 
                                                    label = "Variable to Summarize",
                                                    choices = names(CDCvars),
                                                    ),
                                        radioButtons("groups",
                                                     "Select Variable to Group Summaries By Or Choose to Look at Overall Summary Data for All Groups",
                                                     choices = c("All Groups", names(groupings)),
                                                     selected = "All Groups"
                                                     ),
                                        box(title = "Bar Graph Options",
                                            width=NULL, 
                                            solidHeader = TRUE,
                                            status = "primary",
                                            h4 = "You can create bar plots based on the variable",
                                            selectInput("varDeathBar", 
                                                        label = "Variable to Plot",
                                                        choices = names(CDCvars),
                                                        ),
                                            radioButtons("barplotType",
                                                         "Select Plot Type",
                                                         choices = c("By Year Only", "By Age Only", "By Year Grouped By Age", "By Age Grouped By Year")
                                                         )
                                            ),
                                        box(title = "Map Options",
                                            width=NULL, 
                                            solidHeader = TRUE,
                                            status = "primary")
                                        )
                                    ),
                             #Create a column of width 9 to contain boxes for our output based on user input
                             column(width = 9,
                                    box(title = "Numerical Summaries Table", 
                                        width=NULL, 
                                        solidHeader = TRUE,
                                        status = "primary",
                                        dataTableOutput("summary")
                                        ),
                                    box(title = "Bar Graph",
                                        width=NULL, 
                                        solidHeader = TRUE,
                                        status = "primary",
                                        plotOutput("barPlot")
                                        ),
                                    box(title = "Map",
                                        width=NULL, 
                                        solidHeader = TRUE,
                                        status = "primary",
                                        leafletOutput("mymap")
                                        )
                                    )
                             )
                           ),
                  #Modeling page tab
                  tabPanel("Modeling Page",
                           tabsetPanel(
                             #Modelling info tab
                             tabPanel("Modelling Information",
                                      box(title = h1("Linear Regression Model"),
                                          width = 4,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          h4(lm_info)
                                          ),
                                      box(title = h1("Tree Model"),
                                          width = 4,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          h4(tree_info)
                                          ),
                                      box(title = h1("Random Forest Model"),
                                          width = 4,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          h4(rf_info)
                                          )
                                      ),
                             #Model fitting tab allowing for user input
                             tabPanel("Model Fitting",
                                      fluidRow(
                                        #Creating a column of width 3 to contain all user inputs for model fitting
                                        column(width = 3,
                                               box(
                                                 title = h1("Model Fitting Options"),
                                                 width=NULL, 
                                                 solidHeader = TRUE,
                                                 status = "primary",
                                                 h4(model_options),
                                                 #Creating a slider input for proportion of data for training data set
                                                 box(
                                                   title = h1("Splitting Our Dataset"),
                                                   width=NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary",
                                                   sliderInput(
                                                     inputId = "split",
                                                     label = data_split,
                                                     min = 1,
                                                     max = 99,
                                                     value = 15,
                                                     post = "%")
                                                   ),
                                                 box(
                                                   title = h1("Linear Model Variables"),
                                                   width=NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary",
                                                   varSelectInput(
                                                     inputId = "lmVarNames",
                                                     label = "Variables:",
                                                     data = Deaths_Model_Set,
                                                     multiple = TRUE
                                                   )
                                                   # checkboxGroupInput(
                                                   #   inputId = "lmVars",
                                                   #   label = "Select all variables that you want to include in your linear model. If no variables are selected a linear model will be built using main effects for all variables with no interaction terms.",
                                                   #   choices = names(modelVars)
                                                   #   )
                                                   ),
                                                 box(
                                                   title = h1("Regression Tree Model Variables"),
                                                   width=NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary",
                                                   checkboxGroupInput(
                                                     inputId = "treeVars",
                                                     label = tree_vars,
                                                     choices = names(modelVars)
                                                     )
                                                   ),
                                                 box(
                                                   title = h1("Random Forest Model Variables"),
                                                   width=NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary",
                                                   checkboxGroupInput(
                                                     inputId = "forestVars",
                                                     label = rf_vars,
                                                     choices = names(modelVars)
                                                     )
                                                   ),
                                                 box(
                                                   title = h1("Click the button to run all models"),
                                                   width=NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary",
                                                   actionButton(
                                                     inputId = "runModels",
                                                     label = "RUN",
                                                     icon = icon("person-running"),
                                                     width = "100%"
                                                     )
                                                   )
                                                 )
                                               ),
                                        column(width = 9,
                                               box(
                                                 title = h1("Model Comparison"),
                                                 width = NULL,
                                                 solidHeader = TRUE,
                                                 status = "primary",
                                                 box(
                                                   title = "Table of Fit Statistics on Training Set",
                                                   width = NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary",
                                                   dataTableOutput(
                                                     outputId = "trainFitTable"
                                                     )
                                                   ),
                                                 box(
                                                   title = "Linear Model",
                                                   width = NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary"
                                                 ),
                                                 box(
                                                   title = "Regression Tree Model",
                                                   width = NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary"
                                                 ),
                                                 box(
                                                   title = "Random Forest Model",
                                                   width = NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary"
                                                 ),
                                                 box(
                                                   title = "Table of Fit Statistics on Test Set",
                                                   width = NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary",
                                                   dataTableOutput(
                                                     outputId = "testFitTable"
                                                     )
                                                   )
                                                 )
                                               )
                                        )
                                      ),
                             #Prediction Tab allowing for user input of data values to predict
                             tabPanel("Prediction")
                             )
                           ),
                  tabPanel("Data Page")
                  )
                )
        )
      )
    )
  )
  )