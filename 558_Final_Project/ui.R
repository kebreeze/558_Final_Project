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
    dashboardHeader( title = app_title_text),
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
                    title = h1(app_title_text),
                    width = 12,
                    img(src="CDC_Flu_Gif.gif")),
                  #box to contain description of purpose
                  box(
                    title = h1("Pursose of this App"),
                    width = 6,
                    solidHeader = TRUE,
                    status = "primary",
                    h4(app_purpose_text)
                    ),
                  #box to contain description of data
                  box(
                    title = h1("About the Data"),
                    width = 6,
                    solidHeader = TRUE,
                    status = "primary",
                    h4(about_data_text)
                    ),
                  #box to contain description of purpose of each tab
                  box(
                    title = h1("Purpose of Each Tab"),
                    width = 6,
                    solidHeader = TRUE,
                    status = "primary",
                    h4(tab_purpose_text)
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
                                        dataTableOutput("summary")%>%
                                          withSpinner()
                                        ),
                                    box(title = "Bar Graph",
                                        width=NULL, 
                                        solidHeader = TRUE,
                                        status = "primary",
                                        plotOutput("barPlot")%>%
                                          withSpinner(hide.ui = FALSE)
                                        ),
                                    box(title = "Scatter Plot",
                                        width=NULL, 
                                        solidHeader = TRUE,
                                        status = "primary",
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
                                          withMathJax(helpText("Linear regression provides a relatively simple way to predict a quantitative response. In a simple linear regression we use a single predictor $X$ to predict a response $Y$. In a simple linear model we have two unknown constants, $Beta_0$ represents the intercept and $Beta_1$ represents the slope. $Beta_0$ is the expected value of $Y$ when $X=0$. $Beta_1$ is the average change in $Y$ that is associated with an increase of one-unit of $X$. In linear regression we use our training data to produce estimated values for $Beta_0$ and $Beta_1$ which can then be used to make predictions on our test data.")),
                                          withMathJax(helpText("Some math here $$\\alpha+\\beta$$"))
                                          ),
                                      box(title = h1("Tree Model"),
                                          width = 4,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          h4(tree_info_text)
                                          ),
                                      box(title = h1("Random Forest Model"),
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
                                        column(width = 4,
                                               box(
                                                 title = h2("Model Fitting Options"),
                                                 width=NULL, 
                                                 solidHeader = TRUE,
                                                 status = "primary",
                                                 h4(model_options_text),
                                                 #Creating a slider input for proportion of data for training data set
                                                 box(
                                                   title = "Splitting Our Dataset",
                                                   width=NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary",
                                                   data_split_text,
                                                   sliderInput(
                                                     inputId = "split",
                                                     label = "Percent to Use in Training",
                                                     min = 1,
                                                     max = 99,
                                                     value = 15,
                                                     post = "%")
                                                   ),
                                                 box(
                                                   title = "Linear Model Variables",
                                                   width=NULL,
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
                                                   width=NULL,
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
                                                   width=NULL,
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
                                        column(width = 8,
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
                                                   dataTableOutput(
                                                     outputId = "trainFitTable"
                                                     )
                                                   ),
                                                 box(
                                                   title = "Linear Model",
                                                   width = NULL,
                                                   solidHeader = TRUE,
                                                   status = "primary",
                                                   dataTableOutput(
                                                     outputId = "selected_lm")
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
                  tabPanel("Data Page",
                           fluidRow(
                             column(width = 3,
                                    box(
                                      title = "View, Subset and Download the Data",
                                      width = NULL,
                                      solidHeader = TRUE,
                                      status = "primary",
                                      
                                      downloadButton(
                                        outputId = "download"
                                      )
                                      ),
                                    ),
                             column(width = 9,
                                    # dataTableOutput(
                                    #   outputId = "CDCdataset",
                                    #   width = "95%")
                                    box(
                                      title = "CDC Covid Deaths Dataset",
                                      width = NULL,
                                      solidHeader = TRUE,
                                      status = "primary",
                                      dataTableOutput(
                                        outputId = "CDCdataset"
                                        )
                                      )
                                    )
                             )
                           )
                  )
                )
        )
      )
    )
  )
)