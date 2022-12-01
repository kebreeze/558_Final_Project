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
    #add title
    dashboardHeader( title = "US Respiratory Illness Dashboard"),
    #Define sidebar items
    dashboardSidebar(sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("home")),
      menuItem("Application", tabName = "app", icon = icon("biohazard"))
  )),
  
  #Define the body of the app
  dashboardBody(
    tabItems(
      #First tab content
      tabItem(tabName = "about",
              fluidRow(
                #box to contain picture
                box(
                  title = h1("Interactive Respiratory Illnesses Dashboard"),
                  width = 12,
                  img(src="CDC_Flu_Gif.gif")),
                #box to contain description of purpose
                box(
                  title = h1("Purpose of This App"),
                  width = 6,
                  solidHeader = TRUE,
                  status = "primary",
                  h4("This will be a description of the purpose of the app")
                  ),
                #box to contain description of data
                box(
                  title = h1("About the Data"),
                  width = 6,
                  solidHeader = TRUE,
                  status = "primary",
                  h4("Briefly discuss the data and its source - providing a link 
                     to more information about the data")
                  ),
                #box to contain description of purpose of each tab
                box(
                  title = h1("Purpose of Each Page/Tab"),
                  h4("Tell the user the purpose of each tab (page) of the app")
                  ),
                ),
              ),
      #App layout
      tabItem(tabName = "app",
              tabsetPanel(
                tabPanel("Data Exploration Page",
                         box(
                           leafletOutput("mymap")
                         )),
                tabPanel("Modeling Page",
                         tabsetPanel(
                           tabPanel("Modelling Information",
                                    box(
                                      title = h1("Linear Regression Model"),
                                      width = 4,
                                      solidHeader = TRUE,
                                      status = "primary",
                                      h4("You should explain these three modeling approaches, 
                                         the benefits of each, and the drawbacks of each. You 
                                         should include some type of math type in the explanation 
                                         (you’ll need to include mathJax).")
                                    ),
                                    box(
                                      title = h1("Tree Model"),
                                      width = 4,
                                      solidHeader = TRUE,
                                      status = "primary",
                                      h4("You should explain these three modeling approaches, 
                                         the benefits of each, and the drawbacks of each. You 
                                         should include some type of math type in the explanation 
                                         (you’ll need to include mathJax).")
                                    ),
                                    box(
                                      title = h1("Random Forest Model"),
                                      width = 4,
                                      solidHeader = TRUE,
                                      status = "primary",
                                      h4("You should explain these three modeling approaches, 
                                         the benefits of each, and the drawbacks of each. You 
                                         should include some type of math type in the explanation 
                                         (you’ll need to include mathJax).")
                                      )
                                    ),
                           tabPanel("Model Fitting",
                                    ),
                           tabPanel("Prediction")
                         )),
                tabPanel("Data Page")
              ))
    )
  )
)
))