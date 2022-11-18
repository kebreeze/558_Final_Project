
library(shiny)
library(shinydashboard)

# Define UI for application that displays an about page, a data exploration page, a modeling page (with 3 tabs), and a data page.

dashboardPage(
  #add title
  dashboardHeader(title = "Temporary Title"),
  
  #Define sidebar items
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about"),
    menuItem("Application", tabName = "app")
  )),
  
  #Define the body of the app
  dashboardBody(
    tabItems(
      #First tab content
      tabItem(tabName = "about",
              fluidRow(
              #box to contain description of purpose
              box(
                title = h1("Purpose of This App"),
                h4("This will be a description of the purpose of the app")
              ),

              #box to contain description of data
              box(
                title = h1("About the Data"),
                h4("Briefly discuss the data and its source - providing a link to more information about the data")
              ),
              #box to contain description of purpose of each tab
              box(
                title = h1("Purpose of Each Page/Tab"),
                h4("Tell the user the purpose of each tab (page) of the app")
              ),
              #box to contain picture
              box(
                title = h1("Picture Related to the Data"),
                h4("Include a picture related to the data (for instance, if the data was about the world wildlife fund, you might include a picture of their logo)")
              ),
              ),
      ),
      #App layout
      tabItem(tabName = "app",
              tabsetPanel(
                tabPanel("Data Exploration Page"),
                tabPanel("Modeling Page",
                         tabsetPanel(
                           tabPanel("Maodelling Information"),
                           tabPanel("Model Fitting"),
                           tabPanel("Prediction")
                         )),
                tabPanel("Data Page")
              ))
    )
  )
)
