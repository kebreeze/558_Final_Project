#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #Summary data tables based on user inputs
  output$summary<- DT::renderDataTable(
    {get_summary(input$varDeath, input$groups)}, 
    class="cell-border stripe", 
    caption = as.character(input$varDeath)
    )

  #Bar Plots based on user inputs
  output$barPlot<- renderPlot({

    if(input$plot == "By Year Only"){
      ggplot(Deaths_Data, aes(x=mmwryear, y=Deaths_Data[,CDCvars[input$varDeathBar]])) + 
        geom_col() +
        labs(
          x="Report Year",
          y=input$varDeathBar
        )
      
    } else if(input$plot == "By Age Only"){
      ggplot(Deaths_Data, aes(x=Age_Group, y=Deaths_Data[,CDCvars[input$varDeathBar]])) + 
        geom_col() +
        labs(
          x="Age Group",
          y=input$varDeathBar
        )
      
    } else if(input$plot == "By Year Grouped By Age"){
      ggplot(Deaths_Data, aes(x=mmwryear, y=Deaths_Data[,CDCvars[input$varDeathBar]])) + 
        geom_col(aes(fill = Age_Group), position = "dodge") + 
        labs(
          x="Report Year",
          y=input$varDeathBar
        ) +
        scale_fill_discrete(name="Age Group")
      
    } else if(input$plot == "By Age Grouped By Year"){
      ggplot(Deaths_Data, aes(x=Age_Group, y=Deaths_Data[,CDCvars[input$varDeathBar]])) + 
        geom_col(aes(fill = mmwryear), position = "dodge") + 
        labs(
          x="Age Group",
          y=input$varDeathBar
      ) +
        scale_fill_discrete(name="Report Year")
    }
  })
  
  output$mymap<- renderLeaflet({
    mapStates<- map("state", fill = TRUE, plot = FALSE)
    
    leaflet(data = mapStates)%>%
      addTiles() %>%
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
  })
    # output$distPlot <- renderPlot({
    # 
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # 
    # })

})
