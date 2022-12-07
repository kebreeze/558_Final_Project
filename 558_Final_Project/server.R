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
  output$barPlot<- renderPlot(
    {get_bar_plot(input$varDeathBar, input$barplotType)})
  
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
