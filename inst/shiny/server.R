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
shinyServer(function(input, output) {

  output$corrPlot <- plotly::renderPlotly({

    fig <- plot_score_ability_corr(data = data, fit = fit)
    fig
  })

  output$plot <- renderPlot({

    event <- event_data("plotly_click", source = "scoreAbilityCorr")
    req(event)
    print(event$pointNumber + 1)
    pattern <- data[event$pointNumber + 1,]
    create_pattern_info(pattern = pattern, fit = fit)
  })

})
