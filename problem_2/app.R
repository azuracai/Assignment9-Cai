library(shiny)
library(tidyverse)
library(gapminder)
# range(gapminder$year)


ui <- shinyUI(fluidPage(
  
  titlePanel("Gapminder"),
  
  sidebarLayout(
    # selecting name of a country from a pull-down menu
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1952, max = 2007, value = c(1960, 2000)),
      selectInput("countryInput", "Choose a country:", choices=gapminder$country)
    ),
    
    mainPanel(
      plotOutput("scatterplot")
    )
  )
))


server <- shinyServer(function(input, output){
  
  output$scatterplot <- renderPlot({
    reduced_df <- reactive({
      
      filter(
        gapminder,
        country == input$countryInput,
        year >= input$yearInput[1] & year <= input$yearInput[2]
      )
    })
    
    output$scatterplot <- renderPlot({
      ggplot(data = reduced_df(), aes(log10(gdpPercap), lifeExp)) + 
        geom_point() + geom_smooth() + ggtitle(input$countryInput)
    })
    
  })
})


# Run the application 
shinyApp(ui = ui, server = server)

