library(shiny)
library(babynames)
library(tidyverse)

babynames$year <- as.integer(babynames$year)

ui <- fluidPage(
  titlePanel("Baby Names"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1880, max = 2014, value = c(1900, 2000)),
      textInput("nameInput", "Name"),
      radioButtons("sexID", "Sex",  choices = c("Female only", "Male only", "Both"), selected = "Both")),
    mainPanel(
      plotOutput("main_plot"),
      tableOutput("results")
    )
  )
)

server <- function(input, output, session) {
  reduced_df <- reactive({
    sex_vec <- switch(input$sexID,
                      `Female only` = "F",
                      `Male only` = "M",
                      Both = c("F", "M")
    )
    filter(
      babynames, 
      name == input$nameInput, 
      year >= input$yearInput[1] & year <= input$yearInput[2], 
      sex %in% sex_vec 
    )
  })
  output$main_plot <- renderPlot({
    ggplot(data = reduced_df(), aes(year, n, colour = sex)) + 
      scale_color_manual(values=c("M" = "green", "F" = "red")) +
      geom_line() + ggtitle(input$nameInput)
  })
  output$results <- renderTable({ 
    reduced_df() %>% dplyr::select(-prop) %>%
      spread(sex, n, fill="0")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)