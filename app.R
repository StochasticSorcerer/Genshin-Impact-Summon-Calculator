library(shiny)
library(tidyverse)
library(shinycssloaders)
library(plotly)

source('mc_functions.R')

ui <- fluidPage(

    titlePanel('Genshin Impact Summon Calculator'),

    sidebarLayout(
        sidebarPanel(
          textInput('summons', 'How many summons?', value = '1'),
          sliderInput('sims', 'How many simulation?', min = 100, max = 10000, value = 1000),
          sliderInput('pity', 'What is your current pity?', min = 0, max = 79, value = 0),
          selectInput('featured', 'Was your last 5 star a featured unit?', choices = c('No' = 0, 'Yes' = 1),selected = 'No'),
          textInput('goal', 'How many of the featured unit do you want?', value = '1')
        ),

        mainPanel(
          withSpinner(textOutput('prob')),
          tags$head(tags$style('#prob{font-size: 20px;}')),
          withSpinner(plotlyOutput('plot'))
        )
    )
)

server <- function(input, output) {
  
  data <- reactive({monte_carlo(as.integer(input$summons), as.integer(input$goal), as.integer(input$pity), as.integer(input$featured), as.integer(input$sims))})
  
  probability <- reactive({
    x <- data() %>% filter(X1 >= as.integer(input$goal)) %>% nrow()
    x / as.integer(input$sims)
    })
  
  plot <- reactive({
    data2 <- data.frame(x = unique(data()$X2),
                        y = ecdf(data()$X2)(unique(data()$X2))*length(data()$X2))
    
    data2$y <- scale(data2$y, center = min(data2$y), scale = diff(range(data2$y)))
    
    names(data2) <- c('Summons', 'Probability')
    
    x <- ggplot(data2, aes(Summons, Probability)) +
      geom_step() +
      theme_bw() +
      ggtitle(paste0('Chance of Getting ', input$goal,' Featured 5 Stars'))
    
    ggplotly(x) %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'x unified',
             dragmode = F,
             clickmode = F)
  })
  
  output$prob <- renderText(paste0('The chance that you get at least ', input$goal, ' featured 5 star units is ', probability() * 100, '%'))
  
  output$plot <- renderPlotly({plot()})
}

shinyApp(ui = ui, server = server)
