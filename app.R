library(shiny)
library(tidyverse)
library(shinycssloaders)
library(plotly)

source('mc_functions.R')

ui <- fluidPage(

    titlePanel('Genshin Impact Summon Calculator'),

    sidebarLayout(
        sidebarPanel(
          textInput('summons', 'How many summons?', value = '80'),
          sliderInput('pity', 'What is your current pity?', min = 0, max = 79, value = 0),
          selectInput('featured', 'Was your last 5 star a featured unit?', choices = c('No' = 0, 'Yes' = 1),selected = 'No'),
          textInput('goal', 'How many of the featured unit do you want?', value = '1'),
          sliderInput('sims', 'How many simulations?', min = 100, max = 10000, value = 1000),
          h4('Increasing this will make the program run longer but yield more accurate results.')
        ),
        
        mainPanel(
          withSpinner(textOutput('prob')),
          tags$head(tags$style('#prob{font-size: 20px; color: #000000}')),
          br(),
          withSpinner(textOutput('expected_5star')),
          tags$head(tags$style('#expected_5star{font-size: 20px; color: #000000}')),
          br(),
          withSpinner(textOutput('confint_5star')),
          tags$head(tags$style('#confint_5star{font-size: 20px; color: #000000}')),
          br(),
          withSpinner(textOutput('expected_summons')),
          tags$head(tags$style('#expected_summons{font-size: 20px; color: #000000}')),
          br(),
          withSpinner(textOutput('confint_summons')),
          tags$head(tags$style('#confint_summons{font-size: 20px; color: #000000}')),
          br(), br(),
          withSpinner(plotlyOutput('plot')),
          withSpinner(plotlyOutput('plot2'))
        )
    )
)

server <- function(input, output) {
  
  data <- reactive({monte_carlo(as.integer(input$summons), as.integer(input$goal), as.integer(input$pity), as.integer(input$featured), as.integer(input$sims))})
  
  probability <- reactive({
    x <- data() %>% filter(X1 >= as.integer(input$goal)) %>% nrow()
    x / as.integer(input$sims)
    })
  
  expectation <- reactive({
    data() %>% summarise(mean = mean(X1), sd = sd(X1), n = n(), se = sd / sqrt(n), lb = mean - 1.645*se, ub = mean + 1.645*se,
                         exp_summons = mean(X2), sd_summons = sd(X2), se_summ = sd_summons / n, summ_lb = exp_summons - 1.645*se_summ, summ_ub = exp_summons + 1.645*se_summ)
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
  
  plot2 <- reactive({
    data2 <- data() %>%
      group_by(X1) %>%
      summarise(Probability = n() / nrow(data())) %>%
      ungroup() %>%
      mutate('Featured 5 Stars' = factor(X1))
    
    x <- ggplot(data2 , aes(x = `Featured 5 Stars`, y = Probability)) +
      geom_bar(stat = 'identity', fill = 'black') +
      theme_bw() +
      labs(title = paste0('Distribution of Summoned Featured 5 Stars Using ', input$summons, ' Summons'))
    
    ggplotly(x) %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'x unified',
             dragmode = F,
             clickmode = F)
  })
  
  output$prob <- renderText(paste0('Probability of At Least ', input$goal, ' Featured 5 Stars: ', round(probability() * 100, 2), '%'))
  
  output$expected_5star <- renderText(paste0('Expected Featured 5 Stars in ', input$summons, ' Summons: ', round(expectation()[1,1], 2)))
  
  output$confint_5star <- renderText(paste0('90% Confidence Interval: (', round(expectation()[1,5], 2), ', ', round(expectation()[1,6], 2),')'))
  
  output$expected_summons <- renderText(paste0('Expected Summons for ', input$goal, ' Featured 5 Stars: ', round(expectation()[1,7], 2)))
  
  output$confint_summons <- renderText(paste0('90% Confidence Interval: (', round(expectation()[1,10], 2), ', ', round(expectation()[1,11], 2),')'))
  
  output$plot <- renderPlotly({plot()})
  
  output$plot2 <- renderPlotly(plot2())
}

shinyApp(ui = ui, server = server)
