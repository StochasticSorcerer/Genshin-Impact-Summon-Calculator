library(shiny)
library(tidyverse)
library(shinycssloaders)
library(plotly)
library(bsicons)
library(bslib)

source('mc_functions.R')

ui <- page_sidebar(
  title = 'Genshin Impact Summon Calculator',
  sidebar = sidebar(
    title = 'Settings',
    width = 400,
    textInput('summons', tooltip(p('Summons', bs_icon('info-circle')), 'The amount of summons you want to perform. Each summon is equivalent to 160 Primogems or 1 Intertwined Fate.'), value = '80'),
    sliderInput('pity', tooltip(p('Pity Count', bs_icon('info-circle')), 'The amount of summons since your last 5 star. After 79 consecutive summons without a 5-star, the next summon (80th) is guaranteed to be a 5-star.'), min = 0, max = 79, value = 0),
    selectInput('featured', tooltip(p('Previous 5-Star', bs_icon('info-circle')), 'If your previous 5-star was a featured unit, the next 5-star has a 50% chance to be featured. If your previous 5-star was NOT a featured unit, the next 5-star is guaranteed to be featured.'), choices = c('Non-Featured' = 0, 'Featured' = 1),selected = 'No'),
    textInput('goal', tooltip(p('5-Star Goal', bs_icon('info-circle')), 'How many of the featured unit do you want? The highest you should aim for is 7 to max out your character at constellation level 6.'), value = '1'),
    sliderInput('sims', tooltip(p('Simulations', bs_icon('info-circle')), 'The amount of time to simulate the above settings. Increasing this will make the program run longer but yield more accurate results.'), min = 100, max = 10000, value = 1000)
  ),
  mainPanel(
    width = 12,
    tabsetPanel(
      type = 'tabs',
      tabPanel(
        'Dashboard',
        width = 12,
        layout_columns(
          fill = F,
          height = '140px',
          value_box(
            title = tooltip(p('Chance of Reaching the Goal', bs_icon('info-circle')), "This is the probability that you will reach your goal given the parameters inputted"),
            value = withSpinner(textOutput('prob')),
            showcase = icon('dice')
          ),
          value_box(
            title = tooltip(p('Expected Summons', bs_icon('info-circle')), "This is the amount of summons you are expected to use to reach the goal specified using the parameters inputted. This is an expected value, not a guarantee."),
            value = withSpinner(textOutput('expected_summons')),
            showcase = icon('hat-wizard')
          ),
          value_box(
            title = tooltip(p('Expected Featured 5-Stars', bs_icon('info-circle')), "This is the amount of the featured 5-star unit you are expected to summon using the parameters inputted. This is an expected value, not a guarantee."),
            value = withSpinner(textOutput('expected_5star')),
            showcase = bs_icon('stars')
          )
        ),
        layout_columns(
          fill = F,
          height = '500px',
          card(
            card_title(tooltip(p('Distribution of Summons Needed', bs_icon('info-circle')), "Given the parameters inputted, this is the distribution of summons needed to reach your goal. This graph is designed to give more information than the statistics given above.")),
            withSpinner(plotlyOutput('plot'))
          ),
          card(
            card_title(tooltip(p('Distribution of Featured 5-Stars', bs_icon('info-circle')), "Given the parameters inputted, this is the distribution of the amount of featured 5-stars summoned. This graph is designed to give more information than the statistics given above.")),
            withSpinner(plotlyOutput('plot2'))
          )
        )
      ),
      tabPanel(
        'About',
        width = 12,
        card(
          card_title('The Developer'),
          p('I am Luke Andrade, currently a senior in college double majoring in statistics and mathematics. I am an aspiring data scientist and quantitative researcher who loves applied and theoretical probability. In my freetime I enjoy learning about financial mathematics and stock trading, programming new tools such as this one, and playing videogames with my friends. My other ongoing projects which will most likely not be made publically available include...'),
          p('Blackjack Optimization Tool: A tool that optimizes gameplay by suggesting the optimal move based on the cards that have been drawn. This project will include a heuristic approach via automated card counting and a probabilistic approach to optimal decision making. Additional elements may include a UI and a computer vision model to automatically read cards that are drawn from the screen.'),
          p('Algorithmic Programming: Currently learning about the stock market and trading strategies. Developing a program to automatically trade stocks and backtest new strategies using an API provided by a brokerage. Will integrate machine learning or deep learning models into strategies. The goal with this is to learn, not a failed attempt at getting rich fast!'),
          p('Feel free to connect with me on ', a(href = 'https://www.linkedin.com/in/landrade0228/', 'LinkedIn'))
        ),
        card(
          card_title('Monte Carlo Simulation / Law of Large Numbers'),
          p('This application is built on real-time Monte Carlo simulated data, thus each simulation will yield different results. The statistics generated in the dashboard are not exact but they are good estimators of the actual values. This is backed by the Law of Large Numbers.'),
          p('Suppose you flip a coin 8 times. You would expect to see 4 heads and 4 tails. However, this is not always the case. Sometimes you will get 2 heads and 6 tails which gives you a 25% chance of getting heads. Now if you flip a coin 1000 times, the amount of heads and tails you get will most likely be more even and your probability of seeing heads will be closer to 50% which is what you should expect. That in essence is the Law of Large Numbers. As you take more samples, you approach the true mean or probability.'),
          p('Monte Carlo simulations are based on this law. I coded in the logic for summoning in Genshin Impact and then this program simulates it many times to estimate the probabilities and expected values displayed in the dashboard tab.')
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$plot_name1 <- renderText(paste0('Chance of Getting ', input$goal,' Featured 5 Stars'))
  
  output$plot_name2 <- renderText(paste0('Distribution of Summoned Featured 5 Stars Using ', input$summons, ' Summons'))
  
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
      geom_step(color = '#3d80c6') +
      theme_classic()
    
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
      geom_bar(stat = 'identity', fill = '#3d80c6') +
      theme_classic()
    
    ggplotly(x) %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'x unified',
             dragmode = F,
             clickmode = F)
  })
  

  output$prob <- renderText(paste0(round(probability() * 100, 2), '%'))

  output$expected_5star <- renderText(round(expectation()[1,1], 2))
  
  output$expected_summons <- renderText(round(expectation()[1,7], 2))
  
  output$plot <- renderPlotly({plot()})
  
  output$plot2 <- renderPlotly(plot2())
}

shinyApp(ui = ui, server = server)
