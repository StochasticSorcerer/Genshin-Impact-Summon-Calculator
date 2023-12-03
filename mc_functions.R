library(tidyverse)

featured <- function(){counter <<- counter + 1; pity <<- 0; previous <<- 1}
not_featured <- function(){pity <<- 0; previous <<- 0}
not_5star <- function(){pity <<- pity + 1}

summon50 <- function(){
  ifelse(runif(1,0,1) < 0.5, featured(), not_featured())
}

check_previous <- function(previous){
  ifelse(previous == 1, summon50(), featured())
}

summon <- function(previous){
  ifelse(runif(1,0,1) < 0.006, {check_previous(previous)}, not_5star())
}

summon_simulation <- function(n_summons, goal, starting_pity, previous_featured){
  
  counter <<- 0
  previous <<- previous_featured
  pity <<- starting_pity
  i <- 0
  con <- 0
  res <- c(0,0)
  
  while(i < n_summons | counter < goal){
    ifelse(pity == 79, check_previous(previous), summon(previous))
    i <- i + 1
    if(i == n_summons){res[1] <- counter}
    if(con == 0 & counter == goal){res[2] <- i; con <- 1}
  }
  
  return(res)
}

monte_carlo <- function(n_summons, goal, starting_pity, previous_featured, n_samples){
  
  samples <- map(.x = rep(0, n_samples), .f = ~summon_simulation(n_summons, goal, starting_pity, previous_featured)) %>%
    unlist() %>%
    matrix(nrow = n_samples, byrow = T) %>%
    data.frame() %>%
    arrange(X2)
  
  return(samples)
}