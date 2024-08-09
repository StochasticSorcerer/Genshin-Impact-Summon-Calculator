library(tidyverse)
library(microbenchmark)
library(qrng)
library(spacefillr)

featured <- function(){counter <<- counter + 1; pity <<- 0; previous <<- 1}
not_featured <- function(){pity <<- 0; previous <<- 0}
not_5star <- function(){pity <<- pity + 1}

prob <- function(pity){
  m <- 0.994 / 17
  b <- -73 * m
  p <- 0.006 + max(0, m*pity + b)
  return(p)
}

summon50 <- function(){
  ifelse(runif(1,0,1) < 0.5, featured(), not_featured())
}

check_previous <- function(previous){
  ifelse(previous == 1, summon50(), featured())
}

summon <- function(pity, previous, u){
  ifelse(u <= prob(pity), {check_previous(previous)}, not_5star())
}

# Normal Simulation

summon_simulation <- function(n_summons, goal, starting_pity, previous_featured){
  
  counter <<- 0
  previous <<- previous_featured
  pity <<- starting_pity
  i <- 0
  con <- 0
  res <- c(0,0)
  
  while(i < n_summons | counter < goal){
    u <- runif(1, 0, 1)
    
    summon(pity, previous, u)
    i <- i + 1
    if(i == n_summons){res[1] <- counter}
    if(con == 0 & counter == goal){res[2] <- i; con <- 1}
  }
  
  return(res)
}

# Antithetic Variates

summon_simulation_av <- function(n_summons, goal, starting_pity, previous_featured){
  
  counter <<- 0
  previous <<- previous_featured
  pity <<- starting_pity
  i <- 0
  con <- 0
  res <- c(0,0)
  
  while(i < n_summons | counter < goal){
    
    u <- runif(1, 0, 1)
    
    summon(pity, previous, u)
    i <- i + 1
    if(i == n_summons){res[1] <- counter}
    if(con == 0 & counter == goal){res[2] <- i; con <- 1}
    
    summon_av(pity, previous, 1-u)
    i <- i + 1
    if(i == n_summons){res[1] <- counter}
    if(con == 0 & counter == goal){res[2] <- i; con <- 1}
  }
  
  return(res)
}

# Quasi Random Numbers

summon_simulation_qrn <- function(n_summons, goal, starting_pity, previous_featured){
  
  counter <<- 0
  previous <<- previous_featured
  pity <<- starting_pity
  i <- 0
  con <- 0
  res <- c(0,0)
  
  qrn <- sobol(2520, randomize = 'Owen')
  
  while(i < n_summons | counter < goal){
    
    u <- qrn[i]
    
    summon(pity, previous, u)
    i <- i + 1
    if(i == n_summons){res[1] <- counter}
    if(con == 0 & counter == goal){res[2] <- i; con <- 1}
  }
  
  return(res)
}

# Quasi Random Numbers + Antithetic Variates

summon_simulation_qrn_av <- function(n_summons, goal, starting_pity, previous_featured){
  
  counter <<- 0
  previous <<- previous_featured
  pity <<- starting_pity
  i <- 0
  con <- 0
  res <- c(0,0)
  
  qrn <- sobol(goal * 180, randomize = 'Owen')
  
  while(i < n_summons | counter < goal){
    
    u <- qrn[(i+1)/2]
    
    summon(pity, previous, u)
    i <- i + 1
    if(i == n_summons){res[1] <- counter}
    if(con == 0 & counter == goal){res[2] <- i; con <- 1}
    
    summon_av(pity, previous, 1-u)
    i <- i + 1
    if(i == n_summons){res[1] <- counter}
    if(con == 0 & counter == goal){res[2] <- i; con <- 1}
  }
  
  return(res)
}

monte_carlo <- function(n_summons, goal, starting_pity, previous_featured, n_samples, method = 'normal'){
  
  if(method == 'normal'){
    samples <- map(.x = rep(0, n_samples), .f = ~summon_simulation(n_summons, goal, starting_pity, previous_featured)) %>%
      unlist() %>%
      matrix(nrow = n_samples, byrow = T) %>%
      data.frame() %>%
      arrange(X2)
    
    return(samples)
  }
  else if(method == 'av'){
    samples <- map(.x = rep(0, n_samples), .f = ~summon_simulation_av(n_summons, goal, starting_pity, previous_featured)) %>%
      unlist() %>%
      matrix(nrow = n_samples, byrow = T) %>%
      data.frame() %>%
      arrange(X2)
    
    return(samples)
  }
  else if(method == 'qrn'){
    samples <- map(.x = rep(0, n_samples), .f = ~summon_simulation_qrn(n_summons, goal, starting_pity, previous_featured)) %>%
      unlist() %>%
      matrix(nrow = n_samples, byrow = T) %>%
      data.frame() %>%
      arrange(X2)
    
    return(samples)
  }
  else if(method == 'qrn_av'){
    samples <- map(.x = rep(0, n_samples), .f = ~summon_simulation_qrn_av(n_summons, goal, starting_pity, previous_featured)) %>%
      unlist() %>%
      matrix(nrow = n_samples, byrow = T) %>%
      data.frame() %>%
      arrange(X2)
    
    return(samples)
  }
}