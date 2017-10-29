# Matt Russo
# 31.08.2017
# Solution to SS 2017 final assessment 


##################################################################################################################
# clear the environment 
rm(list = ls())

##################################################################################################################
# trials of female orca whale populations and their offspring

# list of ages
ages <- c()
# list of recruits
recruits <- c()
# trial number
trials <- 0

while (trials < 1000) {
  
  # increase trial
  trials <- trials + 1
  #individual's age
  age <- 15
  # is the individual alive, 0: no, 1: yes  
  alive <- 1
  # if the female has a calf set to 1, if not 0
  calf <- 0
  # the current calf's age
  calfage <- 0
  # the number of weaned offspring of an individual female
  offspring <- 0
  
  # count of recruits for an individual female
  recruitCount <- 0
  
  # loops over a single whale's life cycle while it is able to breed
  while (alive == 1 && age < 40) {
  
    # represents condition of individual female 
    co <- rnorm(1, 100, 10)
    
    # environment mean, represents quality of environment
    enMu <- 20
    # environment standard deviation, represents stability of environment
    enSD <- 1
    # random condition based on environment mean and standard deviation
    co2 <- rnorm(1, enMu, enSD)
    # add the random condition to the original condition
    co <- co + co2
    
    # one part of a linear predictor
    s0 <- -2
    # second part of a linear predictor
    s1 <- 0.05
    # linear predictor
    s <- s0+s1*co
    
    # survival rate 
    sr <- exp(s)/(1+exp(s))
  
    # is the individual alive, 0: no, 1: yes
    alive <- rbinom(1,1,sr)
  
    #check is the individual is alive
    if (alive == 1) {
      # increment the individuals age by 1
      age <- age + 1
    } 
    
    # one part of a linear predictor
    b0 <- -10
    # second part of a linear predictor
    b1 <- 0.1
    # linear predictor
    br <- b0+b1*co
    
    # probability of breeding
    b <- exp(br)/(1+exp(br))
    # did the individual give birth, 0: no, 1: yes
    calf <- rbinom(1,1, b)
    # did the calf survive, 0: no, 1: yes
    cdr <- rbinom(1, 1, .8)
    
    # check if the individual has had a new calf
    if (calf == 1 && calfage == 0 && cdr == 1) {
      
      # increment calf age by 1
      calfage <- calfage + 1
      # negative effect on condition
      inv <- 10
      # decrement the individual's condition 
      co <- co - inv
      
    } else if (calfage > 0 && calfage < 5 && cdr == 1) { # check if the individual already has a calf and it survives the year
      
      # increment calf age by 1
      calfage <- calfage + 1
      # get negative effect on condition  
      inv <- 10 * calfage
      # decrement the individual's condition 
      co <- co - inv
      
    } else {  # else the calf has died 
      
      #reset calf and calf age
      calf <- 0
      calfage <- 0
    }
    
    
    # check if the calf has weaned
    if (calfage == 5 && cdr == 1) {
    
      # uncomment this if you want to see sex dilineation of calves // one of two
      # sex of calf, 0: female, 1: male
      # sex <- rbinom(1, 1, .5)
      
      # add one to the count of weaned offspring
      offspring <- offspring +1
      # check if the offspring made it to recruitment, 0: no, 1: yes
      recruit <- rbinom(1, 1, .98)
      # add to this individual's recruit count
      recruitCount <- recruitCount + recruit
      
      # uncomment this if you want to see sex dilineation of calves // two of two
      # recruitCount <- recruitCount + ((recruit + sex) %% 2)
      
      #reset calf and calf age
      calf <- 0
      calfage <- 0
      
    } 
  }
  
  # add the amount of recruits this individual has made to the overall list
  recruits <- c(recruits, recruitCount)
  # add this individuals age at end of breeding to the overall list
  ages <- c(ages, age)

}

# average number of recruits
avgRecruits <- (cumsum(recruits)[1000] / 1000)

##################################################################################################################
# plot histograms

# histogram for ages at the end of breeding
hist(ages,
     main="Ages at End of Breeding",
     xlab="Age",
     border="black",
     ylim=c(0,1000),
     col="lightblue",
     breaks=25,
     las=1)

# histogram for inclusive fitness
hist(recruits,
     main="Number of Inclusive Fitness",
     xlab="Recruits",
     border="black",
     ylim=c(0,1000),
     col="lightgreen",
     las=1)


