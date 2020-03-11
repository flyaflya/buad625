library(tidyverse)
library(lubridate)


## Model the growth
growthDF = read_csv("outsideChina.csv")

## Model Assumptions
## Exponential Growth Means 
## todaysCases = yesterdayCases * (1 + growthPercentage)
## for example, assume 15% growthPercentage, then we get
## this pattern of growth:
expGrowth = function(yesterdayCases,growthRate) {
  todaysCases = yesterdayCases * (1 + growthRate)
  return(todaysCases)
}

## so if yesterday, there was 100 cases,
## today we forecast the following:
expGrowth(100, 0.15)  ## or 115 cases.

## For coronavirus, a simple, but illustrative
## model can be
# N_d = # of cases on a givne day
# E = Avg # of people infected person exposed to
# p = probability of exposure getting infection
# pctGrowth = E*p
# so, N_{d+1} = N_d + E*p*N_d
(growthDF = growthDF %>% 
    mutate(yestNewCases = lag(newCases)) %>% 
    mutate(yestTotCases = lag(totalCases)) %>%
    mutate(pctGrowth = newCases / yestNewCases - 1))

## now let's simulate some growth models and
## see which one is the best at predicting data for
## February 1 - March 6th   (arbitrary date range)
(dataDF = growthDF %>%
    filter(fileDate >= ymd("2020-02-01") & 
           fileDate <= ymd("2020-03-06")) %>%
    select(fileDate, newCases, totalCases))

## NOTE: There are better ways to model this growth,
## but the simulate and check consistency with data 
## approach is how to tackle paydate prediction

## create function that takes known total cases vector 
## and a potential growth rate which describes it
## and simulates forecasted total cases using 
## potential growth rate
simTotalCaseVector = function(totalCases, growthPct){
  ## initialize blank total case vector
  totalCaseVector = numeric()
  numDays = length(totalCases)  ## get number of days to simul
  
  ## use first two days to initialize
  ## total case prediction for subsequent days
  totalCaseVector[1]= totalCases[1]
  totalCaseVector[2]= totalCases[2]
  newCases = totalCaseVector[2] - totalCaseVector[1]
  
  ## create vector predicted by growthPct
  for(i in 3:numDays) {
    newCases = expGrowth(yesterdayCases = newCases,
                         growthRate = growthPct)
    totalCaseVector[i] = totalCaseVector[i-1] + newCases
  }
  
  return(totalCaseVector)
}

## test function 
simTotalCaseVector(totalCases = dataDF$totalCases, 
                   growthPct = 0.1)


## visualize comparison
plotDF = tibble(
  date = dataDF$fileDate,
  actualCases = dataDF$totalCases,
  predCases = simTotalCaseVector(totalCases = dataDF$totalCases, 
                                 growthPct = 0.1)
)

## make tidy and plot
plotDF %>% 
  pivot_longer(-date, names_to = "caseType", values_to = "cases") %>%
  ggplot() +
  geom_point(aes(x=date,y=cases,color=caseType),
             size = 4) +
  ggtitle("Is actual growth rate higher or lower than 10%?")
  
save(expGrowth,simTotalCaseVector,dataDF, growthDF, file = "model.RData")

