## this file will pick the 
## growth rate that seems most
## consistent with the data

## clean workspace to start fresh:
##    1) CTRL+L   clears console
##    2) BROOM ICON    clears environment
##    3) Session -> Restart R   clears out loaded packages

library(tidyverse)
library(lubridate)

load("model.RData")

potentialGrowthRates = seq(0.1,0.3,0.001)

### create function to compare actual versus predicted
### call it a loss function to indicate better predictions
### are smaller (i.e. smaller loss).
lossFun = function(actual,predicted) {
  loss = mean((actual-predicted)^2) # aka mean-squared error
  return(loss)
}

# test loss fun
lossFun(actual = c(1,2,3), predicted = c(3,0,1))

# find best growth rate
# start with benchmark of predicting zero-growth
bestLoss = lossFun(dataDF$totalCases,0)
bestGrowth = 0
bestPred = numeric()  ## initialize blank vector
totalCases = dataDF$totalCases

# try to beat benchmark with all candidates, find best candidate
for(i in 1:length(potentialGrowthRates)) {
  predTotCases = simTotalCaseVector(totalCases,
                                    potentialGrowthRates[i])
  loss = lossFun(actual = totalCases,
                 predicted = predTotCases)
  if(loss < bestLoss){
    bestGrowth = potentialGrowthRates[i]
    bestPred = predTotCases
    bestLoss = loss
  }
}

# show best model
## visualize comparison
plotDF = tibble(
  date = dataDF$fileDate,
  actualCases = dataDF$totalCases,
  predCases = bestPred
)

## make tidy and plot
plotDF %>% 
  pivot_longer(-date, 
               names_to = "caseType", 
               values_to = "cases") %>%
  ggplot() +
  geom_point(aes(x=date,y=cases,color=caseType),
             size = 4) +
  ggtitle(paste0("Predicted Growth Rate of ",
                 100*bestGrowth,
                 "% versus Actuals")) + 
  scale_y_continuous(labels = scales::comma_format())

## make predictions for future
fcstDate = seq((max(growthDF$fileDate) + 1),
             ymd("2020-04-30"),
             by = 1)
fcstCases = numeric()
newCases = 
  expGrowth(growthDF$newCases[nrow(growthDF)],bestGrowth)
fcstCases[1] = growthDF$totalCases[nrow(growthDF)] +
  newCases
for(i in 2:length(fcstDate)){
  newCases = expGrowth(newCases,bestGrowth)
  fcstCases[i] = fcstCases[i-1] + newCases
}

## combine predictions with previous dataframe
prevDF = growthDF %>% 
  select(date = fileDate, totalCases) %>%
  mutate(caseType = "Actual")
fcstDF = tibble(date = fcstDate,
                totalCases = fcstCases,
                caseType = "Predicted") %>%
  bind_rows(prevDF) %>%
  arrange(date)
  
## plot combined data
## make tidy and plot
fcstDF %>%
  ggplot() +
  geom_jitter(aes(x=date,y=totalCases,color=caseType),
             size = 4) +
  ggtitle(paste0("Predicted Growth Rate of ",
                 100*bestGrowth,
                 "% versus Actuals")) + 
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal(16) +
  labs(x = element_blank(),
       y = "Total Confirmed Cases")

## log scale
last_plot() +
  scale_y_log10(labels = scales::comma_format())

          