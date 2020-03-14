library(tidyverse)
library(lubridate)

theme_set(theme_minimal(22))

## growth rate estimates
## De Blasio says local prepared for a coronavirus outbreak with 1,200 hospital beds

## of new cases in local (assume throughput rate)
## average length of stay 2 days

confirmedDataURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

localName = "New York County, NY"
localAbbrevName = "NYC"
#new local cases
(localDF = read_csv(confirmedDataURL) %>%
  pivot_longer(cols = -(1:4), 
               names_to = "fileDate", 
               values_to = "cases") %>%
  mutate(fileDate = lubridate::mdy(fileDate)) %>%
    filter(fileDate >= ymd("2020-03-01")) %>%
  rename(state = `Province/State`,
         country = `Country/Region`) %>%
  filter(state == localName) %>%
  group_by(fileDate) %>%
  summarize(totalCases = sum(cases)) %>%
    mutate(newCases = totalCases - lag(totalCases)) %>%
  filter(totalCases > 0))

(italyDF = read_csv(confirmedDataURL) %>%
    pivot_longer(cols = -(1:4), 
                 names_to = "fileDate", 
                 values_to = "cases") %>%
    mutate(fileDate = lubridate::mdy(fileDate)) %>%
    filter(fileDate >= ymd("2020-03-01")) %>%
    rename(state = `Province/State`,
           country = `Country/Region`) %>%
    filter(country == "Italy") %>%
    group_by(fileDate) %>%
    summarize(totalCases = sum(cases)) %>%
    mutate(newCases = totalCases - lag(totalCases)) %>%
    filter(totalCases > 0))

localDF %>%
  ggplot(aes(x = fileDate, y = newCases)) +
  geom_col()

expGrowth = function(yesterdayCases,growthRate) {
  todaysCases = yesterdayCases * (1 + growthRate)
  return(todaysCases)
}

simTotalCaseVector = function(totalCases, numDays, growthPct){
  ## initialize blank total case vector
  totalCaseVector = numeric()
  
  ## use first two days to initialize
  ## total case prediction for subsequent days
  prevTotal = totalCases[length(totalCases)]
  prevNew = prevTotal - totalCases[length(totalCases)-1]

  ## create vector predicted by growthPct
  for(i in 1:numDays) {
    newCases = expGrowth(yesterdayCases = prevNew,
                         growthRate = growthPct)
    if(i == 1) {
      totalCaseVector[i] = prevTotal + newCases
    } else {
      totalCaseVector[i] = totalCaseVector[i-1] + newCases
    }
    prevNew = newCases
  }
  
  return(totalCaseVector)
}

## test function 
simTotalCaseVector(totalCases = localDF$totalCases,
                   numDays = 30,
                   growthPct = 0.1)




## make predictions for future
nDays = 20
fcstDate = seq((max(localDF$fileDate) + 1),
               (max(localDF$fileDate) + nDays),
               by = 1)
fcstNewCases = function(growth) {
  newC = simTotalCaseVector(totalCases = localDF$totalCases,
                               numDays = nDays,
                               growthPct = growth)
  fcstTotalCases = localDF$totalCases[nrow(localDF)] +
    cumsum(newC)
  return(fcstTotalCases)
}
fcstNewCases(0.14)

## combine predictions with previous dataframe
prevDF = localDF %>% 
  select(date = fileDate, totalCases) %>%
  mutate(caseType = paste0("Actual ",localAbbrevName," Data"))

prevDFItaly = italyDF %>%
  select(date = fileDate, totalCases) %>%
  mutate(caseType = "Actual Italy Data")

fcstDF = tibble(date = ymd(),
                totalCases = numeric(),
                caseType = character(),
                growthRate = numeric())

minGrowth = 0.05
maxGrowth = 0.20

for(growthRate in seq(0.05, 0.20, 0.05)) {
  fcstDF = tibble(
    date = fcstDate,
    totalCases = fcstNewCases(growthRate),
    caseType = paste0(
      "Predicted local cases\nassuming ",
      100 * minGrowth,
      "% to ",
      100 * maxGrowth,
      "%\ndaily growth in new cases"
    ),
    growthRate = growthRate
  ) %>%
    bind_rows(fcstDF) %>%
    arrange(date)
}

plotDF = bind_rows(prevDF,fcstDF,prevDFItaly)

(newCasesByDate = plotDF %>%
    group_by(growthRate) %>%
    mutate(newCases = totalCases - lag(totalCases)) %>%
    filter(!is.na(growthRate)) %>%
    filter(date == min(fcstDF$date) + 1 |
             date == round((max(fcstDF$date) - min(fcstDF$date))/2,0) + min(fcstDF$date) |
             date == max(fcstDF$date)) %>%
    group_by(date) %>%
    summarize(minNewCases = min(newCases),
              maxNewCases = max(newCases),
              medianTotalCases = median(totalCases)) %>%
    mutate(label = paste0(prettyNum(signif(minNewCases,2),big.mark=",",scientific=FALSE)," - ",prettyNum(signif(maxNewCases,2),big.mark=",",scientific=FALSE), " new \n cases fcstd on \n",date)))


## plot combined data
## make tidy and plot
plotDF %>% ggplot() +
  geom_point(aes(x=date,y=totalCases,color=caseType),
              size = 4) +
  ggtitle("What Exponential Growth in New York County Might Look Like") + 
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal(16) +
  labs(x = element_blank(),
       y = "Total Confirmed Cases") +
  geom_segment(data = newCasesByDate,
               aes(x = date -0.5 , y = 1.75 * max(plotDF$totalCases) / 2,
                   xend = date, yend = medianTotalCases)) +
  geom_label(data = newCasesByDate,
            aes(x = date, y = 1.75 * max(plotDF$totalCases) / 2, label = label),
            nudge_x = -0.5) +
  annotate("text", x = max(prevDF$date), y = (max(prevDF$totalCases) + max(plotDF$totalCases) / 3)/4, label = paste0("Last Observation of \n",max(prevDF$totalCases), " confirmed cases \n as of \n",max(prevDF$date))) + scale_color_manual(values = c("black","navyblue","red")) +
  theme(legend.title = element_blank(),
    legend.key.size = unit(2.0, 'cm')) +
  labs(caption = "Data Source: COVID19 Repository by Johns Hopkins CSSE at https://github.com/CSSEGISandData/COVID-19")





  