#DATA SOURCE: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) Coronavirus
#Provides a daily summary of the Coronavirus (COVID-19) cases by state/province. Data source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) Coronavirus https://github.com/CSSEGISandData/COVID-19.

library(tidyverse)
library(lubridate)

confirmedDataURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
deathsDataURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
recoveredDataURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"


## read in the data and put it in tidy form
confirmedDF = read_csv(confirmedDataURL) %>%
  pivot_longer(cols = -(1:4), 
               names_to = "fileDate", 
               values_to = "cases") %>%
  select(region = `Country/Region`, fileDate, cases) %>%
  mutate(type = "confirmed")

deathsDF = read_csv(deathsDataURL) %>%
  pivot_longer(cols = -(1:4), 
               names_to = "fileDate", 
               values_to = "cases") %>%
  select(region = `Country/Region`, fileDate, cases) %>%
  mutate(type = "deaths")

recoveredDF = read_csv(recoveredDataURL) %>%
  pivot_longer(cols = -(1:4), 
               names_to = "fileDate", 
               values_to = "cases") %>%
  select(region = `Country/Region`, fileDate, cases) %>%
  mutate(type = "recovered")

## combine the data
covidDF = bind_rows(confirmedDF,
                    deathsDF,
                    recoveredDF)


## save the data to a file
write_csv(covidDF, "covidDF.csv")

## move on to analysis file
