library(tidyverse)
library(lubridate)

theme_set(theme_minimal(16))

## read in cleaned data
cleanDF = read_csv("cleanedDF.csv")

## mirror analysis of 3Blue1Brown at
## https://youtu.be/Kas0tIxDvrg
outsideChinaConfirmedCasesDF = cleanDF %>%
  filter(region != "Mainland China",
         type == "confirmed") %>%
  group_by(fileDate) %>%
  summarize(newCases = sum(cases)) %>%
  mutate(totalCases = cumsum(newCases))

## visualize
outsideChinaConfirmedCasesDF %>%
  ggplot() +
  geom_point(aes(x = fileDate, y = totalCases)) +
  scale_y_continuous(labels = scales::comma_format())

## inside and outside china
## mirror analysis of 3Blue1Brown at
## https://youtu.be/Kas0tIxDvrg
cleanDF %>%
  mutate(region = ifelse(region == "Mainland China",
                         "China","Other")) %>%
  group_by(region,fileDate) %>%
  summarize(newCases = sum(cases)) %>%
  mutate(totalCases = cumsum(newCases)) %>%
  ggplot() +
  geom_point(aes(x = fileDate, y = totalCases, color = region)) +
  scale_y_continuous(labels = scales::comma_format())

## log scale
last_plot() + 
  scale_y_log10(labels = scales::comma_format())

## save outside China data to model
outsideChinaConfirmedCasesDF %>%
  write_csv("outsideChina.csv")
