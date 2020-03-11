## exploring COVID DATA
## read in data file
covidDF = read_csv("covidDF.csv")

## visualize data
covidDF %>%
  group_by(fileDate,type) %>%
  summarize(numCases = sum(cases)) %>%
  ggplot(aes(x = fileDate, y = numCases, fill = type)) +
  geom_col()

## WTH:  with the drops in cases
View(covidDF %>%
       group_by(fileDate,type) %>%
       arrange(region,fileDate) %>%
       filter(region == "Mainland China"))
### ahh yes!!! fileDate is stored as character and not date
str(covidDF)

## Here's a bandaid fix ... it would be better to fix
## the problem when the data is loaded, but then we 
## would not have this teachable moment here.
covidDF = covidDF %>%
  mutate(fileDate = mdy(fileDate))

## visualize data (roughly)
covidDF %>%
  group_by(fileDate,type) %>%
  summarize(numCases = sum(cases)) %>%
  ggplot(aes(x = fileDate, y = numCases, fill = type)) +
  geom_col()

## rewrite clean Data to new file
write_csv(covidDF, "cleanedDF.csv")

## continue to "3-Analysis.R"

