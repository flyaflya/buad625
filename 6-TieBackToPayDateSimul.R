### hints for simulating pay dates

## let's simulate pay dates for someone
## who gets paid on the first and fiteenth
## of the month

library(tidyverse)
library(lubridate)
library(bizdays)  ## research this 
library(gtools)

## bizdays function to get quantlib calendars
load_quantlib_calendars("UnitedStates/Settlement", 
                        from = "2020-01-01", to = "2020-06-30")
## use settlement calendar to mimic bank holidays
bizdays.options$set(default.calendar = 
                      "QuantLib/UnitedStates/Settlement")
## load calendar from previous step to be used by bizdays
bizdays.options$get('default.calendar')

## see holidays ## note, also weekends have no pay dates
bizdays::holidays() 


# simulate PayDates on 15th and last of month
simBiMonthly = function(startDate,endDate) {
      ## initialize paydate vector
      payDateVector = lubridate::ymd()
      ## get date for the first of the startDate month
      firstOfMonth = floor_date(startDate, "month")
      ## get date for the fifteenth of the month
      fifteenth = firstOfMonth + 14
      ## get date for the last of the month
      lastOfMonth = ceiling_date(startDate, "month") - 1
      
      ## start incrementing dates until 
      dateIndex = 1
      payDateVector[1] = if_else(startDate <= fifteenth,
                           fifteenth,
                           lastOfMonth)
      
      while(payDateVector[dateIndex] <= endDate) {
        dateIndex = dateIndex + 1
        ## look at previous date to decide path
        nextDate =
          if_else(day(payDateVector[dateIndex-1]) == 15,
                 ## TRUE:  Get Last Day of Month
                 ceiling_date(payDateVector[dateIndex-1],
                              "month") - 1,
                 ## FALSE:  Get 15th Day of Next Month
                 ceiling_date(payDateVector[dateIndex-1],
                              "month") + 14
               )
        
        ## ensure date is within bounds (inelegant)
        if(nextDate <= endDate & nextDate >= startDate) {
          payDateVector[dateIndex] = nextDate
        }
        
        ## break out if proposed date is past end date
        if(nextDate > endDate) {
          break
        }
      }
      
      ## if date falls on Weekend or holiday, roll forward
      ## using adjust.previous function from bizdays
      finalDates = adjust.previous(payDateVector)
      return(finalDates)
      
      }

## test the function
startDate = ymd("2020-03-01")
endDate = ymd("2020-06-29")

simBiMonthly(startDate,endDate)

## debugging:  talk about ifelse vs dplyr::if_else in line 38

## scoring:  need to create method of determining 
## which of your simulated pay date sequences
## mirrors the bank account