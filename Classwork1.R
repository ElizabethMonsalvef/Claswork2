library(nycflights13)
library(tidyverse)

##### 5.2.4 Exercises: items 1 and 2 #############

flights_d <- nycflights13::flights

Delay2 <- filter(flights_d,arr_delay >="120")

F_HOU <- filter(flights_d,dest =="HOU")


###################################################
##### 5.3.1 Excercises: all items #################

missing_flights_sorted  <- flights %>%
  arrange(desc(is.na(dep_time)))

most_delayed <- flights %>%
  arrange(desc(arr_delay))

fastest_flights <- flights %>%
  mutate(speed = distance / air_time) %>%
  arrange(desc(speed))

farthest_flights <- flights %>%
  arrange(desc(distance))

shortest_flights <- flights %>%
  arrange(distance)

#### 5.4.1 Exercises: items 2, 3, and 4 ############
#### If a variable name is included many times in a select() call, the variable will appear multiple times in a result.######

v2<- select (flights, year, month, day, dep_time, dep_time)
Print(v2)

variables <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(variables))

time <- select(flights, contains("TIME"))
print(time)

default<-select(flights, contains("TIME", ignore.case = FALSE))
print(default)

## 5.5.2 Exercises: items 1, and 2

minutes_since_midnight<-transmute(flights,deptime = dep_time/60, schedeptime=sched_dep_time/60)
print(minutes_since_midnight)

function1<-flights %>% 
mutate(dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),
         sched_dep_time = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100),
         arr_time = (arr_time %/% 100) * 60 + (arr_time %% 100),
         sched_arr_time = (sched_arr_time %/% 100) * 60 + (sched_arr_time %% 100)) %>%
transmute((arr_time - dep_time) %% (60*24) - air_time)



flights %>%
  group_by(flight) %>%
  summarise(n = n(),
            fifteen_early = mean(arr_delay == -15, na.rm = TRUE),
            fifteen_late = mean(arr_delay == 15, na.rm = TRUE),
            ten_always = mean(arr_delay == 10, na.rm = TRUE),
            thirty_early = mean(arr_delay == -30, na.rm = TRUE),
            thirty_late = mean(arr_delay == 30, na.rm = TRUE),
            percentage_on_time = mean(arr_delay == 0, na.rm = TRUE),
            twohours = mean(arr_delay > 120, na.rm = TRUE)) %>%
  map_if(is_double, round, 2) %>%
  as_tibble()

F1<-filter(fifteen_early == 0.5, fifteen_late == 0.5)
F2<-filter(ten_always == 1)
F3<-filter(thirty_early == 0.5 & thirty_late == 0.5)
F4<- filter(percentage_on_time == 0.99 & twohours == 0.01)
as_tibble()





