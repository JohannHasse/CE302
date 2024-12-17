library(dplyr)
library(lubridate)
library(tidyverse)
install.packages("tidyverse")
require(magrittr)


airlines
airports
planes
weather

planes %>%
  count(tailnum) %>%
  filter(n > 1)

weather %>%
  count(time_hour,origin) %>%
  filter(n > 1)

planes %>%
  filter(is.na(tailnum))


flights

flights2 <- flights %>%
  filter(distance > 2000) %>%
  select(year,time_hour,origin,dest,tailnum,carrier)
flights2


flights2_airlines <- flights2  %>% 
  left_join(airlines,
            by = "carrier")

planes_flights = flights2 %>% 
  right_join(planes, by = "tailnum")


origin_flights = flights2 %>% 
  inner_join(airports, by = c("origin"= "faa"))


dest_flights = flights2 %>% 
  full_join(airports, by = c("dest"= "faa"))


airports %>% 
  semi_join(flights2, join_by(faa == origin))

flights %>%
  anti_join(airports, join_by(dest == faa)) %>% 
  distinct(dest)

flights %>% filter(arr_delay > 24) %>%
  select(month,arr_delay,origin) %>%
  left_join(weather,by = "origin")

################################################################################

library(data.table)

glimpse(flights)
glimpse(weather)
glimpse(delayed_flights)

delayed_flights <- flights %>%
  filter(arr_delay > 24)

delayed_weather <- delayed_flights %>%
  left_join(weather) 

padrões_clim <- delayed_weather %>%
  group_by(month, origin, temp, humid, wind_speed) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE), 
            count = n()) %>%
  arrange(desc(avg_delay))


delays_por_mês <- delayed_flights %>%
  group_by(month) %>%
  summarise(total_delay = sum(arr_delay, na.rm = TRUE), 
            num_flights = n()) %>%
  arrange(desc(total_delay))


Destinos_Comuns <- delayed_weather %>%
  mutate(mes = month(time_hour)) %>%
  group_by(dest,mes) %>%
  summarise(Total_de_vôs = n(),
            Temp_Média = mean(temp,na.rm = T),
            Precp_Média = mean(precip,na.rm = T)) %>%
  arrange(desc(Total_de_vôs))

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  

cia <- flights %>%
  select(carrier,tailnum)

planes_cia <- planes %>%
  left_join(cia)

cia_voa_avião <- planes_cia %>%
  group_by(tailnum,carrier) %>%
  summarise(Total_de_vôs = n()) %>%
  arrange(desc(carrier))

glimpse(flights)

lat_lon <- airports %>%
  select(faa,lat,lon)
  
flights_latlon <- flights %>%
  left_join(airports, by = c("dest"= "faa"))
  
install.packages("who")
library(who)

table1

table1_2 <- table1 %>% 
  select(country,year,cases) %>%
  pivot_wider(names_from = year,values_from = cases)
table1_2

table1_2 %<>% 
  pivot_longer(cols = -c(country), 
               names_to = "year", 
               values_to = "cases")
table1_2


table1 <- data.table(country = c("Afghanistan","Afghanistan","Brazil","Brazil",
                                  "China","China"),
                     year = c(1999,2000,1999,2000,1999,2000),
                     cases = c(745,2666,37737,80488,212258,213766),
                     population = c(19987071,20595360,172006362,174504898,
                                    1272915272,1280428583)
)

separado <- table1 %>%
  separate(rate, into = c("cases", "population"))

table1 <- separado %>%
  unite(rate,cases,population, sep = "/")

TB <- fread("TB.csv.gz")

glimpse(TB)



TB1 <- TB %>% 
  pivot_longer(
    cols = -c(1:4), 
    names_to = "chave", 
    values_to = "casos", 
    values_drop_na = TRUE
  )
TB1


TB1 %>%
  count(chave)

TB1 %<>% filter(chave %like% "^new")

TB2 <- TB1 %>% 
  mutate(chave = str_replace(chave, "newrel", "new_rel"))
TB2

TB3 <- TB2 %>% 
  separate(chave, c("new", "type", "sexage"), 
           sep = "_")
TB3

TB4 <- TB3 %>% 
  select(-new, -iso2, -iso3)
TB4

TB5 <- TB4 %>%
  separate(sexage, c("Sexo","Idade"), sep = 1)
TB5


TB5_wide1 <- TB5 %>%
  group_by(country, year) %>%
  summarise(casos_totais = sum(casos)) %>%
  mutate(year = str_replace(as.character(year), "^", "Ano_")) %>%
  pivot_wider(names_from = year, values_from = casos_totais)
TB5_wide1

TB5_wide2 <- TB5 %>%
  group_by(country, year,Idade,Sexo) %>%
  summarise(casos_totais = sum(casos)) %>%
  pivot_wider(names_from = year, values_from = casos_totais)
TB5_wide2

TB5_wide1_2 <- TB5_wide1 %>%
  pivot_longer(cols = -c(country),
               names_to = "year",
               values_to = "casos_totais")


TB5 %>%
  group_by(country, year,Idade,Sexo) %>%
  summarise(casos_totais = sum(casos))

TB5_wide2_2 <- TB5_wide2 %>%
  pivot_longer(cols = -c(country,Idade,Sexo),
               names_to = "year",
               values_to = "casos_totais") %>%
  select(country,year,Idade,Sexo,casos_totais)


