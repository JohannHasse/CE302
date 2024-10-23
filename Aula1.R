library(tidyverse)
install.packages("tzdb")


dados <- readr::read_csv("data/Mental Health Dataset.csv")


head(dados)
class(dados)
tail(dados)



glimpse(dados)


Poland <- subset(dados, Country == "Poland")

Colunas <- dados[, c("Gender","Country")]

### Pipe #################################################################


x <- seq(1:10)
y <- sqrt(x)
z <- log(y)


# Jeito Feio
tan(
  cos(
    log(
      sqrt(
        x
          )
        )
      )
    )

# Jeito Bonito/Certo/Pipe

x %>%
  sqrt() %>%
  log() %>%
  cos() %>%
  tan() 

# Usando o Pipe ############################################################

# x %>% f é equivalente à f(x)



# Usando magrittr #

require(magrittr)

set.seed(123)

rnorm(10) %>%
  multiply_by(5) %>%
  add(5)

# Que é a masma coisa que: 

rnorm(10) %>%
  `*`(5) %>%
  `+`(5)



### DATA FRAMES #######################################################

require(dplyr)


meu_data_frame <- data.frame(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  meio_de_transporte = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta'))

meu_data_frame = meu_data_frame %>%
  mutate(idade_Mais_25 = idade > 25)

meu_data_frame

require(tidyr)
require(dplyr)
require(data.table)



### NOVO DATA FRAME #################################################

car_crash = fread("data/Brazil Total highway crashes 2010 - 2023.csv")

summary(car_crash)
head(car_crash)

car_crash[,c("onibus","moto")]

# ou 

x1 <- car_crash %>% 
  select(c(1:4,9))

x1_1 <- car_crash %>% 
  select(-c("moto","onibus"))

x1_2 <- car_crash %>%
  select(contains("feridos"))

x1_3 <- car_crash %>%
  select(where(is.numeric)) 
    
x1_4 <- car_crash %>%
  select(where(is.logical))

######################################################################

var_inter <- c("onibus","moto",)

car_crash %>%
  select(any_of(var_inter))

######################################################################

# Vetore de moto

car_crash %>% 
  pull(moto)


# Data frame de moto

car_crash %>% 
  select(moto)


car_crash %>% 
  select(moto, automovel, data) %>% 
  filter(moto > 2 & automovel == 2)

car_crash %>% 
  group_by(tipo_de_ocorrencia) %>% 
  summarise(media = mean(automovel), na.rm = T)


car_crash %>% 
  filter(tipo_de_ocorrencia %in% c("sem vitima","com vítima")) %>% 
  group_by(tipo_de_ocorrencia) %>% 
  summarise(media_carros = mean(automovel, na.rm = T),
            media_motos = mean(moto, na.rm = T),
            mediana_carros = median(automovel, na.rm = T),
            n = n(),
            quantil_25 = quantile(automovel, 0.25))



###############################################################################

#1
summary(car_crash)


Total_Automovel <-sum(car_crash$automovel, na.rm = T)
Total_Bicicleta <- sum(car_crash$moto, na.rm = T)
Total_Caminhão <-sum(car_crash$caminhao, na.rm = T)
Total_Moto <-sum(car_crash$moto, na.rm = T)
Total_Onibus <-sum(car_crash$onibus, na.rm = T)
Total_Outros <-sum(car_crash$outros, na.rm = T)
 
Total <- data.frame()
car_crash %>% 
  select(data, tipo_de_ocorrencia, automovel, bicicleta, onibus, 
         caminhao, moto, trator_maquinas, outros)

#2 #2 bicicleta
car_crash %>% 
  select(ends_with("feridos"))
















data_string<- "23/10/2014"

data <- as.Date(data_string,
                format = "%d/%m/%Y")
print(data)



  