library(lubridate)
library(data.table)
library(tibble)
library(magrittr)
library(dplyr)
library(tidyverse)

################################################################################
seq5 <- seq(10,100,5)

seq5 <- seq5 * 2

subconjunto <- seq5[1:2]

altura <- c("João" = 1.82,
            "Bianca" = 1.68,
            "Eduarda" = 1.62,
            "Pedro" = 1.78,
            "Joana" = 1.67,
            "Julhos" = 1.87)


maior_q_1.74 <- altura > 1.74
altura[maior_q_1.74]

gsub(pattern = "G",
     replacement = "R",
     "Gato")

dados <- c(1, NA, 3, 4, NA)


idade <- c(20,38,14)
names(idade)

names(idade) <- c("João", "Pedro", "Luana")

idade["João"]

################################################################################

vetor_a <- c(1,7,3,4)
vetor_b <- c(7,3,9,5)

matriz1 <- rbind(vetor_a, vetor_b)
matriz1

matriz2 <- cbind(vetor_a,vetor_b)
matriz2

A <- matrix(seq(1:9),nrow = 3,byrow = F)
A

B <- matrix(seq(1:16), nrow = 4, byrow = T)
B


elementos_maior_q_3 <- B[which(B > 3)]
elementos_maior_q_3

B[B > 3]

B[!(B %% 2 == 0)]

A 
A <- A[,-2]
A

B
B <- B[-3,]
B

A
colSums(A)

B
rowSums(B)

upper.tri(A)
A[upper.tri(A,diag = F)]
A[upper.tri(A,diag = T)]

################################################################################

meu_data_frame <- data.frame(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  meio_de_transporte = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta')
)

meu_data_frame$genero <- c("F", "M", "F", "F", "M", "M", "F", "M")
meu_data_frame

subset(meu_data_frame, idade > 25 & idade < 50)

meu_data_frame$genero <- NULL


# Exemplo de uso da função by()
resultado <- by(meu_data_frame$salario, meu_data_frame$idade, mean)
resultado


meu_data_frame$genero = as.factor(meu_data_frame$genero)

################################################################################

Queimadas <- read.csv("C:\\Users\\cabelo\\Documents\\Rstudio\\data\\Queimadas.csv")

head(Queimadas,n = 9)

tail(Queimadas, n=3)

nrow(Queimadas)

ncol(Queimadas)

summary(Queimadas)

str(Queimadas)

unique(Queimadas$bioma)

Sul <- c("Santa Catarina","Rio Grande do Sul","Paraná")
Norte <- c("Amazonas","Pará","Acre","Roraima","Rondônia","Amapá","Tocantins")


região_sul <- subset(Queimadas, estado %in% toupper(Sul))
mean(região_sul$avg_numero_dias_sem_chuva)

região_norte <- subset(Queimadas, estado %in% toupper(Norte))
mean(região_norte$avg_numero_dias_sem_chuva)

library(data.table)

meu_data_table <- data.table(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  meio_de_transporte = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta'))
meu_data_table


# ler um arquivo como data.table
Queimadas <- fread("C:\\Users\\cabelo\\Documents\\Rstudio\\data\\Queimadas.csv")


dados <- readr::read_csv("Mental Health Dataset.csv")

head(dados,2)
glimpse(dados)


################################################################################

car_crash <- fread("C:\\Users\\cabelo\\Documents\\Rstudio\\data\\Brazil Total highway crashes 2010 - 2023.csv.gz")
car_crash

glimpse(car_crash)

car_crash1 <- car_crash %>%
  select(data, tipo_de_ocorrencia, automovel, bicicleta, onibus, caminhao, moto, outros)

car_crash1 <- car_crash %>%
  select(contains("feridos"))

car_crash1 <- car_crash %>%
  select(is.numeric)

car_crash1 <- car_crash %>%
  select(is.logical)
  
car_crash1 <- car_crash %>%
  select(ends_with("o"))

car_crash1 <- car_crash %>%
   select(starts_with("t"))

car_crash1 <- car_crash %>%
  select(automovel,moto) %>%
  filter(automovel >= 5 & moto == 3)

car_crash1 <- car_crash %>%
  select(automovel,moto) %>%
  filter(automovel >= 5 | moto == 3)
 
car_crash1 <- car_crash %>%
  filter(tipo_de_ocorrencia == "com vítima")

operadoras <- c("Autopista Regis Bittencourt","Autopista Litoral Sul","Via Sul")

car_crash1 <- car_crash %>%
  filter(automovel >= 5 | moto == 3) %>%
  filter(lugar_acidente %in% operadoras) %>%
  select(lugar_acidente,automovel,moto)

tabela <- car_crash %>% 
  filter(tipo_de_ocorrencia %in% c("sem vítima", "com vítima"))%>% 
  group_by(tipo_de_ocorrencia) %>%
  summarise(n = n(), 
            f_r = n()/nrow(car_crash), 
            f_per = n()/nrow(car_crash) * 100, 
            media = mean(levemente_feridos, na.rm = T), 
            Q1 = quantile(levemente_feridos, 0.25, type = 5, na.rm = T), 
            Q2 = quantile(levemente_feridos, 0.5, type = 5, na.rm = T), 
            Q3 = quantile(levemente_feridos, 0.75, type = 5, na.rm = T), 
            var = var(levemente_feridos, na.rm = T), 
            sd  = sd(levemente_feridos, na.rm = T), 
            min = min(levemente_feridos, na.rm = T), 
            max = max(levemente_feridos, na.rm = T)) 
tabela

glimpse(starwars)

length(unique(starwars))

tabela1 <- starwars %>%
  group_by(species) %>%
  summarise(Quantidade = n(),
            Quantidade_R = n()/nrow(starwars))
tabela1

tabela2 <- starwars %>%
  filter(!is.na(gender)) %>%
  group_by(gender) %>%
  summarise(média_altura = mean(height, na.rm = T))
tabela2  

tabela3 <- starwars %>%
  filter(gender == "masculine") %>%
  select(birth_year,species,gender) %>%
  group_by(species)%>%
  summarise(média_idade = mean(birth_year, na.rm = T))
tabela3


tabela4 <- starwars %>% 
  select(name, birth_year, species) %>% 
  group_by(species) %>% 
  mutate(primeiro_da_especie = max(birth_year, na.rm = T)) %>% 
  filter(primeiro_da_especie == birth_year)

################################################################################

data_string <- "2024/08/23"
data <- as.Date(data_string)
data
class(data)

data_formadata <- as.Date(data_string,
                          format = "%Y/%m/%d")
data_formadata


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

data <- as.Date("2023-08-04")
data
data2 <- data - 1
data2
data3 <- data + 56
data3


data <- as.Date("2023-08-21")
data_formatada <- format(data, "%d/%m/%Y")


data <- as.Date("2023-08-21")
ano <- format(data, "%Y")
mes <- format(data, "%m")
dia <- format(data, "%d")

print(paste(ano,mes,dia))

library(lubridate)

data_ymd <- ymd("2023-08-21")
data_mdy <- mdy("08-21-2023")
data_dmy <- dmy("21-08-2023")

print(data_ymd)
print(data_mdy)
print(data_dmy)


# Data original no fuso horário de Nova Iorque
data_ny <- ymd_hms("2023-08-21 12:00:00", tz = "America/New_York")

# Converter para o fuso horário de Londres
data_london <- with_tz(data_ny, tz = "Europe/London")

diferenca_horas <- as.numeric(data_london - data_ny)
diferenca_horas



dados <- data.frame(
  nome = c("Evento 1", "Evento 2"),
  data = c(
    ymd_hms("2023-08-21 12:00:00", tz = "America/New_York"),
    ymd_hms("2023-08-21 17:00:00", tz = "Europe/London")
  )
)

# Converter todas as datas para um fuso horário comum, por exemplo, UTC
dados$data_utc <- with_tz(dados$data, tz = "UTC")

print(dados)

################################################################################

require(dplyr)
require(tidyr)
require(data.table)
library(lubridate)

car_crash <- fread("C:\\Users\\cabelo\\Documents\\Rstudio\\data\\Brazil Total highway crashes 2010 - 2023.csv.gz")

glimpse(car_crash)

car_crash2 = car_crash %>% 
  mutate(nova_data = 
            as.Date(data, 
                    format = "%d/%m/%Y")) %>% 
  mutate(novo_horario = hms(horario)) %>% 
  mutate(mes = month(nova_data), 
         ano = year(nova_data), 
         hora = hour(novo_horario))

car_crash2 %>% 
  group_by(mes) %>% 
  summarise(total_mes = n()) %>%
  filter(total_mes == max(total_mes))
