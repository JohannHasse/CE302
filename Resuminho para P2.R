require(dplyr)

#Matrizes

A <- matrix(c(1:9), 
            ncol = 3,
            byrow = TRUE)
A
A[2,3]
ncol(A)

B <- matrix(c(1:9), 
            ncol = 3, 
            byrow = FALSE)
B
B[1,3]
nrow(B)

elementos_maior_que_3 <- B[which(B > 3)]
elementos_maior_que_3
length(elementos_maior_que_3)


################################################################################

A

for (i in 1:nrow(A)){
  for(o in 1:ncol(A)){
    print(A[i,o])
  }
  
}

B
C <- B

for (i in 1:nrow(B)){
  for(o in 1:ncol(B)){
    C[i,o] = B[i,o] * 2
  }
  
}

B
C

multiplicar_por_2(2)

D <- B
for (i in 1:nrow(B)){
  for(o in 1:ncol(B)){
    D[i,o] = multiplicar_por_2(B[i,o])
  }
  
}

B
D


# data.frames
df <- data.frame(a = 1:5, b = 6:10, c = 11:15)

# Aplicar a função sum a todas as colunas

resultado <- as.data.frame(lapply(df, mean))
print(resultado)

maior_que_4 <- function(x){
  ifelse(x<4,T,F)
}

df1 <- df %>%
  mutate_all(maior_que_4)
df1


df2 <- data.frame(tipo_a = c("bolas1","bolas2","bolas3","bolas4","cunts1",
                             "cunts2","cunts3"),
                  tipo_b = c(1,2,3,4,5,6,7),
                  caracoles1 = c(T,F,T,T,F,F,T),
                  caracoles2 = c("Grande","Pequeno","Pequeno","Pequeno",
                                 "Grande","Grande","Pequeno"))


# Seleção pelo nome
carateres <- df2 %>%
  select(where(is.character))

tipos <- df2 %>%
  select(starts_with("tipo"))

termina_b_2 <- df2 %>%
  select(ends_with("b") | ends_with("2"))

coles <- df2 %>%
  select(contains("coles"))


# Seleção por critérios

variavel <- c("tipo_a","caracoles1")
bla <- df2 %>%
  select(all_of(variavel))
bla

variavel2 <-c("tipo_a","caracoles1","gobuetas")
bla1 <- df2 %>%
  select(any_of(variavel))
bla1

# group by e summerise (aqui não vai funcionar porque só tem um de cada)

df2_tipo_a <- df2 %>%
  group_by(tipo_a) 


df2_tipo_a_mean <- df2 %>%
  group_by(tipo_a) %>%
  summarise(média = mean(tipo_b))

# data

data <- "2024-12-04"
data_normal <- as.Date(data)
data_normal

data_formatada <- format(data_normal, "%d/%m/%Y")
data_formatada

ano <- format(data_normal,"%Y")
ano
mês <- format(data_normal,"%m")
mês
dia <- format(data_normal,"%d")
dia


df5 <- data.frame(data = c("2024-05-03","2020-12-25","2012-06-19"))
df5 <- df5 %>%
  mutate(data = as.Date(data)) %>%
  mutate(mês = format(data,"%m")) 
df5

df6 <- df5 %>%
  filter(as.numeric(mês) > 5)


################ Limpeza dos dados ################

# Remover duplicatas (linhas repetidas)
data <- data %>%
  distinct()

# Tratar valores ausentes (NA) para "Idade" e "Salario"
data$Idade[is.na(data$Idade)] <- median(data$Idade, na.rm = TRUE)  # Substituindo NAs por mediana

data$Salario[is.na(data$Salario)] <- median(data$Salario, na.rm = TRUE)  # Substituindo NAs por mediana


data$Salario[data$Salario > 10000] <- median(data$Salario[data$Salario <= 10000], na.rm = TRUE)


data$Data_Nascimento <- as.Date(data$Data_Nascimento)



