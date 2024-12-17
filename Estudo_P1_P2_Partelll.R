require(tidyverse)
require(magrittr)

Texto_multilinha <- "Banana\nBanana"
str_view(Texto_multilinha)


Texto_horiz <- "Banana\tBanana\tBanana"
str_view(Texto_horiz)

texto_unicode_grau <- "A temperatura é de 25\u00B0C."
str_view(texto_unicode_grau)

texto_multilinhas_unicode <- "Primeira linha\u000ASegunda linha"
str_view(texto_multilinhas_unicode)

simbolo_somatorio <- "O símolo do somatório é: \u2211"
str_view(simbolo_somatorio)

emoji <- "OMG! Também posso usar emoji! \U1F631"
str_view(emoji)



df <- data.frame(nome = c("Ana", "Maria", "João", NA), 
                 sobrenome= c("Santos", "Silva", "Souza", NA))
df %>% 
  mutate(ola = str_c("Boa noite ", nome, " ", sobrenome, "!"))

df %>% 
  mutate(mensagem = str_glue("Boa noite {nome} {sobrenome}!"))

df$nome %>% 
  paste(., collapse = ", ")

df$nome %>% 
  str_flatten(na.rm = TRUE)

df$nome %>% 
  str_flatten(na.rm = TRUE, collapse = ", ", last = " e ")

df %<>% 
  mutate(Nome_Sobrenome = str_c(nome, sobrenome, sep = " "))

df$Nome_Sobrenome %>% 
  str_split(., " ", simplify = TRUE)


words %>%
  str_detect(., "^y")

words %>%
  str_detect(., "^[^y]")

words %>%
  str_detect("x$") %>%
  table()

words %>%
  str_detect("\\b[a-zA-Z]{3}\\b") %>%
  table()


words %>%
  str_detect("\\b[a-zA-Z]{7,}\\b") %>%
  table()

words %>%
  str_detect("([aeiouAEIOU][^aeiouAEIOU]){2}") %>%
  table()

words %>%
  str_detect("(^x|x$)") %>%
  table()

words %>%
  str_detect("^(?i)[aeiou].*[^aeiou]$") %>%
  table()

words %>%
  str_detect("(?i).*a.*e.*i.*o.*u.*") %>%
  table()

words %>%
  str_detect("(?=.*a)(?=.*e)(?=.*i)(?=.*o)(?=.*u)") %>%
  table()

colours()

cores <- colors()
cores_com_modificador <- cores[str_detect(cores, "(?i)(light|dark)")]

cores_sem_modificador <- str_replace(cores_com_modificador, "(?i)(light|dark)\\s?", "")

cores_restantes <- cores[!cores %in% cores_sem_modificador]

################################################################################

i <- 0
while (i < 6) {
  i = i + 1
  if (i == 3) {
    next
  }
  print(i)
}


set.seed(1234)
dado <- seq(1:6)
n_lancamento <- 0
sorteio <- 0

while (sorteio != 5) {
  sorteio =  sample(dado, 1)
  n_lancamento = n_lancamento + 1
  
  cat(paste0("\n\nLançamento: ", n_lancamento, "\nValor Sorteado: ", sorteio))
}

#   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

dado = c(1:6)
soma_dois_dados = function(dado1, dado2){
  soma = dado1 + dado2
  
}
quadrado_soma = function(soma){
  soma2 = soma^2
  return(soma2)
}

resultado = list()
k = 0 
for(i in dado){
  for(j in dado){
    k = k + 1
    soma = soma_dois_dados(dado[i], dado[j])
    somaqd = quadrado_soma(soma)
    
    resultado[[k]] = data.frame(dado1 = dado[i], 
                                dado2 = dado[j], 
                                soma = soma, 
                                soma2 = somaqd)
  }
}

resultado %<>% bind_rows()
resultado
#   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   



matriz1 <- matrix(1:6, nrow = 2)
soma_linhas <- apply(matriz1, 1, sum)
soma_colunas <- apply(matriz1, 2, sum)


resultado <- mapply(soma_dois_dados, 
                    dado, 
                    dado)
print(resultado)



dois_dados <- expand.grid(dado,dado)
resultado <- mapply(soma_dois_dados,
                    dois_dados$Var1,
                    dois_dados$Var2)
resultado




Fibonatti <- function(n){
  lista = c(0,1)
  for(i in 3:n){
    lista[i] = lista[i-1] + lista[i-2]
  }
  return(lista)
  }
Fibonatti(10)

eh_primo <- function(n){
  divisores = 0
  for(i in 2:n){
    if(n%%i == 0){
      divisores = divisores + 1
    }
  }
  if(divisores > 1){
    cat("O número não é primo")
  } else {
    cat("O numero é primo")
  }
  }

eh_primo(25)


quadrante <- function(x, y) {
  if (x > 0) {
    if (y > 0) {
      quadrante = "Quadrante 1"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
      return(quadrante)
    } else {
      quadrante = "Quadrante 4"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    }
  } else {
    if (y > 0) {
      quadrante = "Quadrante 2"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    } else {
      quadrante = "Quadrante 3"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    }
  }
}


quadrante(-1,1)

iris$cat_sepal = 
  case_when((iris$Sepal.Length < mean(iris$Sepal.Length) - sd(iris$Sepal.Length)) ~ "X < media - 1 sd", 
            (iris$Sepal.Length < mean(iris$Sepal.Length) + sd(iris$Sepal.Length)) ~ "X < media + 1 sd", 
            .default = "X > media + 1 sd")



classifica_combustivel <- function(transporte){
  tipo_de_gas <- switch(transporte,
                     "Carro" = "Gasolina ou Diesel",
                     "Moto" = "Gasolina",
                     "Bicicleta" = "Humana (sem combustível)l",
                     "Ônibus" = "Diesel ou Gás Natural",
                     "Trem" = "Eletricidade ou Diesel",
                     "Avião" = "Querosene",
                     "Avião" = "Diesel ou Gasolina",
                     
                     )
  return(tipo_de_gas)
}

classifica_combustivel("Carro")
classifica_combustivel("Avião")
