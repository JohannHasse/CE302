require(dplyr)

quadrado <- function(a){
  a = a^2
  return(a)
}


Coisas_d_5 <- function(b) {
  if (b < 5) {
    return("Menor")
  } else if (b > 5) {
    return("Maior")
  } else {
    return("Igual")
  }
}

df <- data.frame(a = c(1, 2, 3,6,4,5,3,8,6), b = c(4, 5, 6,3,5,8,4,5,6))
df

df <- df %>%
  mutate(a = sapply(a,FUN = quadrado)) %>%
  mutate(b_e_5 = sapply(b,FUN = Coisas_d_5))





print(df)

df <- df %>%
  mutate(df = sapply(df,FUN = quadrado))

print(df)



df <- data.frame(a = c(3, 5, 7), b = c(2, 6, 4))

# Modificando a coluna 'a' diretamente com transmute
df_modificado <- df %>%
  mutate(a = sapply(a, quadrado))

print(df_modificado)

################################################################################
################################################################################


minha_funcao <- function(x) {
  return(x^2 + 2*x + 1)
}

# Data frame de exemplo
library(dplyr)
df <- data.frame(a = 1:5, b = 6:10)

df <- df %>%
  mutate(a_transformada = minha_funcao(a))

# Visualize o resultado
print(df)

df <- df %>% 
  mutate(a = minha_funcao(a))
df



df <- df %>%
  mutate(across(everything(), minha_funcao))

# Visualize o resultado
print(df)


colnames(df) <- c("azasso","bezasso","cezasso")
names(df)[names(df) == "bezasso"] <- "tipo_b"
print(df)

################################################################################
matriz <- matrix(1:6, nrow = 2)
df42 <- as.data.frame(matriz)

df42[] <- apply(df42, c(1,2), menor_que_4)
df42
################################################################################
# Matrizes

multiplicar_por_2 <- function(x) {
  return(x * 2)
}

matriz2 <- matrix(c(1, 5, 3, 4, 6, 2, 7, 8, 1), ncol = 3)
matriz2

# Aplicar a função na segunda coluna
matriz3 <- matriz2

matriz3[, 2] <- sapply(matriz3[, 2], multiplicar_por_2)
matriz3


# Aplicar a função na terceira linha

matriz4 <- matriz2

matriz4[3,] <- sapply(matriz4[3,], multiplicar_por_2)
matriz4



# Linha específica: apply(matriz[linha, , drop = FALSE], 2, func)
# Coluna específica: apply(matriz[, coluna, drop = FALSE], 1, func)
# Várias linhas: apply(matriz[linhas, , drop = FALSE], 2, func)
# Várias colunas: apply(matriz[, colunas, drop = FALSE], 1, func)




################################################################################
matriz <- matrix(c(1, 5, 3, 7, 2, 4, 6, 1), ncol = 2)
df <- as.data.frame(matriz)

# Função para aplicar
minha_funcao <- function(x) {
  ifelse(x < 4, 0, x)
}

# Usando apply para aplicar a função a todas as colunas
df_novo <- apply(df, 2, minha_funcao)

# Exibindo o resultado
print(df_novo)

################################################################################
################################################################################


df1 <- data.frame(tipo_a = c("bolas","bolas1","cuters"),
                  tipo_b = c(1,2,3),
                  c = c(T,F,T))
df1

tabela <- df1 %>%
  filter(tipo_b >= 2)
tabela

tabela2 <- df1 %>%
  filter(tipo_a %in% c("bolas","bolas1"))
tabela2

df2 <- df1 %>%
  select(starts_with("tipo"))
df2

df2 <- df1 %>%
  select(contains("ipo"))



df2 <- df1 %>%
  select(where(is.character))
df2

df2 <- df1 %>%
  select(where(is.numeric))
df2

df2 <- df1 %>%
  select(where(is.logical))
df2



