# Sla
require(readxl)
require(dplyr)

View(pokemons) <- read_excel("C:/Users/cabelo/Documents/Rstudio/data/dados/pokemon.xlsx") # 'dataset' é a tabela importada pelo Power BI

pokemons_nomes <- pokemons %>%
  select(Name) %>%
  distinct()

View(pokemons_nomes)








require(readxl)
require(dplyr)
require(writexl)
install.packages("writexl")

# Carregar os dados do arquivo Excel
pokemons <- read_excel("C:/Users/cabelo/Documents/Rstudio/data/dados/pokemon.xlsx")

# Filtrar os Pokémons Mega
pokemons_mega <- dplyr::filter(pokemons, grepl("^Mega", Name))

# Salvar os dados filtrados em um arquivo Excel
write_xlsx(pokemons_mega, "C:/Users/cabelo/Documents/Rstudio/data/dados/pokemons_mega.xlsx")
