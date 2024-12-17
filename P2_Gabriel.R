#Questão 1
Dados <- data.frame(ChickWeight)

media <- function(data){
  media = sum(data)/length(data)
  media = round(media, 2)
  return(media)
}

variancia_amostral <- function(dados) {
  media1 <- media(dados) 
  diferenca <- dados - media1  
  quadrados <- diferenca^2  
  variancia <- sum(quadrados) / (length(dados) - 1)  
  return(variancia)
}

variancia_amostral(ChickWeight$Time)

desvio_absoluto_medio <- function(dados2) {
  media1 <- media(dados2) 
  diferenca <- dados2 - media1  
  absoluto <- abs(diferenca)  
  dma <- sum(absoluto) / (length(dados2) - 1)  
  return(dma)
}

desvio_absoluto_medio(ChickWeight$Time)

assi_pearson <- function(dados2) {
  media3 <- media(dados2) 
  mediana <- median(dados2)
  diferenca <- media3 - mediana  
  desv_p <- sqrt(variancia_amostral(dados2))  
  as2 <- 3*diferenca/desv_p  
  return(as2)
}
assi_pearson(ChickWeight$Chick)

require(dplyr)
nivel_40 <- Dados %>% filter(Chick >=40)
nivel_a_40 <- Dados %>% filter(Chick ==40)

assi_pearson(nivel_40$Time)
desvio_absoluto_medio(nivel_a_40$Time)

variancia_amostral(ChickWeight$weight)
variancia_amostral(ChickWeight$Time)
variancia_amostral(as.numeric(ChickWeight$Chick))
variancia_amostral(as.numeric(ChickWeight$Diet))
desvio_absoluto_medio(ChickWeight$weight)
desvio_absoluto_medio(ChickWeight$Time)
desvio_absoluto_medio(as.numeric(ChickWeight$Chick))
desvio_absoluto_medio(as.numeric(ChickWeight$Diet))



#Questão 3
fat_reproducaao <- function(estacao) {
  fat_reproducao <- 0
  if(estacao == "Primavera") {
    fat_reproducao = 0.05} else{
      if (estacao == "Verão"){  
        fat_reproducao = 0.02} else{
          if (estacao == "Outono") { 
            fat_reproducao = -0.03} else{
              fat_reproducao = 0.03}
            
        }}
  return(fat_reproducao)
  }

PH <- function(ph) {
  PH1 <- 0
  if(ph >= 6.0 & ph <= 6.5) {
    PH1 = 0.09} else{
      if (ph >= 6.6 & ph <= 7.0){  
        PH1 = 0.19} else{
          if (ph >= 7.1 & ph <= 7.5) { 
            PH1 = 0.05} else{
              PH1 = 0.11}
          
        }}
  return(PH1)
  }

simular_lago <- function(dias, peixes_iniciais, estacao, ph){
  dia <- dias
  peixes <- peixes_iniciais
  pesc_lago <- PH(ph)   
  fator <- fat_reproducaao(estacao)
  taxa_repro <- 0.19+fator
  peixes_pesc <- peixes*pesc_lago
  peixes_p_dia <- peixes*(1+taxa_repro)-peixes_pesc
  resultado = list()
  dia_f <-0
  while (dia_f < dia){
    dia_f <- dia_f + 1
    resultado[[dia_f]] = data.frame(dia_lago = dia_f, 
                                    peixe_lago = peixes
    )
    peixes_pesc <- peixes*pesc_lago
    peixes_p_dia <- peixes*(1+taxa_repro)-peixes_pesc
    peixes <- peixes_p_dia
  if (peixes >= 100000) {break}
    }
  return(resultado)
}
simular_lago(52, 80, "Primavera", 7.4)
simular_lago(15, 80, "Verão", 7.7)
simular_lago(100, 80, "Verão", 7.7)
simular_lago(100, 80, "Primavera", 7.4)
#Questão 3
primo <- function(numero){
valor <- 


}

