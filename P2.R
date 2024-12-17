eh_negativo <- function(n){
  if(n < 0){
    return("sim")
  } else {
    return("não")
  }
}

eh_primo <- function(n){
  if(as.numeric(n) < 0){
    return("não")
  }
  
  divisores = 0
  for(i in 1:as.numeric(n)){
    if(n %% i == 0){
      divisores = divisores + 1
    } else {
      divisores = divisores
    }
  }
  if(divisores > 2){
    return("não")
  } else {
    return("sim")
  }
}

eh_qudrad <- function(n){
  if(n < 0){
    return("não")
  } 
  raiz = sqrt(n)
  if(raiz == floor(raiz)){
    return("sim")
  } else {
    return("não")
  }
  
  return(qudrad)
}



transf_matriz <- function(matriz){
  
  for(i in 1:nrow(matriz)){
    for(j in 1:ncol(matriz)){
      if(eh_primo(matriz[i,j]) == "sim"){
        matriz[i,j] = matriz[i,j] * 3
      } else if (eh_qudrad(matriz[i,j]) == "sim"){
        matriz[i,j] = matriz[i,j] - 14
        if(matriz[i,j] < 0){
          matriz[i,j] = matriz[i,j]^5
        }
      } else if (eh_negativo(matriz[i,j]) == "sim"){
        matriz[i,j] = sqrt(abs(matriz[i,j]))
      } 
    }
  }
  
  return(matriz)
}

matriz_A <- matrix(c(-5,7,8,9,-2,-1,5,3,-8,6,-7,4,10,-6,0,2),nrow = 4)
matriz_B <- matrix(c(-11,17,-2,-12,-8,0,4,10,-6,-20,-19,20,-3,-17,-9,-10,-18,15,-15,1),nrow = 4)
matriz_C <- matrix(c(6,5,-19,15,25,30,-3,16,-24,2,-5,13,-6,-18,1,-28),nrow = 4)

transf_matriz(matriz_A)

matriz_A[2,1]




