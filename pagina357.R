## Autor: Gonzalo Sanhueza

## - Se requiere calcular el tiempo de falla de un sistema con componentes 
## en paralelo y en serie que son irreparables


nsim <- 100

## Supuestos
## - El sistema converge con 100 replicas

ta <- 500
tb <- 200
tc <- 600
t_falla <- c()
causa_falla <- c()

set.seed(2020) ## Fijar Semilla para obtener resultados reproducibles

for (i in 1:nsim) {
  falla_a <- rexp(n = 1, rate = 1/ta)
  falla_c <- rexp(n = 1, rate = 1/tc)
  if(falla_c <= falla_a){
    t_falla[i] = falla_c
    causa_falla[i] = "c"
  }
  else{
    falla_b <- rexp(n = 1, rate = 1/tb)
    falla_ab <- falla_a + falla_b
    if(falla_c <= falla_ab){
      t_falla[i] = falla_c
      causa_falla[i] = "c"
    }
    else{
      t_falla[i] = falla_ab
      causa_falla[i] = "ab"
    }
  }
}

falla_df <- data.frame(t_falla, causa_falla)
falla_df



