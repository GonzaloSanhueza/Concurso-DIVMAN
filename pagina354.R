## Autor: Gonzalo Sanhueza
## Ejemplo Simulación de un componente

## - Un componente falla con una distribución exponencial con tiempo medio de 
## falla de 500 h.
## - El proceso de reparación tiene una distribución normal con tiempo medio 
## de reparación de 25 h. y desviación estándar de 5h.
## Calcular el porcentaje de tiempo de inactividad en 2500 h.

## Supuestos
## - La simulación converge a las 100 replicas.
## - Si el tiempo de proceso excede las 2500 horas con la última simulación 
## de tiempo de reparación, entonces el ultimo tiempo de reparación no se 
## considera para el cálculo del tiempo de proceso.


process_time <- c()
downtime <- c()
set.seed(2020) ## Fijar Semilla para obtener resultados reproducibles
for(i in 1:100){
  process_time[i] = 0
  downtime[i] = 0
  while (process_time[i] < 2500){
    mttf = rexp(n = 1, rate = 1/500)
    process_time[i] = process_time[i] + mttf
    if (process_time[i] >= 2500){
      break
    } 
    rp <- rnorm(1,25,5)
    variable_temporal = process_time[i] + rp
    if(variable_temporal >= 2500){
      break
    }
    else{
      process_time[i] = variable_temporal
      downtime[i] = downtime[i] + rp
    }
  }
}
downtime

downtime_pct = downtime/2500 *100
downtime_pct


