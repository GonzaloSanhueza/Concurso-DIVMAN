## Ejemplo Simulación de un componente

## Un componente falla con una distribución exponencial con tiempo medio de falla de 500 h.
## El proceso de reparación tiene una distribución normal con tiempo medio de reparación de 25 h. y desviación estandar de 5h.
## Calcula el porcentaje de tiempo muerto en 2500 h.

## Supuestos
## La simulación converge a las 100 replicas
## Si el tiempo de proceso excede las 2500 horas con la ultima simulación de tiempo de reparación, entonces el ultimo tiempo de reparación no se considera para el calculo del tiempo de proceso.


process_time <- c()
downtime <- c()
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


## install.packages("ggplot2")

library(ggplot2)

theme_set(theme_classic())

downtime_df <- data.frame(downtime_pct = downtime_pct)
downtime_df$q <- ifelse(downtime_df$downtime_pct < quantile(downtime_pct, 0.025) | 
                          downtime_df$downtime_pct > quantile(downtime_pct, 0.975), 
                        "fuera", "dentro")

downtime_df

ggplot(data = downtime_df, mapping = aes(x = downtime_pct)) +
  geom_histogram(bins = 15, color = "white", alpha = 0.5, aes(fill = q), show.legend = F) + 
  geom_vline(xintercept = mean(downtime_pct), linetype = "dashed") +
  scale_fill_manual(values = c(dentro = "dodgerblue4", fuera = "red")) +
  labs(title = "Distribución de porcentaje de Downtime\ncon intervalo de confianza del 95%", x = "Porcentaje de Downtime",
       y = "Frecuencia Absoluta")
  



