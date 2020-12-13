
## Grafico 1

## install.packages("ggplot2")

library(ggplot2)

theme_set(theme_classic())
downtime_df <- data.frame(downtime_pct = downtime_pct)
downtime_df$q <- ifelse(downtime_df$downtime_pct < quantile(downtime_pct, 0.025) | 
                          downtime_df$downtime_pct > quantile(downtime_pct, 0.975), 
                        "fuera", "dentro")

downtime_df

grafico_1 <- 
  ggplot(data = downtime_df, mapping = aes(x = downtime_pct)) +
  geom_density(fill = "grey", alpha = 0.7, color = NA) +
  geom_histogram(bins = 15, color = "white", alpha = 0.5, 
                 aes(fill = q, y = stat(count)/sum(count))) +
  geom_errorbarh(aes(y = -0.01), xmin = quantile(downtime_pct, 0.025), 
                xmax = quantile(downtime_pct, 0.975), height = 0.01) +
  annotate("text", y = -0.01, x = quantile(downtime_pct, 0.025) - 0.05, label = "q1") + 
  annotate("text", y = -0.01, x = quantile(downtime_pct, 0.975) + 0.05, label = "q2") + 
  geom_vline(xintercept = mean(downtime_pct), linetype = "dashed") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c(dentro = "dodgerblue4", fuera = "red"), 
                    name = "Intervalo\nde confianza", 
                    labels = c(dentro = "En el 95%", fuera = "En el 5%")) +
  labs(title = "Distribución de porcentaje de Inactividad\ncon intervalo de confianza del 95%", 
       x = "Porcentaje de Inactividad", y = NULL) + 
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), plot.title = element_text(hjust = .5), 
        legend.position = c(0.75, 0.75))

ggsave(plot = grafico_1, filename = "pagina354.png", type = "cairo", dpi = 300,
       height = 3.5, width = 5)


## Gráfico 2

grafico_2 <- 
  ggplot(data = falla_df, mapping = aes(x = t_falla, y = causa_falla, fill = causa_falla)) +
  geom_density_ridges(color = NA, alpha = 0.5, scale = 30) + 
  scale_fill_manual(values = c(ab = "dodgerblue4", c = "red"), 
                    name = "Componente\nque causa la falla", 
                    labels = c(ab = "A y B", c = "C")) +
  labs(title = "Distribución de MTTF del sistema\npor componente", 
       y = NULL, x = "Tiempo medio de falla en horas (MTTF)") +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), plot.title = element_text(hjust = .5), 
        legend.position = c(0.75, 0.75))

ggsave(plot = grafico_2, filename = "pagina357.png", type = "cairo", dpi = 300,
       height = 3.5, width = 5)

## Estadisticas 1

data.frame(downtime_pct) %>% 
  summarise(media = mean(downtime_pct), q1 = quantile(downtime_pct, 0.025), 
            q2 = quantile(downtime_pct, 0.975),
          mediana = median(downtime_pct), devest = sd(downtime_pct))

shapiro.test(downtime_pct)
## h0: Los datos no provienen de una distribución distinta a la normal
## h1: Los datos provienen de una distribución distinta a la normal
## La diferencia entre la media y la mediana es cercana a cero. Esto sugiere
## que la distribución de los datos es simetrica, por ende Se realiza la prueba
## de Shapiro-Wilk de normalidad y el resultado es un p-value = 0.08. Se 
## concluye que con un nivel de confianza del 95%, no hay evidencia suficiente 
## para rechazar la hipotesis nula. Por lo tanto, el porcentaje de Downtime del sistema no 
## proviene de una distribución distinta a la normal. Esto es una demostración 
## empirica del teorema del limite central.  

## Estadisticas 2
falla_df %>% 
  count(causa_falla) %>% 
  mutate(pct = n/sum(n))

falla_df %>% 
  group_by(causa_falla) %>% 
  summarise(mttf = mean(t_falla), q1 = quantile(t_falla, 0.025), q2 = quantile(t_falla, 0.975),
            mediana = median(t_falla), devest = sd(t_falla))

falla_df %>% 
  summarise(mttf = mean(t_falla), q1 = quantile(t_falla, 0.025), q2 = quantile(t_falla, 0.975),
            mediana = median(t_falla), devest = sd(t_falla))





