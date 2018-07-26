library(tidyverse)
library(ggplot2)
library(GGally)
library(corrplot)

### Import .csv
data_gr <- read.csv(file = 'data_esame_2018.csv')

### Generazione grafico esplorativo
ggplot() +
  geom_density(data = data_gr, aes(x = diam, fill = thinned), alpha = 2/3) +
  labs(x = "Diametro", y = "Densità") +
  theme_bw()
#   geom_density(data = data_gr, aes(x = diam, fill = thinned),
#                color = "yellow", alpha = 0.7) +
#   # geom_density(data = data_gr, aes(x = h, fill = taxa),
#   #              color = "green", alpha = 0.5) +
#   # geom_density(data = data_gr, aes(x = h, fill = thinned),
#   #              color = "grey", alpha = 0.5) +

### Metriche 
data_gr %>%
  group_by(thinned) %>%
  summarise(max_values = max(h),
            mean_values = mean(h),
            standard_deviations = sd(h)) -> taxa_stat

print(taxa_stat)

## Correlazione lineare
pm = ggpairs(data = data_gr,
             columns = c(1:2),
             mapping = aes(alpha = 0.20, color = thinned),
             title = "Grafico correlazione lineare diametro e altezza su trattamento colturale") + theme_bw()
print(pm)

### Correlazione lineare
pm = ggpairs(data = data_gr,
             columns = c(1:2),
             mapping = aes(alpha = 0.20, color = taxa),
             title = "Grafico correlazione lineare diametro e altezza su specie") + theme_bw()
print(pm)

### P-Value e R^2
linearMod <- lm(diam ~ h, data = data_gr)  # build linear regression model on full data
print(linearMod)

summary(linearMod)
