

pop=10
med=100
vari=10
n.sites= 3

sites= rep(1:n.sites,ceiling( nrow(data)/n.sites))
sites= sites[1:nrow(data)]
sites


males=rnorm(pop, med, vari)
data=data.frame(males=males)
data$sites= sample(sites, replace = F)



# Define limites mínimos e máximos para as probabilidades
min_prob <- 0.05  # 5% de chance mínima
max_prob <- 0.50   # 50% de chance máxima

# Normaliza as condições, ajustando para os limites
prob_normalizada <- (males - min(males)) / (max(males) - min(males))  # Escala 0-1
prob_ajustada <- min_prob + prob_normalizada * (max_prob - min_prob)  # Escala min_prob - max_prob

# Garante que soma = 1 (opcional, pode normalizar novamente)
prob_final <- prob_ajustada / sum(prob_ajustada)
sum(prob_final)
data.frame(
  Individuo = nomes,
  Condicao = condicoes,
  Probabilidade = round(prob_final, 3)
)



ajustar_probabilidades <- function(condicao, min_prob = 0.05, max_prob = 0.50) {
  # Normaliza para escala 0-1
  prob_normalizada <- (condicao - min(condicao)) / (max(condicao) - min(condicao))
  # Ajusta para o intervalo [min_prob, max_prob]
  prob_ajustada <- min_prob + prob_normalizada * (max_prob - min_prob)
  # Garante que soma = 1 (normalização final)
  prob_final <- prob_ajustada / sum(prob_ajustada)
  return(round(prob_final, 3))
}

resultado <- data %>%
  group_by(sites) %>%
  mutate(
    probabilidade = ajustar_probabilidades(condicao=males, min_prob = 0.05, max_prob = 0.50)
  ) %>%
  arrange(sites, desc(probabilidade))


