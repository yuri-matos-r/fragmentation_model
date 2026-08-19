library(raster)
library(ggplot2)
library(dplyr)

# Definir o tamanho do grid
n_rows = 10
n_cols = 10

# Número de indivíduos
n_individuals = 10

# Criar um grid vazio
environment_grid = matrix(0, nrow = n_rows, ncol = n_cols)


# Visualizar o grid
par(mar=c(4,4,1,1))
image(environment_grid, col = terrain.colors(10))

?expand.grid
# Criar um dataframe a partir da matriz environment_grid
grid_df = expand.grid(x = 1:n_cols, y = 1:n_rows)
grid_df$value = as.vector(environment_grid)  # Transformar a matriz em um vetor


# Posições aleatórias no grid
individuals = data.frame(
  id = 1:n_individuals,
  x = sample(1:n_cols, n_individuals, replace = T),
  y = sample(1:n_rows, n_individuals, replace = T)
)



ggplot() +
  geom_tile(data = grid_df, aes(x = x, y = y, fill = value), color = "white") +  # Camada do grid
  geom_point(data = individuals, aes(x = x, y = y), color = "blue", size = 3) +  # Camada dos indivíduos
  scale_fill_gradient(low = "white", high = "green") +  # Escala de cores para o grid
  theme_minimal() +
  labs(title = "Ambiente Espacial", x = "Coluna", y = "Linha")

# Função para mover indivíduos
individuals$initial.x=individuals$x
individuals$initial.y=individuals$y

#Comandos iniciais pra gerar variação de position. Usamos eles como base pra criar a função position abaixo
#individuals$x=individuals$x+sample(-1:1, nrow(individuals), replace=T)
#individuals$y=individuals$y+sample(-1:1, nrow(individuals), replace=T)
#individuals$x=ifelse(individuals$x<1, 1, individuals$x)
#individuals$x=ifelse(individuals$x>nrow(individuals), nrow(individuals), individuals$x)
#individuals$y=ifelse(individuals$y<1, 1, individuals$y)
#individuals$y=ifelse(individuals$y>nrow(individuals), nrow(individuals), individuals$y)

position=function(position.x, position.y, n){
  new.x=position.x+sample(-1:1, n, replace=T)
  new.y=position.y+sample(-1:1, n, replace=T)
  new.x=ifelse(new.x<1, 1, new.x)
  new.x=ifelse(new.x>n, n, new.x)
  new.y=ifelse(new.y<1, 1, new.y)
  new.y=ifelse(new.y>n, n, new.y)
  return(data.frame(x = new.x, y = new.y))
}


resultado = position(individuals$x, individuals$y, n = nrow(individuals))
individuals$x = resultado$x
individuals$y = resultado$y

ggplot() +
  geom_tile(data = grid_df, aes(x = x, y = y, fill = value), color = "white") +  # Camada do grid
  geom_point(data = individuals, aes(x = x, y = y), color = "blue", size = 3) +  # Camada dos indivíduos
  scale_fill_gradient(low = "white", high = "green") +  # Escala de cores para o grid
  theme_minimal() +
  labs(title = "Ambiente Espacial", x = "Coluna", y = "Linha")


#Criar área de habitat que vai degradando pra não habitat

grid_df$hab=c('f')

frag.prob=0.2
grid_df$prob.frag=rbinom(nrow(grid_df), 1, frag.prob)

for(i in 1:nrow(grid_df)){
if(grid_df$prob.frag[i]==1) {grid_df$hab[i]='m'
} else {grid_df$hab[i]}
}

ggplot() +
  geom_tile(data = grid_df, aes(x = x, y = y, fill = hab), color = "white") +  # fill = hab
  geom_point(data = individuals, aes(x = x, y = y), color = "blue", size = 3) +
  scale_fill_manual(values = c("f" = "lightgreen", "m" = "grey")) +  # Escala manual para o fundo
  theme_minimal() +
  labs(title = "Ambiente Espacial", x = "Coluna", y = "Linha")

