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

position=function(position.x, position.y, n, grid){
  new.x=position.x+sample(-1:1, n, replace=T)
  new.y=position.y+sample(-1:1, n, replace=T)
  new.x=ifelse(new.x<1, 1, new.x)
  new.x=ifelse(new.x>max(grid[,1]), max(grid[,1]), new.x)
  new.y=ifelse(new.y<1, 1, new.y)
  new.y=ifelse(new.y>max(grid[,2]), max(grid[,2]), new.y)
  return(data.frame(x = new.x, y = new.y))
}


resultado = position(individuals$x, individuals$y, n = nrow(individuals), grid=grid_df)
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

frag.prob=0.1
grid_df$prob.frag=rbinom(nrow(grid_df), 1, frag.prob)

for(i in 1:nrow(grid_df)){
if(grid_df$prob.frag[i]==1) {grid_df$hab[i]='m' #m de matriz
} else {grid_df$hab[i]}
}

ggplot() +
  geom_tile(data = grid_df, aes(x = x, y = y, fill = hab), color = "white") +  # fill = hab
  geom_point(data = individuals, aes(x = x, y = y), color = "blue", size = 3) +
  scale_fill_manual(values = c("f" = "lightgreen", "m" = "grey")) +  # Escala manual para o fundo
  theme_minimal() +
  labs(title = "Ambiente Espacial", x = "Coluna", y = "Linha")


for(i in 1:nrow(individuals)){
individuals$point[i]=grid_df$hab[which(grid_df$x==individuals$x[i] & grid_df$y==individuals$y[i])]
}

surround=c()
for(i in 1:nrow(individuals)){
surround[i]=grid_df$hab[which(grid_df$x==individuals$x[i] & grid_df$y==individuals$y[i])]
}

neighborhood = expand.grid(x = -1:1, y = -1:1)

surround = matrix(NA, nrow = n_individuals, ncol = nrow(neighborhood))

for (i in 1:n_individuals) {#o primeiro loop roda seguindo a ordem dos individuos
  for (k in 1:nrow(deltas)) {#o segundo loop roda adicionando -1, 0 ou 1 da posicao
    x = individuals$x[i] + neighborhood$x[k]#nova posicao no x
    y = individuals$y[i] + neighborhood$y[k]#nova posicao no y
    if(x > max(grid_df$x)| y > max(grid_df$y)){
      val='m'} else 
        if(x < min(grid_df$x)| y < min(grid_df$y)) {
          val='m'} else {
            val = grid_df$hab[grid_df$x == x & grid_df$y == y]}
    surround[i, k] = val #o loop final coloca m na vizinhanca que for borda. No data frame produzido ao final, a coluna 5 de surround tem a posicao que o individuo ta localizado
  }
}


for (i in 1:n_individuals) {
individuals$movement[i]=if(all(surround[i,-5]=='f') & surround[i,5]=='m') {'stay'} else {'move'}
}

#Paramos na parte em que definimos se cada indivíduo fica ou move por estar ou não em uma mancha adequada cercada por mata. Próximo passo é fazer eles se moverem e quando um encontrar outri na mancha, brigar
