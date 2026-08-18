install.packages("raster")  # Para manipulação de dados espaciais
install.packages("ggplot2") # Para visualização
install.packages("dplyr")   # Para manipulação de dados

library(raster)
library(ggplot2)
library(dplyr)

# Definir o tamanho do grid
n_rows <- 10
n_cols <- 10

# Número de indivíduos
n_individuals <- 10

# Criar um grid vazio
environment_grid <- matrix(0, nrow = n_rows, ncol = n_cols)
?matrix

# Visualizar o grid
image(environment_grid, col = terrain.colors(10))

?matrix
?environment_grid
?image
?sample

# Criar um dataframe a partir da matriz environment_grid
grid_df <- expand.grid(x = 1:n_cols, y = 1:n_rows)
grid_df$value <- as.vector(environment_grid)  # Transformar a matriz em um vetor




# Posições aleatórias no grid
individuals <- data.frame(
  id = 1:n_individuals,
  x = sample(1:n_cols, n_individuals, replace = TRUE),
  y = sample(1:n_rows, n_individuals, replace = TRUE)
)

?geom_tile


ggplot() +
  geom_tile(data = grid_df, aes(x = x, y = y, fill = value), color = "white") +  # Camada do grid
  geom_point(data = individuals, aes(x = x, y = y), color = "blue", size = 3) +  # Camada dos indivíduos
  scale_fill_gradient(low = "white", high = "green") +  # Escala de cores para o grid
  theme_minimal() +
  labs(title = "Ambiente Espacial", x = "Coluna", y = "Linha")

# Função para mover indivíduos
move_individuals <- function(individuals, n_rows, n_cols) {
  individuals <- individuals %>%
    mutate(
      x = x + sample(-1:1, n(), replace = TRUE),
      y = y + sample(-1:1, n(), replace = TRUE)
    )
  
  # Garantir que os indivíduos permaneçam dentro do grid
  individuals$x <- pmin(pmax(individuals$x, 1), n_cols)
  individuals$y <- pmin(pmax(individuals$y, 1), n_rows)
  
  return(individuals)
}


# Número de passos de tempo
n_steps <- 20

# Loop de simulação
for (step in 1:n_steps) {
  individuals <- move_individuals(individuals, n_rows, n_cols)
  
  # Visualizar o estado atual
  print(ggplot() +
          geom_tile(data = grid_df, aes(x = x, y = y, fill = value), color = "white") +
          geom_point(data = individuals, aes(x = x, y = y), color = "blue", size = 3) +
          theme_minimal() +
          ggtitle(paste("Step", step)))
  
  Sys.sleep(1)  # Pausa para visualização
}



# Adicionar recursos ao ambiente
resource_grid <- matrix(sample(0:1, 1, replace = TRUE), nrow = n_rows, ncol = n_cols)
?sample
resource_df <- expand.grid(x = 1:n_cols, y = 1:n_rows)
resource_df$value <- as.vector(resource_grid)


#function# Função para mover indivíduos em direção aos recursos
move_towards_resources <- function(individuals, resource_df) {
  for (i in 1:nrow(individuals)) {
    x <- individuals$x[i]
    y <- individuals$y[i]
    
    # Encontrar a célula com o maior recurso nas proximidades
    neighbors <- expand.grid(x = (x-1):(x+1), y = (y-1):(y+1))
    neighbors <- neighbors %>%
      filter(x >= 1 & x <= n_cols & y >= 1 & y <= n_rows)
    
    best_cell <- neighbors[which.max(resource_df[as.matrix(neighbors)]), ]
    
    # Mover para a célula com o maior recurso
    individuals$x[i] <- best_cell$x
    individuals$y[i] <- best_cell$y
  }
  
  return(individuals)
}






# Função para mover indivíduos em direção aos recursos
move_towards_resources <- function(individuals, resource_df) {
  for (i in 1:nrow(individuals)) {
    x <- individuals$x[i]
    y <- individuals$y[i]
    
    # Encontrar a célula com o maior recurso nas proximidades
    neighbors <- expand.grid(x = (x-1):(x+1), y = (y-1):(y+1))
    neighbors <- neighbors %>%
      filter(x >= 1 & x <= n_cols & y >= 1 & y <= n_rows)
    
    # Encontrar o valor máximo de recurso nas células vizinhas
    best_cell <- neighbors[which.max(resource_df$value[as.matrix(neighbors)]), ]
    
    # Mover para a célula com o maior recurso
    individuals$x[i] <- best_cell$x
    individuals$y[i] <- best_cell$y
  }
  
  return(individuals)
}

# Função para mover indivíduos em direção aos recursos
move_towards_resources <- function(individuals, resource_df) {
  for (i in 1:nrow(individuals)) {
    x <- individuals$x[i]
    y <- individuals$y[i]
    
    # Encontrar a célula com o maior recurso nas proximidades
    neighbors <- expand.grid(x = (x-1):(x+1), y = (y-1):(y+1))
    neighbors <- neighbors %>%
      filter(x >= 1 & x <= n_cols & y >= 1 & y <= n_rows)
    
    # Encontrar o valor máximo de recurso nas células vizinhas
    best_cell <- neighbors[which.max(resource_df$value[as.matrix(neighbors)]), ]
    
    # Mover para a célula com o maior recurso
    individuals$x[i] <- best_cell$x
    individuals$y[i] <- best_cell$y
  }
  
  return(individuals)
}


# Número de passos de tempo
n_steps <- 20

# Loop de simulação
for (step in 1:n_steps) {
  individuals <- move_towards_resources(individuals, resource_df)
  
  # Visualizar o estado atual
  print(ggplot() +
          geom_tile(data = resource_df, aes(x = x, y = y, fill = as.factor(value))) +
          geom_point(data = individuals, aes(x = x, y = y), color = "blue", size = 3) +
          theme_minimal() +
          ggtitle(paste("Step", step)))
  
  Sys.sleep(1)  # Pausa para visualização
}



# Criar um grid 100x100 inicializado com um status (ex: 1 para habitat)
grid + matrix(1, nrow = 100, ncol = 100)

# Definir probabilidade de mudança para não-habitat
probabilidade_nao_habitat <- 0.3  # 30% de chance de virar não-habitat

# Aplicar a regra aleatória

grid_alterado <- matrix(
  sample(
    c(1, 0), 
    size = 100 * 100, 
    replace = TRUE, 
    prob = c(1 - probabilidade_nao_habitat, probabilidade_nao_habitat)
  ), 
  nrow = 100, 
  ncol = 100
)

# Visualizar o grid
image(grid_alterado, main = "Grid Habitat (1) vs Não-Habitat (0)",
      col = c("red", "green"), xlab = "Colunas", ylab = "Linhas")
legend("topright", legend = c("Não-Habitat", "Habitat"), 
       fill = c("red", "green"))
