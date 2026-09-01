install.packages('terra')
install.packages('faux')
library(terra)
library(faux)


#Determining landscape traits
h= 100 #landscape height
w= 100 #landscape width
percent.patch= 0.05 #initial proportion of defensible sites
n.pop= 300 #inital population size
mean.size= 50 #initial mean size of individuals
sd.size= 5 #standard deviation of the initial population mean size
r.quali.size= 0.8 #correlation between size and quality
r.energy.size= 0.8 #correlation between size and energy


r =rast(nrows = h, ncols = w, xmin = 0, xmax = w, ymin = 0, ymax = h)

?rast


#random determination of patches
n_total = ncell(r)
val = sample(c(rep(1, round(n_total * percent.patch)),
                    rep(0, n_total - round(n_total * percent.patch))))

values(r) = val

# Visualiza
plot(r, col = c( "darkgreen", "gold"), main = "Mapa de habitat")
legend("topright", legend = c("Campo (0)", "Floresta (1)"),
       fill = c("gold", "darkgreen"))



size= rnorm(n.pop,mean.size, sd.size)
quality= rnorm_pre(size, 100, 10, r.quali.size)
total.energy= rnorm_pre(size, 100, 10, r.energy.size)
fight.energy= 0.1*total.energy


data= data.frame(size=size, quality= quality, total.energy=total.energy, fight.energy=fight.energy )

data$x= sample(c(1:w), replace = T)
data$y= sample(c(1:h), replace = T)

data$x=data$x-0.5
data$y=data$y-0.5

data$patch = extract(r, cbind(data$x, data$y))[, 2]#não entendi






Resumo da estrutura recomendada
data$col  = índice inteiro da coluna (1–100) — use para lógica de movimento
data$row  = índice inteiro da linha  (1–100) — use para lógica de movimento
data$x    = col - 0.5 # use apenas para extract() e plot()
data$y    = row - 0.5 # use apenas para extract() e plot()
data$patch = habitat do indivíduo (0 ou 1)




