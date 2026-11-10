


# definicion factoriales y iveles

altura <- c(10, 15, 20)
longitud <- c(40, 60)
masa <- c(0, 20)

# diseño factorial

diseno <- expand.grid(
  altura = altura,
  longitud = longitud,
  masa = masa
)

# combinaciones
print(diseno)

####### borrar despues ######
# verificar numero de tratamientos
nrow(diseno)



# hacer replicas por tratamiento

experimento <- diseno[
  rep(1:nrow(diseno), each = 3),
]

# identificar cada tratamiento y replica
experimento$tratamiento <- rep(1:12, each = 3)
experimento$replica <- rep(1:3, times = 12)


########## borrar despues ########
# verificar las 36 corridas
print(experimento)
nrow(experimento)



# 4. aletorizacion 

set.seed(25)

# orden aleatorio
experimento <- experimento[
  sample(1:nrow(experimento)),
]

rownames(experimento) <- NULL

experimento$orden <- 1:nrow(experimento)

experimento <- experimento[
  , c("orden", "tratamiento", "replica",
      "altura", "longitud", "masa")
]

# mostrar las 36 corridas aleatorizadas
print(experimento)

######## borrar despues ########
# comprobar que seguimos teniendo 36 corridas
nrow(experimento)



### aca se usa el csv de resultados de nuestro experimento 

write.csv(
  experimento,
  file = "diseno_experimental.csv",
  row.names = FALSE
)

print("Diseno experimental guardado correctamente")
