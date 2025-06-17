## Codigo: Exemplos de aplicacao do teste K-S:

# Gerando dados hipoteticos

n <- 1000
x <- rpois(n, lambda=3)

## Para a distribuicao Qui^2_9:
# Passo 1:
x.ord <- sort(x)

# Passo2:
p.chi <- pchisq(x.ord, df = 3)

# Passo 3:

p.ecdf <- ecdf(x.ord)
p <- p.ecdf(x.ord)

# Passo 4:

dif <- abs(p.chi-p)

# Passo 5:

maximo <- max(dif) 

# Valor tabelado:

alfa <- 0.05
n <- length(x) 
Dtab <- sqrt(-log(alfa/2)/(2*n))

# Decisao:

if(maximo>Dtab){
  print("Rejeita H0!")
}else{
  print("Não rejeita H0!")
}


## Comparação da acumulada empirica com
## teorica



plot(p.ecdf)
x.aux <- seq(0,11, length.out=2000)
lines(x.aux, pchisq(x.aux, df = 3), col="red")


## Teste com a funcao do R:

ks.test(x = x, "pchisq", 3)



## Teste para uma Poisson(3):

# Passo 1:
x.ord <- sort(x)

# Passo2:
p.teo <- ppois(x.ord, lambda = 3)

# Passo 3:

p.ecdf <- ecdf(x.ord)
p <- p.ecdf(x.ord)

# Passo 4:

dif <- abs(p.teo-p)

# Passo 5:

maximo <- max(dif) 

# Valor tabelado:

alfa <- 0.05
n <- length(x) 
Dtab <- sqrt(-log(alfa/2)/(2*n))

# Decisao:

if(maximo>Dtab){
  print("Rejeita H0!")
}else{
  print("Não rejeita H0!")
}


## Comparação da acumulada empirica com
## teorica

plot(p.ecdf)
x.aux <- seq(0,11, length.out=2000)
lines(x.aux, ppois(x.aux, lambda = 3), col="red")


## Teste com a funcao do R:

ks.test(x = x.ord, "ppois", lambda=3)




## Teste para uma Gama(3,2):

# Passo 1:
x.ord <- sort(x)

# Passo2:
p.teo <- pgamma(x.ord, shape =  3, rate = 2)

# Passo 3:

p.ecdf <- ecdf(x.ord)
p <- p.ecdf(x.ord)

# Passo 4:

dif <- abs(p.teo-p)

# Passo 5:

maximo <- max(dif) 

# Valor tabelado:

alfa <- 0.05
n <- length(x) 
Dtab <- sqrt(-log(alfa/2)/(2*n))

# Decisao:

if(maximo>Dtab){
  print("Rejeita H0!")
}else{
  print("Não rejeita H0!")
}


## Comparação da acumulada empirica com
## teorica

plot(p.ecdf)
x.aux <- seq(0,11, length.out=2000)
lines(x.aux, pgamma(x.aux, shape = 3, rate = 2), col="red")


## Teste com a funcao do R:

ks.test(x = x.ord, "pgamma", 3, 2)


