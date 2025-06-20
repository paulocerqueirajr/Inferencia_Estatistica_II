## Codigo: Aula de exercícios testes especificos
## Autor: Paulo Cerqueira Jr.
## Data: 18/06/2025


##--- Carregando os dados

dados <- readxl::read_xlsx("dadosInf2_ex.xlsx")

# dados <- read.csv("dados.csv", header = TRUE, 
#                   sep=",")

#write.csv(dados, file = "dados.csv")

# 1. O peso médio antes do tratamento difere entre
# os participantes do grupo **Controle** e do grupo 
# **Tratamento**?
  
# Teste t amostra independentes.

# Pressupostos:
# Normalidade
# Igualdade de variâncias

# Verificando a normalidade:

par(mfrow=c(1,2))
hist(dados$Peso_Antes[dados$Grupo=="Tratamento"],
     xlab="Tratamento")
hist(dados$Peso_Antes[dados$Grupo=="Controle"],
     xlab="Controle")

mediat <- mean(dados$Peso_Antes[dados$Grupo=="Tratamento"])
mediac <- mean(dados$Peso_Antes[dados$Grupo=="Controle"])
sdt <- sd(dados$Peso_Antes[dados$Grupo=="Tratamento"])
sdc <- sd(dados$Peso_Antes[dados$Grupo=="Controle"])

tabela <- cbind(c(mediat, mediac), c(sdt, sdc))
rownames(tabela) <- c("Tratamento", "Controle")
colnames(tabela) <- c("Média", "Desvio-Padrão")
tabela

ks.test(dados$Peso_Antes[dados$Grupo=="Tratamento"],
        "pnorm",mediat, sdt)

ks.test(dados$Peso_Antes[dados$Grupo=="Controle"],
        "pnorm",mediac, sdc)

## Teste de F para igualdade de variâncias

var.test(Peso_Antes ~ Grupo, data = dados)


## Teste t com variâncias iguais


t.test(Peso_Antes ~ Grupo, data = dados, 
       var.equal=TRUE)


## 2. A média da pressão arterial 
## **antes do tratamento** difere entre os 
## **homens** e as **mulheres**?

# Pressupostos:
# Normalidade
# Igualdade de variâncias

# Verificando a normalidade:

par(mfrow=c(1,2))
hist(dados$Pressao_Antes[dados$Sexo=="Masculino"],
     xlab="Masculino")
hist(dados$Pressao_Antes[dados$Sexo=="Feminino"],
     xlab="Feminino")

mediat <- mean(dados$Pressao_Antes[dados$Sexo=="Masculino"])
mediac <- mean(dados$Pressao_Antes[dados$Sexo=="Feminino"])
sdt <- sd(dados$Pressao_Antes[dados$Sexo=="Masculino"])
sdc <- sd(dados$Pressao_Antes[dados$Sexo=="Feminino"])

tabela <- cbind(c(mediat, mediac), c(sdt, sdc))
rownames(tabela) <- c("Masculino", "Feminino")
colnames(tabela) <- c("Média", "Desvio-Padrão")
tabela

ks.test(dados$Pressao_Antes[dados$Sexo=="Masculino"],
        "pnorm",mediat, sdt)

ks.test(dados$Pressao_Antes[dados$Sexo=="Feminino"],
        "pnorm",mediac, sdc)


## Teste de F para igualdade de variâncias

var.test(Pressao_Antes ~ Sexo, data = dados)


## Teste t com variâncias iguais

t.test(Pressao_Antes ~ Sexo, data = dados, 
       var.equal=TRUE)


## 5. O peso médio dos participantes mudou após 
## o tratamento?


# teste t - amostras dependentes

dif <- dados$Peso_Antes - dados$Peso_Depois
par(mfrow=c(1,1))
hist(dif)

mediad <- mean(dif)
sdd <- sd(dif)

## Normalidade

ks.test(dif, "pnorm", mediad, sdd)


## Teste t:


t.test(dados$Peso_Antes, dados$Peso_Depois, paired=TRUE)

peso <- c(dados$Peso_Antes, dados$Peso_Depois)
grupo <- c(rep("Antes", length(dados$Peso_Antes)),
           rep("Depois", length(dados$Peso_Depois)))

boxplot(peso~grupo)
boxplot(dif)

## Final do código.
