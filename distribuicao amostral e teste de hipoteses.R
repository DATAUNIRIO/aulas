
#How to disable scientific notation
options(scipen=999)

# Passo 1
# H0: p = 0,5
# H1: p!=0,5

# Passo 2 
# definir o nivel 0,05

# Passo 3
# realizar o teste
binom.test(792, 1000,p = 0.5)

# Passo 4
# concluir
# A moeda é viciada

#------------------------------------------

# Passo 1
# H0: p = 0,5
# H1: p!=0,5

# Passo 2 
# definir o nivel 0,05

# passo 3
binom.test(530, 1000,p = 0.5)

#p-valor <= 0,05 Rejeito H0
#p-valor >  0,05 Não rejeito H0

Teste se podemos ter 54 caras em 100 lançamentos
# Passo 1
# H0: p = 0,5
# H1: p!=0,5

# Passo 2 
# definir o nivel 0,05

# passo 3
binom.test(54, 100,p = 0.5)

#p-valor <= 0,05 Rejeito H0
#p-valor >  0,05 Não rejeito H0

#Teste se podemos ter 7 caras em 10 lançamentos
# Passo 1
# H0: p = 0,5
# H1: p!=0,5

# Passo 2 
# definir o nivel 0,05

# passo 3
binom.test(7, 10,p = 0.5)

#p-valor <= 0,05 Rejeito H0
#p-valor >  0,05 Não rejeito H0

#Teste se podemos ter 4.950 caras em 10.000 lançamentos
# Passo 1
# H0: p = 0,5
# H1: p!=0,5

# Passo 2 
# definir o nivel 0,05
binom.test(4950, 10000,p = 0.5)
binom.test(4000, 10000,p = 0.5)



library(readxl)
populacao <- read_excel("C:/Users/Hp/Desktop/Base_de_dados-master/AMOSTRAS ALEATORIAS.xlsx", 
                        +     sheet = "AAS")

library(dplyr)
amostra2 <- sample_n(populacao,20)
amostra2 <- sample_n(populacao,20)

# Distribuição amostral
# https://www.statology.org/sampling-distribution-in-r/

set.seed(0)
#definir o numero de amostras
n = 5
n = 10000
n = 100000
#criar um vetor vazio de tamanho n
media_amostras = rep(NA, n)
#preencher o vetor vazio com as médias
for(i in 1:n){
  media_amostras[i] = mean(rnorm(100, mean=5.3, sd=9))
}
#olhar as primeiras médias das amostras
head(media_amostras)
#criar um histograma para visualizar a distribuição amostral
hist(media_amostras, main = "", xlab = "Médias das amostras", col = "steelblue")
#média da distribuição amostral
mean(media_amostras)
#desvio padrão da distribuição amostral
sd(media_amostras)

# https://openintro.shinyapps.io/CLT_mean/

#calculate probability that sample mean is less than or equal to 6
#sum(sample_means <= 6) / length(sample_means)

mean(Lucy$Income)
amostra<-sample_n(BigLucy,500)
mean(amostra$Income)


library(readxl)
dados <- read_excel("C:/Users/Hp/Desktop/Base_de_dados-master/Questionario_Estresse.xls")
load("C:/Users/Hp/Desktop/Base_de_dados-master/CARROS.RData")

#Horas_estudo =35
#Cred = 25
#Estresse= 25
#Desempenho=8

#library(TeachingDemos)
#sd(dados$Desempenho)
#z.test(dados$Desempenho,mu= 8.7, stdev=0.775319, conf.level = 0.95)

t.test(dados$Desempenho,mu= 8, data = dados)

# Testar o resultado 
#Horas_estudo =35
#Cred = 25
#Estresse= 25
#Desempenho =8

#Kmporlitro=20
#Preco=250
#HP=130
#Peso=3
#RPM=15


# Passo 1
# H0: Média Km/l = 20
# H1: Média Km/l != 20

# Passo 2 
# definir o nivel 0,05

# Passo 3
# realizar o teste
t.test(CARROS$Kmporlitro, mu= 20, data = CARROS)

#p-valor <= 0,05 Rejeito H0
#p-valor >  0,05 Não rejeito H0

# Passo 4
# concluir

#------------------------------------------

# Passo 1
# H0: Média Preço = 250
# H1: Média Preço != 250

# Passo 2 
# definir o nivel 0,05

# Passo 3
# realizar o teste
t.test(CARROS$Preco, mu= 250, data = CARROS)

#p-valor <= 0,05 Rejeito H0
#p-valor >  0,05 Não rejeito H0

# Passo 4
# concluir

#Passo 1
#H0: Média HP = 130
#H1: Média Hp != 130

#Passo 2 
#definir o nivel 0,05

# Passo 3
# realizar o teste
t.test(CARROS$HP, mu= 130, data = CARROS)

#p-valor <= 0,05 Rejeito H0
#p-valor >  0,05 Não rejeito H0

# Passo 4
# concluir






