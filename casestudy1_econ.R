#Questão 2 - Grupo 7

#Inicialmente, baixaremos a base de dados proposta:

library(readxl)
CS_1 <- read_excel("C:/Users/JúliaGonçalves/Downloads/CS_1.xlsx")
View(CS_1)
summary(CS_1)

#Agora, para que possamos demonstrar as médias entre o grupo de tratado e controle para todas
#as variáveis presente na base de dados, rodaremos o t.test de cada uma: 

t.test(Criancas_frequentam~auxilio, data=CS_1) 

t.test(Numero_pessoas~auxilio, data=CS_1) 

t.test(Escolaridade~auxilio, data=CS_1)

t.test(Renda~auxilio, data=CS_1)

t.test(Num_criancas~auxilio, data=CS_1)

t.test(Cor_maioria~auxilio, data=CS_1)

t.test(id_familia~auxilio, data=CS_1)


#Questão 3 - Grupo 7 

#Para que possamos realizar o histograma da Renda, abriremos o pacote 
#ggplo2

library(ggplot2)

#Agora, para rodar o histograma, usaremos o comando a seguir: 

ggplot(data=CS_1, aes(Renda)) + 
  xlab("Distribuição de Renda") +
  ylab("Número de Famílias") +
  geom_histogram(bins = 50) +
  labs(col = 'gray0', title = "Histograma de Renda")+
  theme(plot.title = element_text(hjust = 0.5))

# Questão 06 - Grupo 7 

#Para começar, baixaremos os pacotes a seguir:

library("dplyr")
library("sandwich")
library("lmtest")
library("MatchIt")
data("lalonde")


#Demonstraremos a a correspondência do vizinho mais próximo (NN),
#onde cada unidade tratada é emparelhada com uma unidade de controle disponível com 
#escore de propensão mais próximo. Para isso, argumentamos selecionando o método logit:

match <- matchit(auxilio ~ Numero_pessoas + Escolaridade + Num_criancas + Cor_maioria
                 + Renda,
                 distance = "glm", 
                 method = "nearest", 
                 data = CS_1) 

# Visualizando os resultados:
summary(match, UN = FALSE)

#Agora, salvaremos os pares feitos através do argumento a seguir:

match_df <- match.data(match)

View(match_df)

# Como forma de observar as dimensões desse frame:

dim(match_df)

summary(match_df)

#Agora, computaremos os gráficos de densidade dos escores de propensão dos grupos de tratamento 
#e controle, como forma de analisar a similaridade das distribuições: 


plot(match, type = "hist")

plot(match, type = "jitter", interactive = FALSE)


#Calcularemos a regressão das variáveis independentes, com base na amostra de tratado e controle. 
#Para isso, computaremos o efeito do auxilio na permanência das crianças na escola: 

Reg_1 <- lm(Criancas_frequentam ~ auxilio + Numero_pessoas + Escolaridade + Num_criancas + Cor_maioria
            + Renda, 
            data=match_df, weights =weights)

# Para ter um resumo da regressão: 
coeftest(Reg_1, vcov. = vcovCL, cluster = ~subclass)


# Questão 07 - Grupo 7 

#Para realizar essa comparação, computaremos a regressão MQO:

Reg_MQO <- lm(Criancas_frequentam ~ auxilio + Numero_pessoas + Escolaridade + Num_criancas + Cor_maioria
              + Renda, 
              data = CS_1)

# Para ter um resumo de Reg_MQO:
summary(Reg_MQO)