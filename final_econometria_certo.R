## FINAL EXAM ECONOMETRIA
## JÚLIA PAPA GONÇALVES MÃO CHEIA (C353125)

## PARTE 3

# importando a base de dados "Prova_1"
library(readxl)
Prova_1 <- read_excel("C:/Users/JúliaGonçalves/Downloads/Prova_1.xlsx")
View(Prova_1)

## 1- Você quer estimar uma regressão linear para avaliar o impacto da educação
## de um indivíduo no logaritmo de seu salário, controlando pelo ano de nascimento.
## Estime a regressão proposta e interprete os coeficientes

reg <- lm(log_salario ~ Educ + Ano_nasc, data = Prova_1)
reg


## 5 - Estime a equação acima considerando o trimestre de nascimento como 
## variável instrumental e o ano do nascimento como um controle da regressão. 
## Interprete os coeficientes da regressão e compare com os resultados da 
## regressão por MQO, proposta no item 1.

# reg = MQO

library(ivreg)

reg5 <- ivreg(log_salario ~ Educ + Ano_nasc | Trime_nasc +
                Ano_nasc, data = Prova_1)
reg5

## 6 - Há alguma forma de provar que a variável sugerida é uma boa variável 
## instrumental? Se sim, as estime.

library(tidyverse)
library(knitr)
library(lavaan)
library(psych)
library(MBESS)

## analise dos dados

Prova_1 %>%
  headTail() %>%
  kable()

Prova_1 %>%
  select(log_salario, Ano_nasc, Trime_nasc) %>%
  pairs.panels()

summary(Prova_1)

mod1<-"# a path
Trime_nasc ~ a * log_salario

# b path
Ano_nasc ~ b * Trime_nasc

# c prime path
Ano_nasc ~ cp * log_salario

# indirect and total effects
ab := a * b
total := cp + ab"

fseml <- sem(mod1, data = Prova_1)
summary(fsem1, standardized = TRUE)

mediate(Ano_nasc ~ log_salario + (Trime_nasc), data = Prova_1)
mediate(Ano_nasc ~ log_salario + (Trime_nasc), data = Prova_1,
        n.iter = 1000) %>% 
  print(short = FALSE)