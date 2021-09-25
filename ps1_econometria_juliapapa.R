## ECONOMETRIA - PROBLEM SET 1
## Júlia Papa Gonçalves Mão Cheia (C353125)


## EXERCÍCIO 1

## C -  estimate the effect of education ("educ") 
## on the log of wage ("lwage"). Interpret the result.

library(readxl)
wage <- read_excel("C:/Users/JúliaGonçalves/Downloads/wage.xlsx", 
                   col_types = c("numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", 
                                 "numeric"))

# Verificação se todas as variáveis são numéricas
summary(wage)
View(wage)

# Cálculo e análise da regressão
lm(lwage ~ educ, data = wage)
educwage <- lm(lwage ~ educ, data = wage)
summary(educwage)

# educ = beta1 chapéu --> um ano a mais de educação tem um impacto de 0,06 (log de 0,06)
# intercept = beta0 chapéu --> um indivíduo que não tem acesso a educação
# vai ter um salário médio de 6,51 (log de wage


## D - Does the result in item c) change if we add the variable 
## "women"in the regression

women <- lm(lwage ~ educ + women, data = wage)

summary(women)

# para uma mulher, o coeficiente (intercepto) é negativo 
# O salário médio para uma mulher sem acesso a educação é menor que dos
# homens sem acesso a educação --> - 0,04 < 0,06


## EXERCÍCIO 2

## A --> Explain guilty what ceteris paribus thought experiment
## is behind this question.

# ceteris paribus significa "mantido tudo o mais constante" --> analisar o 
# mesmo trabalhador, dado que ele esteja sobre as mesmas condições, antes
# e depois de um treinamento

# Yit = β0 + β1Di + β2Mit + β3Hit + £it

# Yit --> produção
# Di --> Treinamento
# Mit --> motivação
# Hit --> horas trabalhadas

## B --> Does it seem likely that the firm’s decision to train its workers is 
## independent of their characteristics? What are some of the observable and 
## unobservable characteristics of the workers who inhabit relate
## to productivity?

# Uma característica observável é a distância da casa dos trabalhadores até a 
# empresa. A demora de tempo para se deslocar pode prejudicar a produtividade
# dos trabalhos, visto que o trajeto pode gerar cansaço. Uma característica não
# observável é a motivação que um trabalhador tem para realizar suas tarefas. Por
# intrínseco em cada pessoa, não é possível mensurar. 

## C --> What factors other than worker characteristics can affect worker productivity

# Outros fatores são a remuneração, horário de trabalho, recursos, liderança.

## D --> If you found a positive correlation between production and training, 
## would you be convinced that training the most productive workers? Explain.
  
# Não convenceria, pois correlação não implica causalidade. Assim, seria necessário
# garantir que é possível comparar os grupos de tratamento e controle.

## EXERCÍCIO 4

## A --> Why is this comparison not a good evaluation of the training program? 
## How would you explain this (in words) to Mr Manager?

# A aleatorização do programa é falha por ele ser voluntário. Assim, não existe
# um grupo de controle por existirem fatores exógenos e não-observáveis que podem
# causar o aumento salarial


## D --> Verify if the randomization was well done. In other words,
## if the control and treatment group are balanced.

# Primeiro --> verificar se a aleatorização foi bem feita --> Necessário
# fazer um teste T simples.
# É necessário verificar se o P-Valor é menor que 0.05 para ver se há uma 
# diferença significativa entre os grupos. Caso não haja significância,
# a aleatorização foi bem feita.

t.test(educ~treat, data = wage)
t.test(women~treat, data = wage)
t.test(exper~treat, data = wage)

# A aleatorização foi bem feita nos 3 casos

## E --> Regress lwage on educ, exper, expersq, women. Interpret 
## the results.

reg4e <- lm(lwage ~ educ + exper + expersq + women, data = wage)
summary(reg4e)

# educ --> varia 9% em 1 ano extra
# exper --> aparece duas vezes, não sendo possível interpretar do jeito
# que ela aparece

# salario = β0 + β1educ + β2exp + β3exp2 + women
# dsalario/dexp = β2 + 2β3exp

# exper impacta de forma não-linear. O impacto dela vai ser igual
# derivada dsalario/dexp

## F --> Analyze the experiment’s results using as outcomes wage 
## and employment status.

reg4f1 <- lm(wage ~ treat, data = wage)
summary(reg4f1)

# O tratamento apresentou um aumento médio de 216,17 no lwage; A 
# diferença entre pessoas tratadas e não tratadas no lwage é de 216,17

reg4f2 <- lm(emp ~ treat, data = wage)
summary(reg4f2)

# Pessoas com tratamento estão 16% mais empregadas. Assim, a diferença
# delas com pessoas não tratadas é de 16%

## G --> Which assumptions must hold to interpret the previous item’s
## results as the average treatment effect?

# O resultado que é necessário é o da aleatorização funcional. Assim,
# Ti deve ser independente dos resultados potenciais.

## EXERCÍCIO 7

## A --> Proceed a balance check on pre-treatment variables (i.e. for 
## each pre-treatment variable, compute its mean and standard error 
## for treatment and control groups and test if the difference in
## means between groups is statistically significant).

nsw <- read_excel("C:/Users/JúliaGonçalves/Downloads/nsw.xlsx")

head(nsw)
summary(nsw)

# será feito um teste T para cada variável e conferir a diferença de 
# médias de desvio padrão 

t.test(age ~ treat, data = nsw)
t.test(black ~ treat, data = nsw)
t.test(hisp ~ treat, data = nsw)
t.test(married ~ treat, data = nsw)
t.test(re74 ~ treat, data = nsw)
t.test(re75 ~ treat, data = nsw)
t.test(u74 ~ treat, data = nsw)
t.test(u75 ~ treat, data = nsw)

## nao foi encontrada nenhuma diferença estatística entre as médias

## B --> Estimate the average treatment effect of the training program 
## (without covariates). Is it statistically different from zero?
## Interpret the results.

reg7b <- lm(re78 ~ treat, data = nsw)
summary(reg7b)  

# A variavel de tratamento é estatisticamente significante e as pessoas 
# que fizeram tratamento tem 1794 reais a mais no salário do que as 
# que não fizeram

## C -->  Estimate the average treatment effect of the training program,
## including pre-treatment covariates as control variables. Did the results change?

reg7c <- lm(re78 ~treat + age + educ + black +hisp +married +
              re74 + re75 + u74 + u75, data = nsw)
summary(reg7c)

# As variaveis educ e black são as que mais afetam o salário após tratamento e são
# as únicas que tem significância estatísta

## D -->  Proceed a balance check on pre-treatment variables and discuss the results.

#rodar teste t para todas as variaveis
# os dados não-numericos foram tratados no excel

psid <- read_excel("C:/Users/JúliaGonçalves/Downloads/psid.xlsx", 
                           col_types = c("numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric"))
View(psid)

t.test(age ~ treat, data = psid)
t.test(black ~ treat, data = psid)
t.test(hisp ~ treat, data = psid)
t.test(married ~ treat, data = psid)
t.test(re74 ~ treat, data = psid)
t.test(re75 ~ treat, data = psid)
t.test(u74 ~ treat, data = psid)
t.test(u75 ~ treat, data = psid)

# A aleatorização foi mal feita para as variáveis idade e cor de pele, pois
# as médias são bem diferentes entre os grupos de controle e tratado. A variavel
# hisp não possui resultados estatisticamente signficantes.

## E --> Test whether the average post-treatment earnings difference between treated
## and control individuals is statistically different from zero. By doing this, are you
## assessing the average treatment effect? Why?

reg7e <- lm(re78 ~ treat, data=psid)
summary(reg7e)

# Por conta da aleatorização não foi bem feita, não é possível zerar o
# viés de seleção. Assim, a diferença entre as médias e o tratamento negativo
# é possível afirmar que o efeito do tratamento não é significativo
# Além disso,o balanceamento inadequado pode ter ocultado o efeito do tratamento.

## EXERCÍCIO 8

## A --> Using age, education, RE74 and RE75, construct a propensity score using
## the treated as the treatment variable. Report the average p-score for the treated
## and control samples, and plot the propensity score densities for the treatment and
## control groups.

Dehijia_Wahba <- read_excel("C:/Users/JúliaGonçalves/Downloads/Dehijia_Wahba.xlsx",
                            col_types = c("numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric"))
View(Dehijia_Wahba)

library(dplyr)
library(ggplot2)

Dehijia_Wahba <- Dehijia_Wahba[1:1000,]

reg8a <- lm(re78 ~ treat + age + education + re74 + re75, data = Dehijia_Wahba)
summary(reg8a)

betas <- glm(treat ~ age + education + re74 + re75, family = binomial, data = Dehijia_Wahba)

yestimado <- data.frame(pr_score =predict(betas, type = 'response'),
                        treat = betas$model$treat)

head(yestimado)
View(yestimado)

table1::table1(~pr_score| treat, data = yestimado)

yestimado %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = 'white') +
  facet_wrap(~treat)+
  xlab("Probability of Propensity Score")+
  theme_bw()

library(MatchIt)

mod_match <- matchit(treat ~ age + education + re74 + re75,
                     distance = 'glm',
                     method = 'nearest',
                     data = Dehijia_Wahba)

mod_match
summary(mod_match, un = FALSE)

# existem apenas 370 individuos pareados
# sera criada uma base com apenas individuos pareados

dta_m <- match.data(mod_match)
dim(dta_m)

plot(mod_match, type = 'hist')
plot(mod_match, type = 'jitter', interactive = FALSE)

# Como temos muito mais pessoas dentro do grupo de controle do que de tratado, 
# quando pareamos um para um a probabilidade de usarmos 
# todos os tratados e descartamos os controles é grande, como mostrado acima, 
# onde a densidade das unidades tratadas não pareadas é 0, enquanto a densidades 
# das unidades controle não pareadas é grande.

## B --> Compare this to a linear regression, using age, education, RE74 and RE75
## as controls. Compare the hypothesis behind each estimators.

# calculo do impacto do tratamento por meio de um MQO

library(lmtest)
library(sandwich)

reg8b <- lm(re78 ~ treat + age + education + re74 + re75,
            data = dta_m, weights = weights)

summary(reg8b)

# O tratamento teve um valor de impacto alto, em média, pessoas que foram 
# tratadas tem 1445 reais a mais em seus salários de que pessoas do grupo de
# controle, dado tudo mais constante. Além de que os resultados são significantes 
# ao nível de 0,05.