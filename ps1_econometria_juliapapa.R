## ECONOMETRIA - PROBLEM SET 1
## Júlia Papa Gonçalves Mão Cheia (C353125)

## EXERCÍCIO 1

## C -  estimate the effect of education ("educ") 
## on the log of wage ("lwage"). Interpret the result.

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
