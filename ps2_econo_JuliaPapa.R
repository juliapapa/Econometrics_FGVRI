## PROBLEM SET 2 - ECONOMETpnadRIA
## Júlia Papa Gonçalves Mão Cheia (C353125)

## EXERCÍCIO 1 
## ITEM B

pnad <- read.csv("C:/Users/JúliaGonçalves/Downloads/pnad.csv", 
                 sep = ";", header = TRUE)
View(pnad)

library(plm)
reg1b <- plm(formula = larrecad ~ lpib+lprop_serv+lpop+a2007,data=pnad,
             index=c("cod_munic","ano"), model = "within")
summary(reg1b)

## ITEM C

reg1c <- lm(larrecad ~ lpib + lprop_serv + lpop + a2007, data = pnad)
summary(reg1c)

## EXERCÍCIO 3

fertil <- read.csv("C:/Users/JúliaGonçalves/Downloads/Fertil.csv", 
                 sep = ";", header = TRUE)
View(fertil)

## ITEM A

library(ivreg)
reg3a <- lm(children ~educ + age + agesq, data = fertil)
summary(reg3a)

## ITEM B

reg3b <- lm(educ ~ age + agesq + frsthalf, data = fertil)
summary(reg3b)
reg3b2 <- lm(children ~ educ + agesq + frsthalf, data = fertil)
summary(reg3b2)

## ITEM C

reg3c <- ivreg(children ~ educ + age +agesq | frsthalf + age + agesq,
               data = fertil)
summary(reg3c)


## ITEM D 

reg3d <- lm(children ~ educ + age + agesq + electric +tv +
              bicycle, data = fertil)
summary(reg3d)

reg3d2 <- ivreg(children ~ educ + age + agesq + electric + tv +
                  bicycle | frsthalf + age + agesq + electric + tv +
                  bicycle, data = fertil)

summary(reg3d2)

## EXERCÍCIO 8

library(readxl)
Mackinnon <- read_excel("C:/Users/JúliaGonçalves/Downloads/Mackinnon.xlsx")
View(Mackinnon)
 
library(tidyverse)
library(knitr)
library(lavaan)
library(psych)
library(MBESS)

Mackinnon %>%
  headTail() %>%
  kable()

Mackinnon %>%
  select(room_temp, thirst, consume) %>%
  pairs.panels

summary(Mackinnon)

mod1<-"# a path
thirst ~ a * room_temp

# b path
consume ~ b * thirst

# c prime path
consume ~ cp * room_temp

# indirect and total effects
ab := a * b
total := cp + ab"

fseml <- sem(mod1, data = Mackinnon)
summary(fsem1, standardized = TRUE)

mediate(consume ~ room_temp + (thirst), data = Mackinnon)
mediate(consume ~ room_temp + (thirst), data = Mackinnon,
        n.iter = 1000) %>% 
  print(short = FALSE)