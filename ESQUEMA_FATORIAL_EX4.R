setwd("G:/Meu Drive/DOCUMENTOS/ANDRÉ/MESTRADO UFAC PPGCC/DISCIPLINAS/ESTATÍSTICA COMPUTACIONAL/ATIVIDADE ESQUEMA FATORIAL")
dados = read.table("A_FatCruz_ex4.txt", header = TRUE)
attach(dados)
str(dados)
FTR <- as.factor (dados$TR)
FA <- as.factor (dados$A)
FB <- as.factor (dados$B)
Resp <- dados$Y

ex04.m <- tapply(Resp, list(FA, FB), mean)
ex04.m

ex04.mr <- tapply (Resp, FA, mean)
ex04.mr

ex04.me <- tapply(Resp, FB, mean)
ex04.me

boxplot (Resp ~ FTR, col = c(1,2,3,4,5,6))

interaction.plot (FA, FB, Resp, col = c(2, 3))
interaction.plot(FB, FA, Resp, col = c(2,3))

ex04.av <- aov(Resp ~ FA  + FB + FA * FB, data=dados)
summary(ex04.av)

ex04.mt <- model.tables(ex04.av, ty="means")
ex04.mt

mod <- aov (Resp ~ FA + FB, data = dados)

#Colocar os Controles Locais, se é o caso
anova (mod); anv <- anova (mod)
cv <- 100 * sqrt(anv$"Mean Sq"[length(anv$"Mean Square")])
cv

r = residuals(mod)
bartlett.test(Resp~FTR)

ex04.avr <- aov(Resp ~ FA/FB)
summary(ex04.avr, split=list("FA:FB"=list(r1=1, r2=2, r3=3)))
summary.lm(ex04.avr)

ex04.ave <- aov(Resp ~ FA/FB)
summary(ex04.ave, split=list("FA:FB"=list(r1=1, r2=2, r3=3)))
summary.lm(ex04.ave)

require(ExpDes.pt)

fat2.dic(FA,FB,Resp,quali=c(TRUE,TRUE),mcomp="tukey",
         fac.names=c("FA","FB"),sigT = 0.05,
         sigF = 0.05, unfold=NULL)