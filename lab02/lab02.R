rm(list=ls())
met <- read.csv('resAg.csv', header = T)
read.table(file = "http://biecek.pl/MOOC/dane/koty_ptaki.csv", sep = ";", dec= ",", header = TRUE)

install.packages("gdata") 
#www.rstudio.com/products/rpackages
library(gdata)
piwa <- read.xls("PiwaWyniki.xlsx", 2)
piwa [3 ,10]
piwa [ ,5]
piwa [2 ,]

class(piwa)
piwa$Tester
piwa$Rodzaj[30]
summary(piwa$Ocena)
summary(piwa)
install.packages("SmarterPoland")
library(SmarterPoland)
tsdtr210 <- getEurostatRCV("educ_iste")
head(tsdtr210,3)
summary(tsdtr210)

#http://ggplot2.tidyverse.org/reference/ggplot.html
ggplot ( data = tsdtr210 [ tsdtr210 $ geo == " PL " ,] ,
         aes (x = time , y = value, group = vehicle , colour = vehicle )) 
         + geom_line ()

save(piwa, file="PierwszeWyniki.rda")
load("PierwszeWyniki.rda")
write.csv(piwa,file="piwa.csv")

met <- read.csv('resAg.csv')
summary(met)

met2 = read.csv("resAg.csv", header = TRUE ,
                 colClasses = c ( " character " , " factor " , " factor " ,
                                  " numeric " , " numeric " , " numeric " , " numeric " ,
                                  " numeric " , " numeric " , " numeric " , " numeric " ,
                                  " numeric " , " numeric " , " numeric " , " numeric " ,
                                  " numeric " , " numeric " , " numeric " ))
summary(met2)



a = sample(c(1, 2, 4), 1000, rep = TRUE,
             prob = c (0.5 ,0.25 , 0.25))
mean(a)
Mode <- function(x) {
  ux <- unique(x)
  ux [ which.max(tabulate(match(x, ux)))]
}
Mode(a)


a = rexp(1000000, 2)
ld = mean(a) - sd(a)
lg = mean(a)+ sd(a)
sum(a > ld & a < lg )
a = runif(1000000)
ld = mean(a) - sd(a)
lg = mean(a)+ sd(a)
sum (a > ld & a < lg )

a = 100
x = 1:1000
eps = rnorm (1000)
y = x + a*eps
plot(x, y)
cor(x, y)

a = 100
x = 1:1000
eps = rnorm(1000)
y = x^2+a^2*eps
plot(x, y)
cor(x, y)

a = 0.1
x = seq (0, 1, 0.001)
eps = rnorm (1001)
y = ifelse(runif(1001) >0.5 , 1 , -1) * sqrt(1 - x^2) + a*eps
plot(x, y)
cor(x, y)

#http://tylervigen.com/spurious-correlations

hist(met$mean_Noise)
hist(met$mean_SA)
hist(met$mean_TA)
hist(met$mean_Blockiness)
hist(met$mean_Blockloss)
hist(met$mean_Blur)

#Bledny przyklad dopasowania:
install.packages ("fitdistrplus")
library(fitdistrplus)
met <- read.csv('resAg.csv')
fitdistr(met$mean_Blur,'normal')
ks.test(met$mean_Blur, 'pnorm', mean=9.63, sd=5.61)

#Proszę dopasować rozkład na podstawie tego tutoriala: https://www.r-bloggers.com/fitting-distributions-with-r/

#Testowanie hipotez
prop.test(12, 35, p=0.5)