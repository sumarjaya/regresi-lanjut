## Sumber: https://www.statmethods.net/advstats/bootstrapping.html

# coba regresi tanpa bootstrap
reg.mtcars = lm(mpg~wt+disp,data=mtcars)
summary(reg.mtcars)
summary(reg.mtcars)$r.square

# Boostrap untuk selang kepercayaan 95% untuk R^2
library(boot)

# rsq adalah fungsi untuk menghitung R kuadrat dari data
rsq <- function(formula, data, indices) {
  d <- data[indices,] # memungkikan boot untuk memilih sampel
  fit <- lm(formula, data=d) # melakukan regresi linear 
  return(summary(fit)$r.square) # ambil nilai R^2 saja
}

# melakukan bootstrap dengan 1000 replikasi
results <- boot(data=mtcars, statistic=rsq,
   R=1000, formula=mpg~wt+disp)

# view results
results
summary(results)
plot(results)

##
## melakukan bootstrap terhadap koefisien regersi
##
## koefreg adalah fungsi untuk menghitung R kuadrat dari data
koefreg <- function(formula, data, indeks) {
  d <- data[indeks,] # memungkikan boot untuk memilih sampel
  fit <- lm(formula, data=d) # melakukan regresi linear 
  return(coefficients(fit)) # ambil koefisien
}

# melakukan bootstrap dengan 1000 replikasi
results <- boot(data=mtcars, statistic=koefreg,
   R=1000, formula=mpg~wt+disp)
results


## melakukan bootstrap regresi nonlinear
## data anemo

puro <- read.table("puromisin.txt",header=T) # nama data
puro.nls <- nls(kecepatan~beta1*substrat/(beta2 + substrat), start=list(beta1=205,beta2=0.08),data=puro)
summary(puro.nls)

# koefisien regresi nonlinear
koefreg.nonlinear <- function(formula, data, indeks) {
  d <- data[indeks,] # memungkikan boot untuk memilih sampel
  fit <- nls(formula, start=list(beta1=205,beta2=0.08), data=d) # melakukan regresi linear 
  return(coefficients(fit)) # ambil koefisien
}

# melakukan bootstrap dengan 1000 replikasi
results <- boot(data=puro, statistic=koefreg.nonlinear,
   R=1000, formula=kecepatan~beta1*substrat/(beta2 + substrat))
results

## melakukan boostrap terhadap regresi logistik 
##
Beetles = read.table("Beetles.dat",header=T) # header = T artinya header 
Beetles.logit = glm(y~x,family=binomial,data=Beetles)
summary(Beetles.logit)

# koefisien regresi nonlinear
koefreg.logistik <- function(formula, data, indeks) {
  d <- data[indeks,] # memungkikan boot untuk memilih sampel
  fit <- glm(formula, family=binomial,data=d) # melakukan regresi logistik, 
  return(coefficients(fit)) # ambil koefisien
}

# melakukan bootstrap dengan 1000 replikasi
results <- boot(data=Beetles, statistic=koefreg.logistik,
   R=1000, formula=y~x)
results




