## Mengeset direktori kerja (working directory). Tujuan mengeset direktori
## kerja ini adalah agar data dan skrip R tersimpan dalam folder yang mudah
## diatur

## setwd("D:\\my-campus\\catatan-kuliah\\Analisis_Regresi_Lanjut\\workdir\\R\\regresi_linear")

## Melihat direktori kerja saat ini
getwd()

## Melihat objek apa saja yang sudah ada pada direktori kerja. Kalau ada file yang tidak
## diperlukan dihapus saja
ls()

## Menghapus semua objek yang tidak diperlukan
rm(list=ls(all=TRUE))

## Sekarang dicek lagi
ls()

## Memasukkan data savings. Pilihan header = T diperlukan karena kita akan
## menggunakan header yang ada pada data
savings = read.csv("savings.csv",header=T)
## Perintah head dapat digunakan untuk memerikan nama header data
head(savings)

## Melakukan regresi linear berganda. Objek dengan nama savings.lm
## berisikan model regresi linear berganda yang akan kita lakukan. Fungsi
## lm adalah kependekan dari linear model
savings.lm = lm(sr~ pop15+pop75+dpi+ddpi,data=savings)
## Selanjutnya melihat ringkasan analisis regresi yang telah dilakukan
summary(savings.lm)

## Melakukan seleksi variabel
library(MASS)
step.savings = stepAIC(savings.lm,direction="both") # seleksi stepwise dua arah
step.savings$anova

## Seleksi maju
step.savings.forward = stepAIC(savings.lm,direction="forward")
step.savings.forward$anova

## Seleksi mundur
step.savings.backward = stepAIC(savings.lm,direction="backward")
step.savings.backward$anova

## Regresi subhimpunan
library(leaps)
subset.savings = regsubsets(sr~ pop15+pop75+dpi+ddpi,data=savings)
summary.savings = summary(subset.savings)
summary.savings
summary.savings$which # matriks logika
summary.savings$rsq # R^2 masing-masing model
summary.savings$rss # Jumlah kuadrat sisaan
summary.savings$cp #Cp Mallows
summary.savings$adjr2 # R^2 yang disesuaikan
summary.savings$bic # BIC
summary.savings$outmat


## Model terpilih (tanpa ddi)
savings.lm.new = lm(sr~pop15+pop75+ddpi,data=savings)
summary(savings.lm.new)


## Model tanpa pop15

## Model terpilih (tanpa pop75 dan ddi)
savings.lm.new2 = lm(sr~pop15+ddpi,data=savings)
summary(savings.lm.new2)

## Model diagnostik
plot(savings.lm.new2)

## Plot suaian versus sisaan
fit.savings = fitted(savings.lm.new2)
res.savings = residuals(savings.lm.new2)
plot(fit.savings,res.savings,xlab="Suaian",ylab="Sisaan")

## Uji kenormalan sisaan
shapiro.test(res.savings)

## Mengecek asumsi korelasi serial

n.res = length(res.savings)
plot(tail(res.savings,n.res-1)~head(res.savings,n.res-1),xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
abline(h=0,v=0,col="red",lty=3)


## Mengecek nilai h topi
hatv.savings = hatvalues(savings.lm.new2)
hatv.savings
head(hatv.savings)
sum(hatv.savings)

## Cek nilai ungkit apakah lebih dari 2*p/n
hatv.savings > 2*3*50

## Statistik Durbin-Watson
library(lmtest)
dwtest(sr~pop15+ddpi,data=savings)

## Data baru
data.baru = data.frame(pop15=25,ddpi=6)
predict(savings.lm.new2,newdata=data.baru)

data.baru = data.frame(pop15=c(25,20,29),ddpi=c(6,7,6))
predict(savings.lm.new2,newdata=data.baru)

data.baru = data.frame(pop15=c(23,20,25,30),ddpi=c(5,7,8,9))
predict(savings.lm.new2,newdata=data.baru)
