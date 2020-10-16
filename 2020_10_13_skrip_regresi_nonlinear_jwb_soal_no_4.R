# memasukkan data soal nomor 4
soal.4 = read.table("soal_4.txt",header=T)
soal.4
# mengambil/mengakses nilai t dan Y pada soal.4 dan 
# mengkopi ke t dan Y
t = soal.4$t
Y = soal.4$Y
t
Y
# kita akan memplot data untuk melihat
# bagaimana t dan Y 
plot(t,Y,col="blue")

# Kita sudah mencoba, secara matematis, melinearkan
# Y.star = log(Y)
Y.star = log(Y)

# Sekarang kita akan meregresikan 
# Y.star~t-1

reg.soal.4 = lm(Y.star~t-1) # model regresi tanpa intercept
summary(reg.soal.4) # melihat ringkasan regresi

# Berdasarkan luaran kita perolah alfa topi  = -0.2043
# selanjutnya kita peroleh -teta = alfa. Atau teta = -alfa.
# Dengan demikian, teta = 0.2043. Ini akan kita gunakan sebagai
# nilai awal

teta.0 = 0.2043

# Kita akan menerapkan regresi nonlinear
nreg.soal.4 = nls(Y~exp(-teta*t),data=soal.4,start=list(teta=teta.0))
summary(nreg.soal.4)

# Meakses koefisien
coefficients(nreg.soal.4)

# Membuat garis regresi
x = t # perintah curve memerlukan nama variabel x :-(
curve(exp(-0.2069127*x),col="red",add=T)

# Inferensi

# Menghitung selang kepercayaan (confidence interval)
confint(nreg.soal.4)

#
nreg.4 = nreg.soal.4 
yhat.nreg.4 <- fitted(nreg.4) # nilai suaian (fitted value)
res.nreg.4 <- residuals(nreg.4) # nilai residual (sisaan)
plot(yhat.puro,res.nreg.4) # plot fit vs sisaan
abline(h=0) # membuat garis horizontal 
qqnorm(res.nreg.4,col="blue") # menghitung QQ residul
qqline(res.nreg.4,distribution=qnorm,col="red") # membuat garis QQ 

# Uji kenormalan sisaan menggunakan uji Shapiro-Wilk
# Hipotesi nol mengatakan data menyebar/berdistribusi normal
shapiro.test(res.nreg.4)
# p-value uji = 0.5151, dibandingkan alfa = 5%
# Kedimpulan p-value > alfa, hipotesis nol tidak ditolak /diterima
# karena tidak cukup bukti untuk menolakh Ho.

# Memprediksi data baru
data.baru = data.frame(t = c(9,10,11,14,15))
pred.Y = predict(nreg.soal.4,data.baru)
tabel.pred = cbind(data.baru,pred.Y)
tabel.pred

# Mencoba melakukan bootstrap

library(alr3)
system.time(boot.nreg.4 <- Boot(nreg.soal.4,B=1000))
scatterplotMatrix(boot.nreg.4,diagonal="histogram",
	col=palette(),
	var.labels=c(expression(alpha)),
	ellipse = TRUE,
      smooth = TRUE,
      level=c(.5,.95)
)

## estimasi parameter berdasarkan bootstrap
boot.nreg.4.summary <- data.frame(
    rbind(
        apply(boot.nreg.4, 2, mean),
        apply(boot.nreg.4, 2, sd),
        apply(boot.nreg.4, 2, function(x){quantile(x,c(0.25,0.95))})
    )
)

rownames(boot.nreg.4.summary) <- c("Mean","SD","2.5%","97.5%")
colnames(boot.nreg.4.summary) <- c("beta 1","beta 2")




library(nlstools)
boot.nreg.4 = nlsBoot(nreg.soal.4,niter=1000)
summary(boot.nreg.4)

