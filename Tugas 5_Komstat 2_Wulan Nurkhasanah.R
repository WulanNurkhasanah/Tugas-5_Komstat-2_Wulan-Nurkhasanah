#1. Sebaran Eksponensial

# Soal:
# Rata-rata waktu tunggu mu = 5 menit.
# Berapa peluang P(X > 5)?

# Nilai harapan distribusi eksponensial:
# E[X] = 1/lambda
# Diketahui E[X] = 5, sehingga:
# lambda = 1/5 = 0.2

lambda <- 1/5

# Menghitung peluang P(X > 5)
pexp(5, rate = 1/5, lower.tail = FALSE)

# Cara lain menghitung P(X > 5)
1 - pexp(5, rate = 1/5)


# Contoh: Eksponensial dengan lambda = 0.2
# E[X] = 5
x_dexp <- seq(1, 30, by = 1)
y_dexp <- dexp(x_dexp, rate = 0.2)

plot(x_dexp, y_dexp, type = "l", col = "pink", lwd = 2,
     main = "PDF Distribusi Eksponensial (λ = 0.2)",
     xlab = "x", ylab = "f(x)")


# Simulasi data dari distribusi eksponensial
n <- 5
lambda_true <- 0.2

# 1) Satu sampel
x <- rexp(n, rate = lambda_true)
x

# MLE untuk lambda (rate) dan estimator untuk beta = 1/lambda (scale)
(lambda_hat <- n / sum(x))       # MLE: n / sum(x)
(lambda_hat_unbiased <- (n-1)/n * lambda_hat)  # koreksi tak-bias (n > 1)
(beta_hat <- mean(x))

# Jawaban : Peluang = 0.3678794 atau peluang P(X > 5) adalah 36.79%

#2. Sebaran Uniform Kontinu

# Soal:
# Kereta komuter tiba di stasiun secara acak
# antara pukul 07.00 hingga 07.20
# (interval 20 menit).
# Berapakah ragam (varians) waktu tunggu penumpang?

# Membutuhkan a dan b
# Waktu 07.00 dijadikan menit ke-0
# Waktu 07.20 dijadikan menit ke-20

set.seed(2025)
n <- 1000
a <- 0
b <- 20

# Generate sampel
x <- runif(n, min = a, max = b)

# Nilai density / CDF / quantile contoh
d_values <- dunif(c(0, 10, 20), min = a, max = b)
p_values <- punif(c(0, 10, 20), min = a, max = b)
q_values <- qunif(c(0.25, 0.5, 0.75), min = a, max = b)

# Plot: histogram sampel + overlay PDF teoritis
hist(x, breaks = 30, probability = TRUE,
     main = "Histogram Sampel U(0,20) dengan PDF Teoritis",
     xlab = "Waktu tunggu (menit)")

curve(dunif(x, min = a, max = b),
      from = a, to = b, add = TRUE, lwd = 2)

# Menampilkan ringkasan
mean(x)   # harus dekat dengan (a+b)/2 = 10
var(x)    # harus dekat dengan (b-a)^2 / 12 = (20)^2/12 = 33.3333

#Jawaban : ragam (varians) waktu tunggu penumpang adalah 33,33 menit².


#3. Sebaran Eksponensial

# Soal:
# Masa pakai sensor suhu memiliki rata-rata mu = 10 tahun.
# Berapa peluang sensor tersebut rusak sebelum mencapai usia 5 tahun?

# Nilai harapan distribusi eksponensial:
# E[X] = 1/lambda
# Diketahui E[X] = 10, sehingga:
# lambda = 1/10 = 0.1

lambda <- 1/10

# Menghitung peluang P(X < 5)
pexp(5, rate = 1/10)

# Cara lain menghitung P(X < 5)
1 - pexp(5, rate = 1/10, lower.tail = FALSE)


# Contoh: Eksponensial dengan lambda = 0.1
# E[X] = 10

x_dexp <- seq(1, 30, by = 1)
y_dexp <- dexp(x_dexp, rate = 0.1)

plot(x_dexp, y_dexp, type = "l", col = "green", lwd = 2,
     main = "PDF Distribusi Eksponensial (λ = 0.1)",
     xlab = "x", ylab = "f(x)")


# Simulasi data dari distribusi eksponensial
n <- 5
lambda_true <- 0.1

# 1) Satu sampel
x <- rexp(n, rate = lambda_true)
x

# MLE untuk lambda (rate) dan estimator untuk beta = 1/lambda (scale)
(lambda_hat <- n / sum(x))       # MLE: n / sum(x)
(lambda_hat_unbiased <- (n-1)/n * lambda_hat)  # koreksi tak-bias (n > 1)
(beta_hat <- mean(x))

#Jawaban : Peluang = 0.3934693 atau Peluang sensor suhu mengalami kerusakan sebelum mencapai usia 5 tahun adalah sekitar 39,35%.



#4. Sebaran Normal (Gaussian)

# Soal:
# Berat bersih kemasan kopi menyebar normal dengan
# mu = 250 gram dan sigma = 5 gram.
# Kemasan dianggap underweight jika beratnya kurang dari 240 gram.
# Berapa proporsi produk yang tergolong underweight?

# Membutuhkan mu dan sigma
# mu = 250 gram
# sigma = 5 gram

n <- 100
mu <- 250
sigma <- 5

# Generate sampel
x <- rnorm(n, mean = mu, sd = sigma)

# Menghitung proporsi produk underweight
# Underweight jika X < 240 gram
prop_underweight <- pnorm(240, mean = mu, sd = sigma)
prop_underweight

# Mengubah proporsi menjadi persentase
prop_underweight * 100


# Statistik sampel
(x_bar <- mean(x))                   # estimator untuk mu
(mle_sigma2 <- mean((x - x_bar)^2))  # MLE untuk sigma^2
(sd_sample <- sd(x))                 # simpangan baku sampel


# Plot: histogram + overlay PDF teoritis
hist(x, breaks = 30, probability = TRUE,
     main = "Histogram sampel N(250, 5^2) dengan PDF teoritis",
     xlab = "Berat kemasan (gram)")

curve(dnorm(x, mean = mu, sd = sigma),
      from = mu - 4*sigma,
      to = mu + 4*sigma,
      add = TRUE, lwd = 2)

abline(v = x_bar, col = "purple", lwd = 2)       # mean sampel
abline(v = mu, col = "yellow", lwd = 2, lty = 2)  # mean sebenarnya

legend("topright",
       legend = c("PDF teoritis", "mean sampel", "mean true"),
       lty = c(1, 1, 2),
       col = c("black", "purple", "red"),
       bty = "n")


# Simulasi data
n <- 100
x <- rnorm(n, mean = 250, sd = 5)

# Estimasi MLE manual
(mu_hat <- mean(x))                    # MLE untuk mu
(sigma2_hat <- mean((x - mu_hat)^2))   # MLE untuk sigma^2
var(x)

#Jawaban : proporsi produk yang tergolong underweight = 0.02275013 atau dari seluruh produk, diperkirakan sekitar 2,28% kemasan memiliki berat kurang dari 240 gram.