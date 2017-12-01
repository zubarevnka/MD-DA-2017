#Загрузите данные в датафрейм.
gmp <- read.table(file = "https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/gmp.dat")
gmp$pop <- gmp$gmp / gmp$pcgmp

estimate.scaling.exponent <- function(a, y0=6611, response=gmp$pcgmp,
                                        predictor = gmp$pop, maximum.iterations=100, deriv.step = 1/100,
                                        step.scale = 1e-12, stopping.deriv = 1/100) {
  mse <- function(a) { mean((response - y0*predictor^a)^2) }
  for (iteration in 1:maximum.iterations) {
    deriv <- (mse(a+deriv.step) - mse(a))/deriv.step
    a <- a - step.scale*deriv
    if (abs(deriv) <= stopping.deriv) { break() }
  }
  fit <- list(a=a,iterations=iteration,
              converged=(iteration < maximum.iterations))
  return(fit)
}
#Пример вызова с начальным занчением a

a0 <- (estimate.scaling.exponent(0.15))
a0

#С помошью полученного коэффициента постройте кривую (функция curve) зависимости
y1 = 6611
curve(y1 * x ^ estimate.scaling.exponent(0.15, y0 = y1)$a, xlab = "Население", ylab = "Доход на душу населения", from = 1, to = 1000);

#Удалите точку из набора исходных данных случайным образом, как изменилось статистическая оценка коэффициента a?

gmp <- gmp[-c(round(runif(1, 1, nrow(gmp))))] 
a1 <- estimate.scaling.exponent(0.15) 
a1
a1$a - a0$a #значение a не изменилось

#Запустите оценку несколько раз с разных стартовых точек. Как изменилось значение a?

a2 <- estimate.scaling.exponent(0)$a
a3 <- estimate.scaling.exponent(0.12)$a
a4 <- estimate.scaling.exponent(0.25)$a
a5 <- estimate.scaling.exponent(0.40)$a
a6 <- estimate.scaling.exponent(0.50)$a
a7 <- estimate.scaling.exponent(1.01)$a
#Чем больше значение стартовой точки, тем меньше значение а
