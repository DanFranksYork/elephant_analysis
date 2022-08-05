#https://www.vcalc.com/wiki/vCalc/Gompertz+function
gompertz <- function(t,a=1,b=10,c=0.06) {
  # a is the asymtote
  
  # c + number, 'growth' rate
  # t number of years
  a * exp(-b*exp(-c*t))
}

plot(1-gompertz(t=1:100))


