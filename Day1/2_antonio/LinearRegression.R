N = 50
x = runif(N,0,3)
y = 3*x + 1 + rnorm(N,0,1)

# Prior on w0, N(a0,sigma = b0)
a0 = 0
b0 = 1
# Prior on w1, N(a1,sgima = b1)
a1 = 0
b1 = 1


# Sample size for each iteration: K
K=1000

for (i in 1:N) {
  cat('Data item no.: ',i,'\n')
  
  s_w0 = rnorm(K,a0,b0)
  s_w1 = rnorm(K,a1,b1)
  s_epsilon = rnorm(K)
  var_yi = b0^2+b1^2*x[i]^2+1
  
  p = dnorm(s_w0,a0,b0) * dnorm(s_w1,a1,b1) * dnorm(y[i],s_w0+s_w1*x[i]+s_epsilon,sqrt(var_yi)) 
  p_star = dnorm(s_w0,a0,b0) * dnorm(s_w1,a1,b1)
  weights = p / p_star
  sum_weights = sum(weights)
  
  est_a0 = 1/sum_weights*sum(weights*s_w0)
  est_b0 = 1/sum_weights*sum(weights*s_w0^2)
  a0 = est_a0
  b0 = sqrt(est_b0-est_a0^2)
  cat('a0=',a0,' ; b0=',b0,'\n')
  
  est_a1 = 1/sum_weights*sum(weights*s_w1)
  est_b1 = 1/sum_weights*sum(weights*s_w1^2)
  a1 = est_a1
  b1 = sqrt(est_b1-est_a1^2)
  cat('a1=',a1,' ; b1=',b1,'\n')
}

plot(x,y,xlim=c(-0.1,3.2),ylim=c(-0.1,11),pch=20)
curve(3*x+1, from = 0, to = 3.1, add=T, col = 'blue')
curve(a1*x+a0, from = 0, to = 3.1, add=T, col = 'red')
linM = lm(y~x)
curve(linM$coefficients[1]+linM$coefficients[2]*x, from = 0, to = 3.1, add=T, col = 'green')
legend(0, 10.5, legend= c('True model','I.S.','Least squares'), col=c('blue','red','green'),lty=1, cex=0.8)
