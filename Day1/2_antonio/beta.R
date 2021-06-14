N = 100
S = 20
curve(dbeta(x,S+1,N-S+1),0,1,col="blue")
curve(dbeta(x,1,1),0,1,add=T,col="red")

