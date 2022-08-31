### Making the simulations of the paper 
### "Competition for pollinators destabilizes plant coexistence Johnson, 2022"
###
###


### Defining the model ---------------------------------------------------------

model <- function(t, N, p, i) {
  c <- 0
  with(as.list(p), {
    for(j in seq(1,ncol(alfa))){
      c <- c + alfa[j] * g[j] * N[t, j]
    }
    
    N[t + 1,i] <- N[t,i] *((1 - g[i]) * S[i] + lamb[i] * g[i] / (1 + c))
    
    return(N)
  })
}

### Defining the parameters ----------------------------------------------------


alfa = matrix(data = c(1.83,0.85,1.21,1.09), nrow = 2, ncol = 2) #alfa ii
g = c(0.038,0.021)
S = c(0.231,0.413)
lamb = c(4.08,6.14)

maxT = 1000
N <- matrix(data = NA, nrow = maxT, ncol = 2)
N[1,1] = 153
N[1,2] = 138

p <- list(alfa, g, S, lamb)

for(t in seq(1,maxT-1)){
  for(i in seq(1,ncol(alfa))){
    N <- model(t, N, p, i)
  }
}

print(N[maxT,1])
