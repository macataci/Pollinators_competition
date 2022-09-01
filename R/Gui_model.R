library(deSolve)
library(ggplot2) 
library(tidyr)

Model <- function(t, y, p) {
  N <- y
  TP <- N[2]+N[3]
  V <- (N[1]*TP)/(Db+TP)
  with(as.list(p), {
    dB.dt <- (beta*V) - (Kb*N[1]^2) - (d[1]*N[1])
    dP1.dt <- lambda[1]*(V*N[2]/TP)/(Dv+(V*N[2]/TP)) - (K1*N[2]^2) - d[2]*N[2]
    dP2.dt <- lambda[2]*(V*N[3]/TP)/(Dv+(V*N[2]/TP)) - (K2*N[3]^2) - d[3]*N[3]
    return(list(c(dB.dt, dP1.dt, dP2.dt)))
  })
}

# we can change gammas
#Parameters

beta <- 0.5
lambda <- c(0.02, 0.05)
Db <- 10
Dv <- 50
Kb <- 0.0001
K1 <- 0.00001
K2 <- 0.00001
d <- c(0.001, 0.001, 0.001)
p <- list(beta, lambda, Dv, Db, Kb, K1, K2, d)
N0 <- c(20, 10, 10)
t <- c(1:200000)


out <- ode(y = N0, times = t, func = Model, parms = p)
out <- as.data.frame(out)
colnames(out) <- c("Time", "Bees", "P1", "P2")
#View(out)

df <- pivot_longer(out, cols = 2:4)
colnames(df) <- c("Time", "Pop", "N")
ggplot(df) +
  geom_line(aes(x = Time, y = N, color = Pop), size=1.5) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic() +
  ylim(1,50)
tail(out)

  




###########
a<- par[i]


par <- seq(0.01, 0.99, 0.01)

for (i in 1:99) {
  a<- par[i]
  #Parameters
  beta <- 0.5
  lambda <- c(0.01, 0.05)
  Db <- 10
  Dv <- 50
  Kb <- 0.0001
  K1 <- 0.00001
  K2 <- 0.00001
  d <- c(0.01+a, 0.001, 0.001)
  p <- list(beta, lambda, Dv, Db, Kb, K1, K2, d)
  N0 <- c(20, 10, 10)
  t <- c(1:500000)
  
  
  out <- ode(y = N0, times = t, func = Model, parms = p)
  out <- as.data.frame(out)
  colnames(out) <- c("Time", "Bees", "P1", "P2")
  #View(out)
  
  df <- pivot_longer(out, cols = 2:4)
  colnames(df) <- c("Time", "Pop", "N")
  ggplot(df) +
    geom_line(aes(x = Time, y = N, color = Pop), size=1.5) +
    labs(x = "Time", y = "N", color = "Species") +
    theme_classic() +
    ylim(1,50)
  res <- tail(out, 1)[,2:4]
  res <- cbind(d[1], res) 
}

rbind(tail(out, 1), tail(out, 1))