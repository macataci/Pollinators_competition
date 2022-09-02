library(deSolve)
library(ggplot2) 
library(tidyr)

Model <- function(t, y, p) {
  N <- y
  TP <- N[2]+N[3]
  V <- (N[1]*TP*cp)/(Dp+TP)
  with(as.list(p), {
    dB.dt <- (beta*V) - (Kb*N[1]^2) - (d[1]*N[1])
    dP1.dt <- lambda[1]*N[2]*(cv*V*N[2]/TP)/(Dv+(V*N[2]/TP)) - (K[1]*N[2]^2) - d[2]*N[2]
    dP2.dt <- lambda[2]*N[3]*(cv*V*N[3]/TP)/(Dv+(V*N[3]/TP)) - (K[4]*N[3]^2) - d[3]*N[3]
    return(list(c(dB.dt, dP1.dt, dP2.dt)))
  })
}

# we can change gammas
#Parameters
par <- seq(0.01, 5, 0.01)
res <- NULL

for (i in 1:length(par)) {
  Kb <- 0.1
  beta <- 0.5
  lambda <- c(0.2, 1)
  K <-  matrix(c(1, 0, 0, 1), nrow = 2)
  d <- c(0.01 + par[i], 0.1, 0.8)
  cp <- 10
  Dp <- 10
  cv <- 50
  Dv <- 30
  p <- list(Kb, beta, lambda, cp, Dp, cv, Dv, K, d)
  N0 <- c(50, 10, 10)
  t <- c(1:1000)
  
  
  out <- ode(y = N0, times = t, func = Model, parms = p)
  out <- as.data.frame(out)
  colnames(out) <- c("Time", "Bees", "P1", "P2")
  tail(out)
  #View(out)
  run <- tail(out,1 )[,2:4]
  run <- cbind(d[1], run)
  res <- rbind(res, run)
}
res

ggplot() +
  geom_line(aes(res$`d[1]`,res$Bees), col ="red") +
  geom_line(aes(res$`d[1]`,res$P1), col = "blue") +
  geom_line(aes(res$`d[1]`,res$P2), col = "green")+
  labs(x = "Bees death rate", y = "N", color = "Species") +
  theme_classic()
  