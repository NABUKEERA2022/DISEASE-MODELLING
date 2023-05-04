#rm(list = ls())

require(deSolve)

reald<-data.frame(time=1:71, cases=sample(1:100, 71))

## Create an SIR function
SIR_model <- function(times, yinit, pars){
  
  with(as.list(c(yinit,pars)), {
    N = S+I+R
    dS <- Lambda*S - beta*S  - mu*S
    dI <- beta*S - mu*I - gamma*I
    dR <- gamma*I - mu*R
    return(list(c(dS, dI, dR)))})
}

### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init <- c(S = 999999, I = 1, R=0)
## beta: infection parameter; gamma: recovery parameter
parameters <- c(Lambda=1.01, mu=0.9, beta = 0.2, gamma=0.02)
## Time frame 
times      <- seq(0, 70, by = 1)

score<-function(pars){
  out <- ode(y = init, times = times, func = SIR_model, parms = pars)
  out<-as.data.frame(out)
  model_cases <- out$I
  ss<-sum((reald$cases - model_cases)**2)
  return(ss)
}
score(parameters)

fit5 <- optim(score,par=parameters)
newParameters<-fit5$par

parameters
score(parameters)
newParameters
score(newParameters)

parameters <- newParameters

while(score(newParameters) != score(parameters)) {
parameters <- newParameters
fit5 <- optim(score,par=parameters)
newParameters<-fit5$par

parameters
score(parameters)
newParameters
score(newParameters)
}
parameters
newParameters


