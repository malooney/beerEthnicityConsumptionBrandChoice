
par = starting.guesses.theta2.optim
n=1

while(identical(par, starting.guesses.theta2.optim)){

  res <- psoptim(par = starting.guesses.theta2.optim,
               fn = get.gmm.obj, gr = gradient,
               control=list(trace=1, REPORT=1, trace.stats=TRUE,
                            abstol=1e-16, type="SPSO2011",
                            #w=c(0.8, 0.4),
                            #c.p=1.494, c.g=1.494),#,
                            hybrid="improved"),
               lower = -100, upper = 100,

               #method = solver.method,
               #control = solver.control,
               blp.integration = blp.integration,
               blp.parameters =  blp.parameters,
               blp.data = blp.data,
               blp.results = blp.results,

               printLevel = printLevel)

  par = res$par
  n=n+1
  if(n==10){break}
  }

res[["par"]]

plot(res$stats$it, log(res$stats$error), type="l")
