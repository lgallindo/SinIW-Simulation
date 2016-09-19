install.packages("SinIW")
install.packages("AdequacyModel")
install.packages("sqldf")

library(SinIW)
library(AdequacyModel)
library(sqldf)
library(xlsx)

dsiniwp <- function (par, x) {
  return(dsiniw(x, par[1], par[2]))
}
psiniwp <- function (par, x) {
  return(psiniw(x, par[1], par[2]))
}

dsiniwp(c(1,1),0.5)
psiniwp(c(1,1),0.5)
  
alphas <- c(0.5, 1, 1.5)
thetas <- c(0.7, 0.85, 1)
ns <- c(10, 100, 1000)
reps <- 10000

# sqldf("CREATE TABLE  (
# `id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
#       n int not null,
#       alpha double precision not null,
#       theta double precision not null,
#       alpha_hat double precision null,
#       theta_hat double precision null,
#       alpha_bias double precision null,
#       theta_bias double precision null,
#       alpha_error double precision null,
#       theta_error double precision null,
#       KS_Statistic double precision null
# )ENGINE=;
#       ")

results <- NULL
results <- as.data.frame(
  list(
    "ID_run"=NA,
    "n"=NA,
    "rep"=NA,
    "alpha"=NA,
    "theta"=NA,
    "alpha_hat"=NA,
    "theta_hat"=NA,
    "alpha_bias"=NA,
    "theta_bias"=NA,
    "alpha_error"=NA,
    "theta_error"=NA,
    "KS_Statistic"=NA
    )
)
ID <- 1
samples <- list()
for (n in ns) {
  for (alpha in alphas) {
    for (theta in thetas) {
      for (rep in 1:reps) {
        
        # Debug info
        run <- NULL
        run$ID <- ID
        run$n <- n
        run$par <- list("alpha"=alpha, "theta"=theta)
        
        # Estimation
        sucessIndicator <- ""
        class(sucessIndicator) <- "try-error"
        
        while(class(sucessIndicator)=="try-error") {
          sucessIndicator <- try({
            run$sample <- rsiniw(n, alpha, theta)
            run$estimates <- goodness.fit(
                                          pdf = dsiniwp,
                                          cdf = psiniwp,
                                          starts = c(1,1),
                                          method="B",
                                          data=run$sample,
                                          lim_inf=c(0,0),
                                          lim_sup=c(Inf,Inf))
          })
          if (class(sucessIndicator)!="try-error") {
            if ((run$estimates$mle[1]<0)|(run$estimates$mle[2]<0)) {
              class(sucessIndicator) <- "try-error"
            }
          }
        }
          
        # Save debug info
        samples[[ID]] <- run
        
        # Working on comprehensive results...
        results[ID,] <- c(
                          ID,
                          n,
                          rep,
                          alpha,
                          theta,
                          run$estimates$mle[1],
                          run$estimates$mle[2],
                          run$estimates$mle[1]-alpha,
                          run$estimates$mle[2]-theta,
                          run$estimates$Erro[1],
                          run$estimates$Erro[2],
                          run$estimates$KS$statistic)
        
        ID <- ID+1
      }
      Sys.time()
      gc()
      flush.console()
    }
  }
  Sys.time()
  gc()
  flush.console()
}
write.xlsx(results,"SinIW.xlsx")
  
sample <- rsiniw(ns[1], alphas[1], thetas[1])

dsiniw
thetas[1]

hist(sample)
goodness.fit(pdf = dsiniwp, cdf = psiniwp, starts = c(1,1), data=sample, lim_inf=c(0,0), lim_sup=c(Inf,Inf))
