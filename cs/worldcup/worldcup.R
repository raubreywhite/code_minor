setwd("/analyses/code_minor/cs/worldcup")
library ("rstan")
library ("arm")

## Sebastian's function to run stan with caching of compiled Stan models
stan_run <- function(stanModel, ...) {
  if(class(stanModel) == "stanfit") {
    stanExe <- stanModel
  } else {
    stanModel.rda <- gsub("stan$", "rda", stanModel)
    if(!file.exists(stanModel.rda) || file.info(stanModel.rda)$mtime < file.info(stanModel)$mtime) {
      cat("Model",stanModel,"needs recompilation.\n")
      args <- modifyList(list(...), list(file=stanModel, iter=0, warmup=0, chains=0))
      stanExe <- do.call(stan, args)
      saveRDS(stanExe, file=stanModel.rda)
    } else {
      cat("Loading cached stan model", stanModel, ".\n")
      stanExe = readRDS(stanModel.rda)
    }
  }
  # This bit with the seed is for debugging purposes; once we figure out why Stan is crashing R we can remove it.
  seed <- sample.int(.Machine$integer.max, 1)
  write (seed, file="stan_seed.txt")
  stan(fit=stanExe, seed=seed, ...)
}


teams <- as.vector (unlist (read.table ("soccerpowerindex.txt", header=FALSE)))
nteams <- length(teams)
prior_score <- rev(1:nteams)
prior_score <- (prior_score - mean(prior_score))/(2*sd(prior_score))

data2012 <- read.table ("worldcup2012.txt", header=FALSE)
ngames <- nrow (data2012)

team1 <- match (as.vector(data2012[[1]]), teams)
score1 <- as.vector(data2012[[2]])
team2 <- match (as.vector(data2012[[3]]), teams)
score2 <- as.vector(data2012[[4]])

df <- 7

data <- c("nteams","ngames","team1","score1","team2","score2","prior_score","df")

fit <- stan_run("worldcup_matt.stan", data=data, chains=4, iter=5000)
print(fit)

sims <- extract(fit)
a_sims <- sims$a
a_hat <- colMeans(a_sims)
a_se <- apply(a_sims,2,sd)

new_order <- order(a_hat[team1] - a_hat[team2])


png ("worldcup7.png", height=500, width=500)
coefplot (rev(a_hat), rev(a_se), CI=1, varnames=rev(teams), main="Team quality (estimate +/- 1 s.e.)\n", cex.var=.9, mar=c(0,4,5.1,2))
dev.off()

a_sims <- sims$a
sigma_y_sims <- sims$sigma_y
nsims <- length(sigma_y_sims)
random_outcome <- array(NA, c(nsims,ngames))
for (s in 1:nsims){
  random_outcome[s,] <- (a_sims[s,team1] - a_sims[s,team2]) + rt(ngames,df)*sigma_y_sims[s]
}
sim_quantiles <- array(NA,c(ngames,2))
for (i in 1:ngames){
  sim_quantiles[i,] <- quantile(random_outcome[,i], c(.025,.975))
}
flip <- 1
png ("worldcup8.png", height=1000, width=500)
coefplot ((score1 - score2)[new_order]*flip, sds=rep(0, ngames),
          lower.conf.bounds=sim_quantiles[new_order,1]*flip, upper.conf.bounds=sim_quantiles[new_order,2]*flip, 
          varnames=ifelse(flip==1, paste(teams[team1[new_order]], "vs.", teams[team2[new_order]]),
                          paste(teams[team2[new_order]], "vs.", teams[team1[new_order]])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2), xlim=c(-6,6))
dev.off ()
