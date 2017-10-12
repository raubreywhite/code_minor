library(rstanarm)
library(projpred)
library(ggplot2)
library(bayesplot)
options(mc.cores = parallel::detectCores())

data('df_gaussian', package = 'projpred')

n <- nrow(df_gaussian$x) # 100
D <- ncol(df_gaussian$x) # 20
p0 <- 5 # prior guess for the number of relevant variables
tau0 <- p0/(D-p0) * 1/sqrt(n) # scale for tau (notice that stan_glm will automatically scale this by sigma)
prior_coeff <- hs(df=1, global_df=1, global_scale=tau0) # horseshoe prior
fit <- stan_glm(y ~ x, family=gaussian(), data=df_gaussian, prior=prior_coeff,
                # to make this vignette build fast, we use only 2 chains and
                # 800 draws. In practice, more conservative values, eg. 4 chains
                # and 2000 draws might be required for reliable inference.
                seed=1, adapt_delta=0.999, chains=2, iter=800) 

fit <- varsel(fit, method='L1')
fit$varsel$vind

varsel_plot(fit, statistics=c('mlpd', 'mse'), deltas=T)

fit_cv <- cv_varsel(fit, method='L1', cv_method='LOO')

varsel_plot(fit_cv, statistics = c('mlpd', 'mse'), deltas=T)

mcmc_areas(as.matrix(fit), 
           pars = c('(Intercept)', names(fit$varsel$vind[1:fit_cv$varsel$ssize]), 'sigma')) + coord_cartesian(xlim = c(-2, 2))

