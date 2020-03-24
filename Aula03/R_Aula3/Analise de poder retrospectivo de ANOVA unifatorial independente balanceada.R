# Analise de poder restrospectivo de ANOVA unifatorial independente balanceada
k <- 3
n <- 20
alfa <- 0.05
dfn <- k - 1
dfd <- k*(n - 1)
Fcrt <- qf(1-alfa, dfn, dfd)
Fobs <- Fcrt*0.99 
eta2 <- dfn*Fobs/(dfn*Fobs+dfd)

# MBESS: provides various advanced and nonstandard methods that are not easily 
# implemented elsewhere, especially with regards to effect sizes, 
# confidence intervals for effect sizes, and sample size planning.
library(MBESS)

# ci.pvaf: confidence intervals for the population proportion of variance 
# accounted for in the dependent variable by knowing group
# status (i.e., ηp2) for the fixed effects factor in an ANOVA setting.
eta2lims <- MBESS::ci.pvaf(Fobs, dfn, dfd, k*n, 1-alfa)
f2 <- eta2/(1-eta2)
f2.ll <- eta2lims$Lower.Limit.Proportion.of.Variance.Accounted.for/
  (1-eta2lims$Lower.Limit.Proportion.of.Variance.Accounted.for)
f2.ul <- eta2lims$Upper.Limit.Proportion.of.Variance.Accounted.for/
  (1-eta2lims$Upper.Limit.Proportion.of.Variance.Accounted.for)

# GERARD, PD et al. (1998) Limits of retrospective power analysis.
# The Journal of Wildlife Management, 62(2): 801-807.
# Conforme Fig.1 de GERARD et al. (1998, p. 803), a partir de true power
# igual a 0.8, o poder.p é a sua melhor estimativa.
## ncp.p: plug in estimative
ncp.p <- dfd*f2 # ou dfn*Fobs
## ncp.p.ll: IC lower limit plug in estimative
ncp.p.ll <- dfd*f2.ll 
## ncp.p.ul: IC upper limit plug in estimative
ncp.p.ul <- dfd*f2.ul 

## ncp.ub: unbiased (alternative corrected) estimative
# ncp.ub <- ncp.p*(dfd-2)/dfd - dfn
# ## ncp.ub.ul: IC upper limit unbiased (alternative corrected) estimative
# ncp.ub.ul <- ncp.p.ul*(dfd-2)/dfd - dfn
# ## ncp.m: median unbiased (percentile) estimative
# ncp.m <- (ncp.p + ncp.ub)/2
# ## ncp.m.ul: median unbiased (percentile) estimative
# ncp.m.ul <- (ncp.p.ul + ncp.ub.ul)/2

# Comparar o ncp.p.ll e ncp.p.ul com os limites do IC de lambda de NDC:
# NDC is a Noncentral Distribution Calculator that performs noncentrality 
# interval estimation for the noncentral T, noncentral F, and noncentral 
# Chi-Square distributions. NDC is a stand-alone Windows application.
# http://www.statpower.net/Software.html
sink("AnalisePoderANOVA1fb.txt")
cat(paste("N =", k*n, "\tFcrt =", round(Fcrt,2),"\tFobs =", round(Fobs,2),"\n"))
poder.p <- 1-pf(Fcrt,dfn, dfd, ncp.p)
cat(paste("\tPoder.p =", round(poder.p,3),"\n"))
poder.p.ll <- 1-pf(Fcrt,dfn, dfd, ncp.p.ll)
cat(paste("\tPoder.p.ll =", round(poder.p.ll,3),"\n"))
poder.p.ul <- 1-pf(Fcrt,dfn, dfd, ncp.p.ul)
cat(paste("\tPoder.p.ul =", round(poder.p.ul,3),"\n"))
sink()
