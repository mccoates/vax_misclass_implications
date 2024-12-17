## Matthew Coates 12/17/2024
## Example safety study comparing odds of an adverse vaccine outcome
## to the odds in an unvaccinated group, misclassifying vaccination status
rm(list=ls())
library(data.table)
library(ggplot2)
library(viridis)

## inputs
or <- seq(from=1,to=3,by=0.05)
p_covid_unvax <- seq(from=0.1,to=0.9,by=0.2)
pct_vax <- data.frame(combined=0.62,claims=0.58,ehr=0.25)

## function to create data, misclassify data, and estimate biased OR
misclass <- function(or_n,pct_vax,p_covid_unvax_i) {
  ## treating combined data as truth
  N <- 1000
  C <- (1-pct_vax$combined)*N*p_covid_unvax_i
  D <- (1-pct_vax$combined)*N*(1-p_covid_unvax_i)
  oddsAB <- or_n*(C/D)
  B <- (N-C-D)/(1+oddsAB)
  A <- oddsAB*B
  truth <- matrix(data=c(A,B,C,D),nrow=2)
  ## check
  if (!abs(((truth[1,1]*truth[2,2])/(truth[1,2]*truth[2,1])-or_n)) < 0.000000000001) stop("problem")
  
  ## claims
  pct_misclass <- (pct_vax$combined-pct_vax$claims)/pct_vax$combined
  Aclaims <- A - pct_misclass*A
  Cclaims <- C + pct_misclass*A
  Bclaims <- B - pct_misclass*B
  Dclaims <- D + pct_misclass*B
  if (!abs(Aclaims+Bclaims+Cclaims+Dclaims - N) < .000000000001) stop("issue")
  if (!abs((Aclaims+Bclaims)/(N) - pct_vax$claims) < .000000000001) stop("issue")
  claims <- matrix(data=c(Aclaims,Bclaims,Cclaims,Dclaims),nrow=2)
  or_claims <- (Aclaims*Dclaims)/(Bclaims*Cclaims)
  
  ## ehr
  pct_misclass <- (pct_vax$combined-pct_vax$ehr)/pct_vax$combined
  Aehr <- A - pct_misclass*A
  Cehr <- C + pct_misclass*A
  Behr <- B - pct_misclass*B
  Dehr <- D + pct_misclass*B
  if (!abs(Aehr+Behr+Cehr+Dehr - N) < .000000000001) stop("issue")
  if (!abs((Aehr+Behr)/(N) - pct_vax$ehr) < .000000000001) stop("issue")
  ehr <- matrix(data=c(Aehr,Behr,Cehr,Dehr),nrow=2)
  or_ehr <- (Aehr*Dehr)/(Behr*Cehr)
  
  out <- data.table(data.frame(or_ehr=or_ehr,or_claims=or_claims,or_n=or_n,p_covid_unvax_i=p_covid_unvax_i))
  return(out)
}

inputs <- data.table(expand.grid(or_n=or,p_covid_unvax_i=p_covid_unvax))

res <- list()
for (i in 1:nrow(inputs)) {
  res[[i]] <- misclass(or_n=inputs[i]$or_n,pct_vax=pct_vax,
                       p_covid_unvax_i=inputs[i]$p_covid_unvax_i)
}
res <- rbindlist(res)
setnames(res,c("or_n"),c("or_true"))
res[,p_covid_unvax_i_fact:=factor(p_covid_unvax_i)]

res <- melt(res,id.vars=c("or_true","p_covid_unvax_i","p_covid_unvax_i_fact"))
res[,variable:=factor(variable,levels=c("or_claims","or_ehr"),
                      labels=c("a. Misclassified vaccination based on claims (58% vs. 62%)",
                               "b. Misclassified vaccination based on EHR (25% vs. 62%)"))]

pdf("./safety_outcome.pdf",width=6,height=5)

gg <- ggplot(data=res,aes(x=or_true,y=value,group=p_covid_unvax_i_fact,color=p_covid_unvax_i_fact)) +
  geom_line() + geom_abline(slope=1,color="firebrick3") + theme_bw() +
  scale_color_viridis("Probability of Outcome\nAmong Unvaccinated",discrete=TRUE,option="mako") +
  scale_x_continuous("True odds ratio for\nsafety outcome",limits=c(1,2)) +
  scale_y_continuous("Odds ratio estimated with\nmisclassified exposure",limits=c(1,2)) +
  facet_wrap(~variable,nrow=2)
print(gg)

dev.off()








