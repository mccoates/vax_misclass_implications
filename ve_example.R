## Matthew Coates 12/17/2024
## Example VE study comparing VE estimated under different scenarios of 
## true VE and misclassification
rm(list=ls())
library(data.table)
library(ggplot2)
library(viridis)

true_ve <- seq(from=0.5,to=0.9,by=.001)
pct_vax <- data.frame(combined=0.62,claims=0.58,ehr=0.25)
scale_misclass <- seq(from=0,to=1,by=.2)

## function to take percent vaccinated plus odds ratio
## and create estimated odds ratios from misclassified data
misclass <- function(true_ve_i,pct_vax,scale_misclass_i) {
  odds_ppv <- pct_vax$combined/(1-pct_vax$combined)
  pcv <- ((1-true_ve_i)*odds_ppv)/(1+(1-true_ve_i)*odds_ppv)
  odds_pcv <- pcv/(1-pcv)
  
  odds_ppv_claims <- pct_vax$claims/(1-pct_vax$claims)
  odds_ppv_ehr <- pct_vax$ehr/(1-pct_vax$ehr)
  pcv_claims <- (1-(scale_misclass_i*((pct_vax$combined-pct_vax$claims)/pct_vax$combined)))*pcv
  pcv_ehr <- (1-(scale_misclass_i*((pct_vax$combined-pct_vax$ehr)/pct_vax$combined)))*pcv
  odds_pcv_claims <- pcv_claims/(1-pcv_claims)
  odds_pcv_ehr <- pcv_ehr/(1-pcv_ehr)
  
  ve_claims <- 1-odds_pcv_claims/odds_ppv_claims
  ve_ehr <- 1-odds_pcv_ehr/odds_ppv_ehr
  ve_combined <- 1-odds_pcv/odds_ppv ## to check
  if (abs(ve_combined-true_ve_i) > .000000000001) stop('issue')
  
  
  out <- data.table(data.frame(true_ve_i=true_ve_i,scale_misclass_i=scale_misclass_i,
                               ve_claims=ve_claims,ve_ehr=ve_ehr))
  return(out)
}

inputs <- data.table(expand.grid(true_ve_i=true_ve,scale_misclass_i=scale_misclass))

res <- list()
for (i in 1:nrow(inputs)) {
  res[[i]] <- misclass(true_ve_i=inputs[i]$true_ve_i,
                       pct_vax,scale_misclass_i=inputs[i]$scale_misclass_i)
}
res <- rbindlist(res)
setnames(res,c("true_ve_i"),c("ve_true"))
res[,scale_misclass_i_fact:=factor(scale_misclass_i,levels=c(0,0.2,0.4,0.6,0.8,1),
                                   labels=c("None","0.2","0.4","0.6","0.8","Same relative\ndegree as in\ntotal population"))]

res <- melt(res,id.vars=c("ve_true","scale_misclass_i","scale_misclass_i_fact"))
res[,variable:=factor(variable,levels=c("ve_claims","ve_ehr"),
                      labels=c("a. Misclassified vaccination based on claims (58% vs. 62%)",
                               "b. Misclassified vaccination based on EHR (25% vs. 62%)"))]

pdf("./ve_outcome.pdf",width=6.5,height=5)

gg <- ggplot(data=res,aes(x=ve_true,y=value,group=scale_misclass_i_fact,color=scale_misclass_i_fact)) +
  geom_line() + geom_abline(slope=1,color="firebrick3") + theme_bw() +
  scale_color_viridis("Scale of Misclassification\namong Cases",discrete=TRUE,option="mako") +
  scale_x_continuous("True vaccine effectiveness",limits=c(0.5,0.9)) +
  scale_y_continuous("Vaccine effectiveness estimate with\nmisclassified exposure",limits=c(0,.9)) +
  facet_wrap(~variable,nrow=2)
print(gg)

dev.off()
