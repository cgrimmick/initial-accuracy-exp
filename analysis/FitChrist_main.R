# Fitneva and Christiansen, 2015 — having initially wrong pairings benefits learning — both for the pairs that switch and for the pairs that don’t ???
# - familiarization phase (before learning) had high and low initial accuracy (IA) conditions:
#   - high IA: 60% of the associations were correct; low IA: 40% correct
#  - 10 pairs: learning phase - 3 blocks of 5 trials each, 2 pairs/trial, each pair repeated 3 times
#  - "two pairs appeared together only once"

# Fitneva and Christiansen 2015 order
fcord = matrix(c(1,2, 3,4, 5,6, 7,8, 9,10,  
                 1,4, 2,8, 6,10, 9,3, 5,7,  
                 1,7, 3,8, 2,4, 5,10, 6,9), nrow=15, ncol=2, byrow=T)


cooc_matrix <- function(ord) {
  voc_sz = max(unlist(ord))
  m = matrix(0, nrow=voc_sz, ncol=voc_sz)
  for(r in 1:nrow(ord)) {
    m[ord[r,],ord[r,]] = m[ord[r,],ord[r,]] + 1
  }
  return(m)
}

cooc_matrix(fcord)

source("FitChristmodel.R")

RunFitnevaChristiansenExp <- function(par) {
  mdat = data.frame(Cond=c("Low IA","Low IA", "High IA", "High IA"), 
                    Item=c("Initially Accurate","Initially Inaccurate","Initially Accurate","Initially Inaccurate"),
                    Mean=NA, SD=NA)
  X = par[1]*(par[3]^2)
  # low IA: swap 6: 40% correct initial mappings
  lowIA_m = matrix(0, nrow=10, ncol=10)
  lowIA_m[1,3] = X
  lowIA_m[2,1] = X
  lowIA_m[3,2] = X
  lowIA_m[4,6] = X
  lowIA_m[5,4] = X
  lowIA_m[6,5] = X
  diag(lowIA_m)[7:10] = X
  # high IA: swap 4: 60% correct initial mappings
  highIA_m = matrix(0, nrow=10, ncol=10)
  highIA_m[1,2] = X
  highIA_m[2,1] = X
  highIA_m[3,4] = X
  highIA_m[4,3] = X
  diag(highIA_m)[5:10] = X
  #print("Low Initial Accuracy condition (6/10 pairs wrong at familiarization)")
  lom = model(par, ord=fcord, m=lowIA_m)
  acc18afc = diag(lom) / rowSums(lom)
  mean(acc18afc) # .61 - low initial accuracy is good for the model (like adults)
  #mdat[2,]$Mean = mean(acc18afc[1:6]) # .61
  #mdat[2,]$SD = sd(acc18afc[1:6]) 
  #mdat[1,]$Mean = mean(acc18afc[7:10]) # .62
  #mdat[1,]$SD = mean(acc18afc[7:10]) # 
  # diag / (diag + mean(the rest))
  nondiag = lom
  diag(nondiag) = 0
  avg2afc_acc = diag(lom) / (diag(lom) + rowMeans(nondiag))
  mean(avg2afc_acc[1:6]) # .94
  mean(avg2afc_acc[7:10]) # .94
  mdat[2,]$Mean = mean(avg2afc_acc[1:6]) # .61
  mdat[2,]$SD = sd(avg2afc_acc[1:6]) 
  mdat[1,]$Mean = mean(avg2afc_acc[7:10]) # .62
  mdat[1,]$SD = sd(avg2afc_acc[7:10]) # 
  
  #print("High Initial Accuracy condition (4/10 pairs wrong at familiarization)")
  him = model(par, ord=fcord, m=highIA_m)
  acc18afc = diag(him) / rowSums(him)
  mean(acc18afc) # .53 - high initial accuracy actually hinders the model somehow...cool!
  mean(acc18afc[1:4]) # .28
  mean(acc18afc[5:10]) # .70
  nondiag = him
  diag(nondiag) = 0
  avg2afc_acc = diag(him) / (diag(him) + rowMeans(nondiag))
  mean(avg2afc_acc[1:4]) # .75
  mean(avg2afc_acc[5:10]) # .96
  mdat[4,]$Mean = mean(avg2afc_acc[1:4]) # .61
  mdat[4,]$SD = sd(avg2afc_acc[1:4]) 
  mdat[3,]$Mean = mean(avg2afc_acc[5:10]) # .62
  mdat[3,]$SD = sd(avg2afc_acc[5:10]) # 
  return(mdat)
}

RunFitnevaChristiansenExp(c(.2, 2, .95)) # need Low IA 10% greater than High IA -- and init accuracy only helps slightly
RunFitnevaChristiansenExp(c(.1, 2, .95))
RunFitnevaChristiansenExp(c(.1, 4, .95))

RunFitnevaChristiansenExp(c(.1, 2, 1))
RunFitnevaChristiansenExp(c(.1, 4, 1))
RunFitnevaChristiansenExp(c(.1, .5, 1))
RunFitnevaChristiansenExp(c(.1, 5, 1))

humdat = data.frame(Cond=c("Low IA","Low IA", "High IA", "High IA"), 
                  Item=c("Initially Accurate","Initially Inaccurate","Initially Accurate","Initially Inaccurate"),
                  Mean=c(.91,.84,.76,.74), SD=NA)

dat10yo = data.frame(Cond=c("Low IA","Low IA", "High IA", "High IA"), 
                     Item=c("Initially Accurate","Initially Inaccurate","Initially Accurate","Initially Inaccurate"),
                     Mean=c(.75,.63,.75,.56), SD=NA)

dat4yo = data.frame(Cond=c("Low IA","Low IA", "High IA", "High IA"), 
                    Item=c("Initially Accurate","Initially Inaccurate","Initially Accurate","Initially Inaccurate"),
                    Mean=c(.53,.55,.63,.62), SD=NA)


FitChristSSE <- function(par, humdat) {
  mdat = RunFitnevaChristiansenExp(par)
  return(sum((humdat$Mean - mdat$Mean)^2))
}

require("DEoptim")

# adults
fit = DEoptim(FitChristSSE, lower=c(.001,.1,.5), upper=c(2,20,1), DEoptim.control(reltol=.001, NP=100), humdat=humdat) # 
mad = RunFitnevaChristiansenExp(fit$optim$bestmem) # fit$optim$bestmem 0.29 4.54 0.586
fit$optim$bestval # .010

fit10yo = DEoptim(FitChristSSE, lower=c(.001,.1,.5), upper=c(2,20,1), DEoptim.control(reltol=.001, NP=100), humdat=dat10yo) # DEoptim.control(reltol=.001),
m10yo = RunFitnevaChristiansenExp(fit10yo$optim$bestmem) # 0.051 15.0  0.627 
fit10yo$optim$bestval # .005

fit4yo = DEoptim(FitChristSSE, lower=c(.001,.1,.5), upper=c(2,15,1), DEoptim.control(reltol=.001, NP=100), humdat=dat4yo) # DEoptim.control(reltol=.001),
m4yo = RunFitnevaChristiansenExp(fit4yo$optim$bestmem) # 0.012 0.100 0.737
fit4yo$optim$bestval # .007

mad$Age = "Adults"
m10yo$Age = "10-year-olds"
m4yo$Age = "4-year-olds"
allfits = rbind(m4yo, m10yo, mad)
allfits$Age = factor(allfits$Age, levels=c("4-year-olds","10-year-olds","Adults"))
allfits$Cond = factor(allfits$Cond, levels=c("Low IA", "High IA"))

humdat$Age = "Adults"
dat10yo$Age = "10-year-olds"
dat4yo$Age = "4-year-olds"
FCdat = rbind(humdat, dat10yo, dat4yo)

require("ggplot2")
dodge <- position_dodge(width=.9)
#limits <- with(hum, aes(ymax=x+SE, ymin=x-SE))
a <- ggplot(FCdat, aes(x=Cond, y=Mean, fill=Item)) + labs(x="Condition", y="Proportion Correct", fill="Items") + 
  geom_bar(stat="identity", position=dodge) + geom_point(data=allfits, aes(x=Cond, y=Mean, fill=Item), position=dodge) + 
  facet_wrap(~Age, nrow=1) + geom_hline(yintercept=1/2, linetype='dashed')
#b <- a + geom_errorbar(limits,  width=0.2, position=dodge) + geom_hline(yintercept=1/18, linetype='dashed') # + scale_fill_manual(values=c("red", "orange", "yellow"))
print(a)
ggsave("FitnevaChristiansen2015_modelfits.pdf", width=6.5, height=4.5)
dev.off()

