# Fitneva and Christiansen, 2015 — having initially wrong pairings benefits learning — both for the pairs that switch and for the pairs that don’t ???
# - familiarization phase (before learning) had high and low initial accuracy (IA) conditions:
#   - high IA: 60% of the associations were correct; low IA: 40% correct
#  - 10 pairs: learning phase - 3 blocks of 5 trials each, 2 pairs/trial, each pair repeated 3 times
#  - "two pairs appeared together only once"

# our 18 pair order
#fcord = matrix(c(1,2, 3,4, 5,6, 7,8, 9,10, 11,12, 13,14, 15,16, 17,18, 
#                 1,4,  2,8,  6,10,  9,3,  5,7, 11,18, 13,15, 16,12, 17,14,
#                1,11, 18,7, 13,3, 15,8, 16,2,  12,4,  17,5, 14,10,  6,9), ncol=2, byrow=T)

get_rand_ord  <- function(ord)
for(i in 1:3) {
  tmp = sample(1:18, 18)
  ord = rbind(ord, matrix(tmp, ncol=2, byrow=T))
}

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
  # low IA: swap 12: 33% correct initial mappings
  liawordOrder =  c(16,14,5,10,9,6,13,2,0,17,15,4,11,8,7,12,3,1)
  lowIA_m = matrix(0, nrow=18, ncol=18)
  lowIA_m[1,3] = X
  lowIA_m[2,1] = X
  lowIA_m[3,2] = X
  lowIA_m[4,6] = X
  lowIA_m[5,4] = X
  lowIA_m[6,5] = X
  lowIA_m[7,8] = X
  lowIA_m[8,9] = X
  lowIA_m[9,10] = X
  lowIA_m[10,11] = X
  lowIA_m[11,12] = X
  lowIA_m[12,7] = X
  diag(lowIA_m)[13:18] = X
  # high IA: swap 6: 66% correct initial mappings
  hiawordOrder =  c(1,14,5,10,9,6,13,15,17,0,2,4,11,8,7,12,3,16)
  highIA_m = matrix(0, nrow=18, ncol=18)
  highIA_m[1,2] = X
  highIA_m[2,1] = X
  highIA_m[3,4] = X
  highIA_m[4,3] = X
  highIA_m[5,6] = X
  highIA_m[6,5] = X
  diag(highIA_m)[7:18] = X
  #print("Low Initial Accuracy condition (12/18 pairs wrong at familiarization)")
  lom = model(par, ord=fcord, m=lowIA_m)
  acc18afc = diag(lom) / rowSums(lom)
  mean(acc18afc) # low initial accuracy is good for the model (like adults)
  lowIA_inacc_ind = c()
  highIA_inacc_ind = c()
  highIA_acc_ind = c()
  
  mdat[2,]$Mean = mean(acc18afc[1:12]) # Low IA Initially Inaccurate
  mdat[2,]$SD = sd(acc18afc[1:12]) 
  mdat[1,]$Mean = mean(acc18afc[13:18]) # Low IA Initially Accurate
  mdat[1,]$SD = mean(acc18afc[13:18]) # 
  
  #print("High Initial Accuracy condition (4/10 pairs wrong at familiarization)")
  him = model(par, ord=fcord, m=highIA_m)
  acc18afc = diag(him) / rowSums(him)
  mean(acc18afc) # high initial accuracy actually hinders the model somehow...cool!
  mdat[4,]$Mean = mean(acc18afc[1:6]) #
  mdat[4,]$SD = sd(acc18afc[1:6]) 
  mdat[3,]$Mean = mean(acc18afc[7:18]) # 
  mdat[3,]$SD = sd(acc18afc[7:18]) # 
  return(mdat)
}

RunFitnevaChristiansenExp(c(.2, 2, .95)) # need Low IA 10% greater than High IA -- and init accuracy only helps slightly
RunFitnevaChristiansenExp(c(.1, 2, .95))
RunFitnevaChristiansenExp(c(.1, 4, .95))

RunFitnevaChristiansenExp(c(.1, 2, 1))
RunFitnevaChristiansenExp(c(.1, 4, 1))
RunFitnevaChristiansenExp(c(.1, .5, 1))
RunFitnevaChristiansenExp(c(.1, 5, 1))

# from Fitneva & Christiansen, 2015 (10 items, 60%/40%, 2AFC test):
#humdat = data.frame(Cond=c("Low IA","Low IA", "High IA", "High IA"), 
#                  Item=c("Initially Accurate","Initially Inaccurate","Initially Accurate","Initially Inaccurate"),
#                  Mean=c(.91,.84,.76,.74), SD=NA)

humdat = data.frame(Cond=c("Low IA","Low IA", "High IA", "High IA"), 
                    Item=c("Initially Accurate","Initially Inaccurate","Initially Accurate","Initially Inaccurate"),
                    Mean=c(.658,.458,.614,.288), SE=c(.064, .062, .075, .056))

FitChristSSE <- function(par, humdat) {
  mdat = RunFitnevaChristiansenExp(par)
  return(sum((humdat$Mean - mdat$Mean)^2))
}

require("DEoptim")

# adults
fit = DEoptim(FitChristSSE, lower=c(.001,.1,.5), upper=c(2,20,1), DEoptim.control(reltol=.001, NP=100), humdat=humdat) # 
mad = RunFitnevaChristiansenExp(fit$optim$bestmem) # fit$optim$bestmem c(2.0, 0.787, 0.684)
fit$optim$bestval # .0002

mad$Cond = factor(mad$Cond, levels=c("Low IA", "High IA"))

require("ggplot2")
dodge <- position_dodge(width=.9)
limits <- with(humdat, aes(ymax=Mean+SE, ymin=Mean-SE))
a <- ggplot(humdat, aes(x=Cond, y=Mean, fill=Item)) + labs(x="Condition", y="Proportion Correct", fill="Items") + 
  geom_bar(stat="identity", position=dodge) + geom_errorbar(limits,  width=0.2, position=dodge) +
  geom_point(data=mad, aes(x=Cond, y=Mean, fill=Item), position=dodge) + 
  geom_hline(yintercept=1/18, linetype='dashed')
#b <- a + geom_hline(yintercept=1/18, linetype='dashed') # + scale_fill_manual(values=c("red", "orange", "yellow"))
print(a)
ggsave("initial-accuracy18afc_modelfits.pdf", width=4.5, height=4)
dev.off()

