# 18 AFC Initial Inaccuracy Psiturk 
# 18 objects and 18 words
# High Initial Accuracy Condition - 66.6% initially accurate (12 out of 18)
# Low Initial Accuracy Condition - 33.3% initially correct (6 out of 18)

# 
# get_rand_ord  <- function() {
#   ord = c()
#   for(i in 1:3) {
#     tmp = sample(1:18, 18)
#     ord = rbind(ord, matrix(tmp, ncol=2, byrow=T))
#   }
#   return(ord)
# }
# 
# get_rand_ord()


source("ReadMturkData.R")
subjs <- unique(as.character(study$uniqueId))
lia_subj_dat <- subset(study, condition=="Low Initial Accuracy")
hia_subj_dat <- subset(study, condition=="High Initial Accuracy")
lia_subjs <- unique(as.character(lia_subj_dat$uniqueId))
hia_subjs <- unique(as.character(hia_subj_dat$uniqueId))
get_subj_ord <- function(subj) {
  cur_subj <- subj
  cur_subj_dat <- subset(study, uniqueId==cur_subj) 
  ord <- data.matrix(cur_subj_dat[,c(8,9,10,11)]) + 1
  return(ord)
}

  
  


source("FitChristmodel.R")

RunFitnevaChristiansenExp <- function(par, Nsubj=45) {
  # par = c(.2, 2, .95)
  mdat = data.frame(Cond=c("Low IA","Low IA", "High IA", "High IA"), 
                    Item=c("Initially Accurate","Initially Inaccurate","Initially Accurate","Initially Inaccurate"),
                    Mean=NA, SD=NA)
  X = par[1]*(par[3]^2)

  wordOrder = c(1,3,5,7,9,11,13,15,17,0,2,4,6,8,10,12,14,16) +1
  objOrder =  c(0,2,4,6,8,10,12,14,16,1,3,5,7,9,11,13,15,17) + 1
  
  highIA_m = matrix(0, nrow=18, ncol=18)
  hiawordOrder =  c(1,14,5,10,9,6,13,15,17,0,2,4,11,8,7,12,3,16) + 1
      
  for(i in 1:length(wordOrder)) {
    #highIA_m[hiawordOrder[i],wordOrder[i]] = X
    highIA_m[hiawordOrder[i],objOrder[i]] = X
  }
  HIA_initInac = which(wordOrder!=hiawordOrder)
  HIA_initAcc = which(wordOrder==hiawordOrder)

  lowIA_m = matrix(0, nrow=18, ncol=18)
  liawordOrder =  c(16,14,5,10,9,6,13,2,0,17,15,4,11,8,7,12,3,1) + 1
  for(i in 1:length(wordOrder)) {
    #lowIA_m[liawordOrder[i],wordOrder[i]] = X
    lowIA_m[liawordOrder[i],objOrder[i]] = X
  }
  LIA_initInac = which(wordOrder!=liawordOrder)
  LIA_initAcc = which(wordOrder==liawordOrder)
  lowIA_inac = rep(0, Nsubj)
  lowIA_acc =rep(0, Nsubj)
  highIA_acc = rep(0, Nsubj)
  highIA_inac =rep(0, Nsubj)



    for(i in 1:length(lia_subjs)){ 
    #print("Low Initial Accuracy condition (12/18 pairs wrong at familiarization)")
    ord <- get_subj_ord(lia_subjs[i]) 
    lom = model(par, ord=ord, m=lowIA_m)
    acc18afc = diag(lom) / rowSums(lom)
    mean(acc18afc) # low initial accuracy is good for the model (like adults)
    lowIA_inac[i] = mean(acc18afc[LIA_initInac]) # Low IA Initially Inaccurate
    lowIA_acc[i] = mean(acc18afc[LIA_initAcc]) # Low IA Initially Accurate
    }
    for(i in 1:length(hia_subjs)){
    ord <- get_subj_ord(hia_subjs[i])  
    #print("High Initial Accuracy condition (4/10 pairs wrong at familiarization)")
    him = model(par, ord=ord, m=highIA_m)
    acc18afc = diag(him) / rowSums(him)
    mean(acc18afc) # high initial accuracy actually hinders the model somehow...cool!
    highIA_inac[i] = mean(acc18afc[HIA_initInac]) #
    highIA_acc[i] = mean(acc18afc[HIA_initAcc]) # 
    }
  
  mdat[2,]$Mean = mean(lowIA_inac) # Low IA Initially Inaccurate
  mdat[2,]$SD = sd(lowIA_inac) 
  mdat[1,]$Mean = mean(lowIA_acc) # Low IA Initially Accurate
  mdat[1,]$SD = sd(lowIA_acc) # 
  
  mdat[4,]$Mean = mean(highIA_inac) #
  mdat[4,]$SD = sd(highIA_inac) 
  mdat[3,]$Mean = mean(highIA_acc) # 
  mdat[3,]$SD = sd(highIA_acc) # 
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

