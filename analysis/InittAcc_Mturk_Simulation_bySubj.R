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
testdat = read.csv("data/initial-accuracy1_test_data.csv", header=T) 
testdat = na.omit(testdat) # get rid of the single novel pair tested per subject

get_subj_ord <- function(cur_subj_dat) {
  ord = data.matrix(cur_subj_dat[,c(8,9,10,11)]) + 1
  return(ord)
}

median_group_pars = c(0.6111021, 4.635534, 0.9956708)

FitChristSSE <- function(par, humdat, cond, ord, regularize=F) {
  mdat = RunFitnevaChristiansenExp(par, cond, ord)
  SSE = sum((humdat - mdat)^2)
  if(regularize) {
    #SSE = SSE + .002*sum( (par-c(0,0,1))^2 ) # penalize values far from 0 (or extreme decays)
    SSE = SSE + .002*sum( (par-median_group_pars)^2 )
  }
  return(SSE) 
}

RunFitnevaChristiansenExp <- function(par, cond, ord) {
  # par = c(.2, 2, .95)
  # mdat = data.frame(Cond=c("Low IA","Low IA", "High IA", "High IA"), 
  #                   Item=c("Initially Accurate","Initially Inaccurate","Initially Accurate","Initially Inaccurate"),
  #                   Mean=NA, SD=NA)
  X = par[1]*(par[3]^2)

  wordOrder = c(1,3,5,7,9,11,13,15,17,0,2,4,6,8,10,12,14,16) +1
  objOrder =  c(0,2,4,6,8,10,12,14,16,1,3,5,7,9,11,13,15,17) + 1
  
  # familiarization is the same for both conditions (the switching happens after)
  fam_m = matrix(0, nrow=18, ncol=18)
  for(i in 1:length(wordOrder)) {
    fam_m[wordOrder[i],objOrder[i]] = X
  }

  modp = model(par, ord=ord, m=fam_m) # does the training (on prefam matrix)
  acc18afc = rep(NA, 18)
  for(j in 1:18){
    acc18afc[j] = modp[wordOrder[j],objOrder[j]] / sum(modp[wordOrder[j],])
  }

  if(cond=="Low Initial Accuracy") {
    liawordOrder =  c(16,14,5,10,9,6,13,2,0,17,15,4,11,8,7,12,3,1) + 1
    LIA_initInac = which(wordOrder!=liawordOrder)
    LIA_initAcc = which(wordOrder==liawordOrder)
    #print("Low Initial Accuracy condition (12/18 pairs wrong at familiarization)")
    init_inacc = mean(acc18afc[LIA_initInac]) # Low IA Initially Inaccurate
    init_acc = mean(acc18afc[LIA_initAcc]) # Low IA Initially Accurate

  } else if(cond=="High Initial Accuracy") {
    hiawordOrder =  c(1,14,5,10,9,6,13,15,17,0,2,4,11,8,7,12,3,16) + 1
    HIA_initInac = which(wordOrder!=hiawordOrder) # initInac item indices in HiIA cond
    HIA_initAcc = which(wordOrder==hiawordOrder)
    #print("High Initial Accuracy condition (4/10 pairs wrong at familiarization)")
    init_inacc = mean(acc18afc[HIA_initInac]) #
    init_acc = mean(acc18afc[HIA_initAcc]) # 
  }
  
  return(c(init_inacc, init_acc))
}

# RunFitnevaChristiansenExp(c(.2, 2, .95)) # need Low IA 10% greater than High IA -- and init accuracy only helps slightly

# from Fitneva & Christiansen, 2015 (10 items, 60%/40%, 2AFC test):
#humdat = data.frame(Cond=c("Low IA","Low IA", "High IA", "High IA"), 
#                  Item=c("Initially Accurate","Initially Inaccurate","Initially Accurate","Initially Inaccurate"),
#                  Mean=c(.91,.84,.76,.74), SD=NA)

# our results (19AFC, 18 items)
# humdat = data.frame(Cond=c("Low IA","Low IA", "High IA", "High IA"), 
#                     Item=c("Initially Accurate","Initially Inaccurate","Initially Accurate","Initially Inaccurate"),
#                     Mean=c(.658,.458,.614,.288), SE=c(.064, .062, .075, .056))


# FitChristLogLik <- function(par, humdat, cond, ord) {
#   mdat = RunFitnevaChristiansenExp(par, cond, ord)
#   return(sum((humdat - mdat)^2))
# }

require("DEoptim")

fitBySubject <- function() {
  source("FitChristmodel.R")
  subjs <- unique(as.character(testdat$uniqueId))
  agS = aggregate(correct ~ condition + init_acc + uniqueId, data=testdat, mean) 
  parnames = c("X","lambda","alpha")
  agS[,parnames] = NA
  agS$SSE = NA
  agS$Model = NA
  # fit each subject (with appropriate condition), and store the best-fitting parameters per S
  for(s in subjs) {
    ord = get_subj_ord(subset(study, uniqueId==s))
    srows = which(agS$uniqueId==s)
    scond = agS[srows,]$condition[1]
    sperf = agS[srows,]$correct
    fit = DEoptim(FitChristSSE, lower=c(.001,.1,.5), upper=c(2,20,1), DEoptim.control(reltol=.001, NP=100), humdat=sperf, 
                  cond=scond, ord=ord, regularize=T) # 
    mperf = RunFitnevaChristiansenExp(fit$optim$bestmem, scond, ord) # fit$optim$bestmem c(2.0, 0.787, 0.684)
    agS[srows,]$SSE = fit$optim$bestval # .0002
    for(row in srows) {
      agS[row,parnames] = fit$optim$bestmem
    }
    agS[srows,]$Model = mperf
  }
  return(agS)
}

sfits = fitBySubject()

print(paste("Median SSE:",round(median(sfits$SSE),4),"sd:",round(sd(sfits$SSE),4)))
bad_fits = subset(sfits, SSE > sum(median(sfits$SSE)+sd(sfits$SSE)))
print(bad_fits) # mostly in the Low Init Acc condition, subjects which had very high performance (e.g., 100%), 
# or higher for init-inacc than init-acc -- the model just can't do that
aggregate(correct ~ condition + init_acc, data=bad_fits, mean) 

# how many subjects had higher accuracy on initially-inaccurate vs. initially-inaccurate?
# (and can the model ever do that?)
iacc = subset(sfits, init_acc=="True")
iinacc = subset(sfits, init_acc=="False")
ia_gt_inac = iacc$correct - iinacc$correct
sort(ia_gt_inac) # six subjects had lower accuracy on initially-inaccurate pairings (four of those were only .083 worse at initially-accurate)
hist(iacc$correct - iinacc$correct)


save(sfits, file="subject_fits_withReg.RData")

# adults
# fit = DEoptim(FitChristSSE, lower=c(.001,.1,.5), upper=c(2,20,1), DEoptim.control(reltol=.001, NP=100), humdat=humdat) # 
# mad = RunFitnevaChristiansenExp(fit$optim$bestmem) # fit$optim$bestmem c(2.0, 0.787, 0.684)
# fit$optim$bestval # .0002

# mad$Cond = factor(mad$Cond, levels=c("Low IA", "High IA"))

require("ggplot2")
load("subject_fits_withReg.RData") # 
#load("subject_fits_withoutReg.RData") # mean SSE=.030

agg = aggregate(correct ~ condition + init_acc, data=sfits, mean) 
agg$sd = aggregate(correct ~ condition + init_acc, data=sfits, sd)$correct
agg$SE = agg$sd / sqrt(aggregate(correct ~ condition + init_acc, data=sfits, length)$correct - 1)

mag = aggregate(Model ~ condition + init_acc, data=sfits, mean) 
mag$sd = aggregate(Model ~ condition + init_acc, data=sfits, sd)$Model
mag$SE = agg$sd / sqrt(aggregate(Model ~ condition + init_acc, data=sfits, length)$Model - 1)

dodge <- position_dodge(width=.9)
limits <- with(agg, aes(ymax=correct+SE, ymin=correct-SE))
a <- ggplot(agg, aes(x=condition, y=correct, fill=init_acc)) + labs(x="Condition", y="Proportion Correct", fill="Initially Accurate") + 
  geom_bar(stat="identity", position=dodge) + geom_errorbar(limits,  width=0.2, position=dodge) +
  geom_point(data=mag, aes(x=condition, y=Model, fill=init_acc), position=dodge) + 
  geom_hline(yintercept=1/18, linetype='dashed')
#b <- a + geom_hline(yintercept=1/18, linetype='dashed') # + scale_fill_manual(values=c("red", "orange", "yellow"))
print(a)
ggsave("initial-accuracy18afc_model_fit_by_subject2.pdf", width=5, height=4)
dev.off()

