#  Trueswell et al 2013 model:
# 1. guess at chance, 2. next time a word occurs, remember previous guess w prob alpha
# 3. if the remembered guess is present, increase alpha; otherwise choose a new random guess
# was:
# hypothesis-testing model based on Medina, Snedeker, 
# Trueswell, & Gleitman, 2011's verbal description:
# one-trial / "fast mapping" hypothesis:
#  i) learners hypothesize a single meaning based on their first encounter with a word
# ii) learners neither weight nor even store back-up alternative meanings
# iii) on later encounters, learners attempt to retrieve this hypothesis from memory and test it against a new context, updating it only if it is disconfirmed
# Thus, they do not accrue a "best" final hypothesis by comparing multiple episodic memories of prior contexts or multiple semantic hypotheses.

# for hypothesis/response-training condition, want to track correct responses at each occurrence of a word

model <- function(params, ord=c(), m=c(), ord_name="", reps=1, verbose=F, save_traj=F) {
	alpha = params[1] # prob to remember first guess
	alpha_increase = params[2] # Trueswell 2013 empirically estimates this...
	#sa <- params[2] # prob of storage (slow learning down)

	voc_sz = max(unlist(ord), na.rm=TRUE) # vocabulary size
	ppt = ncol(ord) # pairs per trial
	#m <- matrix(0, voc_sz, voc_sz) # hypothesis matrix
  
	# want an item x occurrence matrix, to be filled in during training 
	resps = matrix(0, voc_sz, 9)
	freq = rep(0,voc_sz) # number of occurrences per pair, so far (to index the resps matrix)
  
	#mem_strength = rep(0,voc_sz) # how strong a w's hypothesis is (strengthens if confirmed)
	perf = c()
	for(rep in 1:reps) {
		for(t in 1:nrow(ord)) {
			tr_w = ord[t,c("w1ind","w2ind")]
    	tr_o = ord[t,c("o1ind","o2ind")]
			freq[tr_w] = freq[tr_w] + 1 
			probs = runif(length(tr_w)) # prob for each word of remembering it's hypothesized obj
			forget = tr_w[which(probs > rowSums(m[tr_w,]))] # forget all objs a word is associated with
			#remember = tr[which(probs <= mem_strength[tr])]
			m[forget,] = m[forget,]*0
			have_hypoths = tr_w[which(rowSums(m[tr_w,])!=0)] # throw out inconsistent ones
			for(w in have_hypoths) {
				hypo = which(m[w,]>0) # hypothesized object
				if(!is.element(hypo, tr_o)) { 
					m[w,] = m[w,]*0 # disconfirmed
				} else {
					m[w,hypo] = m[w,hypo] + alpha_increase # strengthen
          for(s in hypo) {
            resps[w,freq[s]] = 1 # know that
          }
				}
			}
			words_need_hypoths = tr_w[which(rowSums(m[tr_w,tr_o])==0)] 
      objs_need_hypoths = tr_o[which(colSums(m[tr_w,tr_o])==0)]
			#store = need_hypoths[which(runif(length(need_hypoths)) < sa)]
			store = objs_need_hypoths
			if(length(store)==1) {
			  new_hyps = store
			} else {
			  new_hyps = sample(store, length(store), replace=FALSE)
			}
			for(w in 1:length(store)) {
				m[words_need_hypoths[w], new_hyps[w]] = alpha
        			resps[w,freq[w]] = 1 # make a guess
			}
		}
		perf = c(perf, sum(diag(m)>0)/voc_sz)
		if(verbose) print(m)
	}
	return(m)
	#return(list(resps=resps, final=perf))
}
