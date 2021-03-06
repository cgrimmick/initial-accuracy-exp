# Associative Uncertainty- (Entropy) & Familiarity-Biased Model
# George Kachergis  gkacherg@indiana.edu  June 10, 2011

shannon.entropy <- function(p) {
	if (min(p) < 0 || sum(p) <= 0)
		return(NA)
	p.norm <- p[p>0]/sum(p)
	-sum(log2(p.norm)*p.norm)
	}

update_known <- function(m, tr) {
	startval = .01
		
	for(i in tr) {
		for(c in 1:dim(m)[2]) {
			if(sum(m[,c]>0) & m[i,c]==0) {
				m[i,c] = startval
				m[c,i] = startval
			}
		}
		for(j in tr) {
			if(m[i,j]==0) m[i,j] = startval
			if(m[j,i]==0) m[j,i] = startval
			}
		}
		return(m)
	}


model <- function(params, ord=c(), ord_name="", reps=1, name="model", save_traj=FALSE, print_matrix=FALSE) {
	X <- params[1] # associative weight to distribute
	B <- params[2] # weighting of uncertainty vs. familiarity
	C <- params[3] # decay
	
	# learning trajectory data
	if(save_traj) {
		traj <- data.frame(Trial=numeric(0), testtype=character(0), x=numeric(0.0))
		ord <- read.table(paste(ord_dir,ord_name,".txt",sep=""), header=T) # ordering
		}
	
	voc_sz = max(unlist(ord), na.rm=TRUE) # vocabulary size
	ppt = length(ord$trials[[1]]$words) # pairs per trial
	mean_ent = c()
	m <- matrix(0, voc_sz, voc_sz) # association matrix
	# training
	for(rep in 1:reps) { # for trajectory experiments, train multiple times
	  for(t in 1:length(ord$trials)) { 
		#print(format(m, digits=3))
		
		tr = as.integer(ord$trials[[t]]$words)
		tr = tr[!is.na(tr)]
		m = update_known(m, tr) # what's been seen so far?
		ent = c() # more entropy = more dispersive
		for(w in tr) { ent = c(ent, shannon.entropy(m[w,])) }
		mean_ent = c(mean_ent, sum(ent)/ppt)
		ent = exp(B*ent)
		
		# get all current w,o strengths and normalize to distr X
		assocs = m[tr,tr]
		
		denom = sum(assocs * (ent %*% t(ent)))
		
		m = m*C # decay everything
		
		m[tr,tr] = m[tr,tr] + (X * assocs * (ent %*% t(ent))) / denom # update assocs
		
		if(print_matrix) print(m)
		
		if(save_traj & t>1) { # saving model learning trajectories for graphing
			modelx <- test(m+.01)
			modela <- test_abs(m+.01)
			traj <- rbind(traj, data.frame(Trial=t, testtype="earlyatoa", x=modelx[1]))
			traja <- rbind(traja, data.frame(Trial=t, testtype="earlyatoa", x=modela[1]))
		}
	  }
	}
	if(save_traj) {
		save(traj, file=paste(name,"_",ord_name,"_traj.RData",sep=''))
		save(traja, file=paste(name,"_",ord_name,"_abs_traj.RData",sep=''))
		}
	m = m+.01
	want = list()
	#print(plot(1:dim(ord)[1], mean_ent)) # col=perf[perf<.5]
	#want[['tr_ent']] = mean_ent
	#want[['perf']] = perf
	return(m)
	}

