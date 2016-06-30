# v1 used 18 pairs (+1 novel at test) repeated 3 times, with 3s per item per trial
# (378 s total study time with 1000ms ISI) -- 6.3 min

require("ggplot2")

preprocess <- function() {
	#instructdat = read.csv("initial-accuracy1_instructquiz_data.csv")
	studydat = read.csv("initial-accuracy1_study_data.csv")
	testdat = read.csv("initial-accuracy1_test_data.csv")
	postqdat = read.csv("initial-accuracy1_postquiz_data.csv")
	
	print(paste(length(unique(testdat$uniqueId)), "subjects"))
	
	cheaters = subset(postqdat, memory_aid=="yes")$uniqueId
	print(paste(length(cheaters), "cheaters"))
	# also see if anybody took a ridiculous number of times to do instruct quiz

	testdat = subset(testdat, !is.element(uniqueId,cheaters))
	postqdat = subset(postqdat, !is.element(uniqueId,cheaters))
	studydat = subset(studydat, !is.element(uniqueId,cheaters))
	
	testdat$condition = as.character(testdat$condition)
	studydat$condition = as.character(studydat$condition)
	#testdat$InitAcc = with(testdat, ifelse(condition=="Low Initial Accuracy", "Low", "High")) 
	
	return(list(test=testdat, quiz=postqdat, study=studydat))
}

dat = preprocess()

# 1 novel pair was tested for each subject:
novel = subset(dat$test, is.na(init_word))
mean(novel$correct) # .67 (more likely the more other items you got right?)

agg_s = aggregate(cbind(correct, rt) ~ uniqueId + init_acc + condition, data=dat$test, mean)
ag = aggregate(cbind(correct, rt) ~ init_acc + condition, data=agg_s, mean)
summary(aov(correct ~ condition*init_acc + Error(uniqueId), data=agg_s))

hist(agg_s$correct)

agg_s = aggregate(cbind(correct, rt) ~ condition + pairs + shuffled + uniqueId, data=dat$test, mean)
summary(aov(correct ~ pairs + shuffled + Error(uniqueId), data=agg_s))
agg = aggregate(correct ~ pairs + shuffled, data=agg_s, mean)
agg$sd = aggregate(correct ~ pairs + shuffled, data=agg_s, sd)$correct
agg$SE = agg$sd / sqrt(aggregate(correct ~ pairs + shuffled, data=agg_s, length)$correct-1)

table(agg_s$pairs, agg_s$shuffled)

dodge = position_dodge(width=.3)
limits <- with(agg, aes(ymax=correct+SE, ymin=correct-SE))
ggplot(data=agg, aes(x=as.factor(pairs), y=correct, group=shuffled, colour=shuffled)) + geom_line(position=dodge) + geom_point(position=dodge) + theme_bw() + ylab("Proportion Correct") + xlab("Pairs per Trial") + geom_errorbar(limits, width=.2, position=dodge) 
	ggsave("perf_by_pairs_and_shuffle.pdf", width=4, height=4)

ggplot(agg_s, aes(x=correct, fill=condition)) + geom_density(alpha=.3)
ggsave("density_by_condition.pdf", width=5, height=4)

library(plyr)
cdat <- ddply(agg_s, "condition", summarise, correct.mean=mean(correct))
# Overlaid histograms with means
ggplot(agg_s, aes(x=correct, fill=condition)) + geom_histogram(binwidth=.2, alpha=.3, position="identity") + geom_vline(data=cdat, aes(xintercept=correct.mean, colour=condition), linetype="dashed", size=1)
