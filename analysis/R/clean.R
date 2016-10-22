require(plyr)

clean <- function(raw, minRT=250.0, maxRT=1500.0) {
  df <- raw[raw$whichPart != 'practice',]
  
  accuracy <- round(mean(df$isRight), 2)*100
  removed <- ((df$rt<minRT) | (df$rt>maxRT))
  percentage <- sum(removed)/length(df$rt)*100
  
  return(list('data' = df[!removed,],
              'acc'=accuracy,
              'minRT'=minRT,
              'maxRT'=maxRT,
              'num_removed'=sum(removed),
              'perct_removed'=round(percentage, 2)))
}

remove_no_fixation_trials <- function(dur) {
  dur.cleaned <- dur
  dur.cleaned$sum_dur <- rowSums(dur[, c('a','b','c','d')])
  removed <- (dur.cleaned$sum_dur == 0)
  percentage <- sum(removed)/length(dur$a)*100
  
  return(list('data'=dur[!removed,],
              'num_removed'=sum(removed),
              'perct_removed'=round(percentage, 2)))
}

# summarize latency and accuracies across a given grouping variable
summarize_by <- function(data, grouping, sds=3) {
  summary <- ddply(data, grouping, summarize, 
                   accuracy = mean(isRight),
                   rt_mean = mean(rt),
                   rt_sd = sd(rt))
  
  bad_accs <- summary[summary$accuracy < mean(summary$accuracy) - sds*sd(summary$accuracy),]
  if (length(bad_accs[,grouping]) > 0) {
    print(paste('Check', grouping, ':'))
    print(bad_accs)
  } else {
    print(paste('All accuracies by', grouping, 'within', sds, 'sds'))
  }
  
  bad_rts <- summary[summary$rt_mean > mean(summary$rt_mean) + sds*mean(summary$rt_sd),]
  if (length(bad_rts[,grouping]) > 0) {
    print(paste('Check', grouping, ':'))
    print(bad_rts)
  } else {
    print(paste('All RTs by', grouping, 'within', sds, 'sds'))
  }
  
  return(summary)
}

