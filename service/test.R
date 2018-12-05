crf_rowname <- NULL
for(i in 1:length(c[,1])){
  if(c[i,1]>0.001){
    crf_rowname <- c(crf_rowname, rownames(c)[i])
  }
}

crf_rowname <- c("default",crf_rowname)
crf_rowname

additiveCols <- c("td_score","zm_score")

boolean <- additiveCols %in% crfcols

for (i in 1:length(boolean)) {
  if (!boolean[i]) {
    crfcols <- c(crfcols, additiveCols)
  }
}