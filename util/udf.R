par(family = 'Kaiti SC')

# not in 
'%!in%' <- function(x,y)!('%in%'(x,y))

#

MyivPlot <- function(df, variableName, plot_path){
  ivmatrix <- as.data.frame(iv.mult(df,"default",vars = variableName))
  ivmatrixname <- paste(variableName, "_woe.csv", sep = "")
  
  woeoutputpath <- str_c(plot_path, "woe", ivmatrixname, sep = "/")
  print(woeoutputpath)
  write_csv(ivmatrix, woeoutputpath)
  iv_info <- iv.mult(df,"default",vars = variableName, summary=FALSE)
  iv_info[[1]]$class <- as.character(iv_info[[1]]$class)
  iv_info[[1]]$class[index(iv_info[[1]])] <- paste(index(iv_info[[1]]), iv_info[[1]]$class,sep = "_")
  IVplot <- iv.plot.woe(iv_info)
  plotname <- paste(variableName,".jpg",sep = "")

  IVplotoutputpath <- str_c(plot_path,"IVplot", plotname, sep="/")
  print(IVplotoutputpath)
  ggsave(filename = IVplotoutputpath, plot = IVplot)
  IVplot
}

scoreCalculator <- function(df, coe, variableName, i){
  x1 <- as.data.frame(iv.mult(df,"default",vars = variableName))
  cn_temp <- levels(x1$class)
  score <- x1$woe*(-1)*B*coe[i]
  c1 <- cbind(cn_temp, score)
  print(colnames(c1))
  colnames(c1)[1] <- variableName
  print(variableName)
  scorefilename <- paste(variableName, ".csv", sep = "")
  scorePath <- str_c(projectPath,"output","score", scorefilename, sep="/")
  print(scorePath)
  write.csv(c1, scorePath)
  iv.mult(df,"default",vars = variableName)
  IVplot <- iv.plot.woe(iv.mult(step2_3,"default",vars = variableName, summary=FALSE))
  plotname <- paste(variableName,".jpg",sep = "")
  IVplotoutputpath <- str_c(projectPath,"output","score", plotname, sep="/")
  print(IVplotoutputpath)
  ggsave(filename = IVplotoutputpath, plot = IVplot)
}


# badratePlot <- function(df, feature, Y = "default"){
#   tmp_df  <- group_by(df, feature)
#   tmp_df <- tmp_df[,c(feature, Y)]
#   tmp_df <- ddply(tmp_df, ~feature, summarise ,mean = mean(default))
#   tmp_df <- tmp_df[order(tmp_df$mean),]
#   row.names(tmp_df) <- 1:nrow(tmp_df)
#   tmp_df
#   
#   
#   ggplot(tmp_df, aes(x=feature, y=mean,group=1)) + geom_line() + geom_point()
# }