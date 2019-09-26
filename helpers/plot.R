############################
# Two-dimensional plotting #
############################
convert_array_to_df = function(arrayout) {
  df = as.data.frame.table(arrayout, base = list(paste0("part", 1:dim(arrayout)[1]),
                                                 paste0("dim", 1:dim(arrayout)[2]),
                                                 as.character(1:dim(arrayout)[3])))
  df[,3] = as.numeric(df[,3])
  
  df1 <- spread(data = df, key = Var2, value = Freq) # spread the dimension component
  head(df1)
  names(df1) = c("particle", "iteration", "dim1", "dim2")
  
  return(df1)
}

plotting = function(df, axes = FALSE) {
  
  # df$dim1 = df$dim1 %% bound
  # df$dim2 = df$dim2 %% bound
  
  # En vue de dessus, trajectoire de chaque particule (tous les x pas)
  my_gg = ggplot(df, aes(x = jitter(dim1), y = jitter(dim2), colour = particle, alpha = alpha)) + 
    geom_path() 
  
  if(!axes) {
    my_gg = my_gg +
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
  }
  return(my_gg)
}

############################
# One-dimensional plotting #
############################
