DepthBreadth_function <- function( depth_data, num_sample, name_sample){
  #names col in data frame
  numCol_Depth<-num_sample+1
  numCol_Breath<-numCol_Depth+1
  colnames(df_DepthBreath)[numCol_Depth:numCol_Breath]<- c(paste0("Depth_",name_sample), paste0("Breath_",name_sample))

  # walking in the list to calculate the depth and breath
  number_effector<- length(unique(depth_data$scaffold))
  # the lsit is to split depends the effectors
  listEffector<- split(depth_data, depth_data$scaffold)

  # for each effector calculate the depth and breath
  for(i in 1:number_effector){
   temporal_dataFrame<-listEffector[[i]]

    ## DEPTH
    depth<- mean(temporal_dataFrame[!(temporal_dataFrame$depth==0),3])
    df_DepthBreath[i, numCol_Depth]<- depth
  
    ## BREATH of coverage
    # length of all the effector
    effectorLength<-dim(temporal_dataFrame)[1]
    # lenght position differences to 0
    #my_effectorLength<-length(which(listEffector[[i]][3]>0, arr.ind = FALSE, useNames = TRUE))
    my_effectorLength<-dim(temporal_dataFrame[!(temporal_dataFrame$depth==0),])[1]

    breadth<- (my_effectorLength/effectorLength)*100
    df_DepthBreath[i, numCol_Breath]<- breadth
  } 
}
