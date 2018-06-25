#sliding windows for protein files

#pw_difference- one window for pairwise differences.  Output is a matrix.Each row is of the form 
  # (row(i),row(j),pwdifference).  Inputs are data, window_size, and start. (There is no skip, yet)

pw_difference<- function(data,window_size,start) {
 output_frame<-data.frame(0,0,0)
   for (i in 1:(nrow(data)-1)) {
     for(j in (i+1):nrow(data)) {
       sum<-0
       for(k in start:(start+window_size-1)) {
         ifelse (abs(data[i,k]-data[j,k])==0,sum<-sum,sum<-sum+1) 
       }
       output_frame<-rbind(output_frame,c(i,j,sum))
     }
     
}
 output_frame<-output_frame[-1,]
 colnames(output_frame)<-c("a","b","c")
 return(output_frame)
 }

#Sliding windows function- use lapply on data and pw_difference function 
  # the output is a list of matrices, one for each window.

sliding_windows<-function(data,window_size,start) {
  
  a<-as.list(seq(1,ncol(data)-window_size+1))
  matrix_list<- lapply(a,pw_difference,data=data,window_size=window_size)
  return(matrix_list)
}


#testing sliding windows

#x<-c(0,1)
#sample(x,7,replace=T)
#df<-as.data.frame(matrix(sample(x,24,replace=T),nrow=3))
#sliding_windows(df,4,1)
