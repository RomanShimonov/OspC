x <- read.csv("~/Desktop/epitope_project/Allignments/windowout.csv", sep="\t", header=F)
head(x)
y <- read.csv("~/Desktop/epitope_project/all_3_averages.csv", sep=",", header=F)
head(y)
y[2,1]

ospv<-c()

for (i in 1:22) 
{
  for (j in (i+1):23) 
  {
    ospv[length(ospv)+1]<-y[i,j]
  }
}

length(ospv)
head(x)
osp.cr<-c(rep(ospv,179))
length(osp.long)
osp.df<-x
osp.df$V7<-osp.cr
head(osp.df)
colnames(osp.df)<-c('o1','o2','pwd','l','start','end','CRp')
cor.vect<-c()
for(i in 0:178) {
cor.vect[length(cor.vect)+1]<-cor(osp.df[(i*253+1):((i+1)*253),3],osp.df[(i*253+1):((i+1)*253),7])
}
plot(cor.vect)  
Window_Frame<-seq(20,198,1)
df<-data.frame(Window_Frame,cor.vect)
plot(df)

