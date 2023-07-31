### This dataset is for Lafayatis Skin Data

setwd("/ix/djishnu/Javad/Hierarchical_ER_v2/R/")
files <- list.files()
sapply(files, source)

## a) Real, b) Random size-matched marginal + interactors,
################################################################################
Z  <- read.table("/ix/djishnu/Javad/Lafayatis/z_matrix.csv",sep=",",header=T,row.names = 1)
y  <- read.table("/ix/djishnu/Javad/Lafayatis/Final_SLIDE_081222/SkinScore_MRSS.csv",row.names = 1,sep=",",header = T)

#Real ##########################################################################


sigK  <- c(12,47)
sigIn <- c("Z12.Z99","Z47.Z6","Z47.Z85") 


IntData        <- interUnion(sigK,Z)
Dataint        <- IntData$interaction[,sigIn]
Data_real      <- data.frame(y=y,Z[,sigK],Dataint)
SumReal            <- summary(lm(MRSS~.,data = Data_real))
SumReal$r.squared


#All random ####################################################################
Fullreport <- NULL

for(i in 1:100){
sigKRandom     <- sample(ncol(Z),size=length(sigK))                             ## Random marginal
IntDataRandom  <- interUnion(sigKRandom,Z)
sigInRandom    <- sample(ncol(IntDataRandom$interaction),size=length(sigIn))    ## Random interaction
IntDataRandom  <- IntDataRandom$interaction[,sigInRandom]
Data_fullRandom<- data.frame(y=y,Z[,sigKRandom],IntDataRandom)
SumInt            <- summary(lm(MRSS~.,data = Data_fullRandom))
SumInt$r.squared
Fullreport <- rbind(Fullreport,sqrt(SumInt$r.squared))
}

#readRDS("r. 5,hngl.fdrghbn. edxs9g'")

## c) Real marginal + random size-matched interactors (all outside CV)
Partialreport <- NULL
for(i in 1:100){                                                              
IntData        <- interUnion(sigK,Z)
sigInRandom    <- sample(ncol(IntData$interaction),size=length(sigIn))          ## Ranodm interaction
IntDataRandom  <-IntData$interaction[,sigInRandom]
Data_partialRandom<- data.frame(y=y,Z[,sigK],IntDataRandom)
SumPInt            <- summary(lm(MRSS~.,data = Data_partialRandom))
SumPInt$r.squared
Partialreport <- rbind(Partialreport,sqrt(SumPInt$r.squared))
}
################################################################################
## Report
library(ggplot2)
library(reshape2)
rawdf       <- data.frame(FullRandom=Fullreport,PartialRandom=Partialreport)
df          <- melt(rawdf)
colnames(df)<-c("group","value")

cols <- c("#0000FF", "#00FF00")

# Basic density plot in ggplot2
P2 <- ggplot(df, aes(x           = value, fill = group)) +
  geom_density(alpha            = 0.7) +xlim(0,1)+
  scale_fill_manual(values      = cols) +
  theme_light() +
  geom_vline(xintercept         = c(sqrt(SumReal$r.squared)), 
             linetype = "longdash",
             colour = "red",size=2) + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("Density") +
  xlab("Pearson Correlation")


P3 <- P2 + xlim(c(0.5,1))

 
ggsave(P2,filename = "/ix/djishnu/Javad/Lafayatis/Plots/randomvsPartialPlot_all.pdf")
ggsave(P2,filename = "/ix/djishnu/Javad/Lafayatis/Plots/randomvsPartialPlot_all.png")

## Limited
ggsave(P3,filename = "/ix/djishnu/Javad/Lafayatis/Plots/randomvsPartialPlot_limit.pdf")
ggsave(P3,filename = "/ix/djishnu/Javad/Lafayatis/Plots/randomvsPartialPlot_limit.png")

################################################################################
ERres          <- readRDS("/ix/djishnu/Javad/Lafayatis/Final_SLIDE_081222/final_er_output.rds")
beta_positive  <- order(ERres$beta,decreasing = T)[2]
beta_negative  <- order(ERres$beta,decreasing = F)[2]

ordered_beta   <- c(beta_positive,beta_negative)
marginal_vars  <- c("Z12","Z47")
databeta <- data.frame(y=as.matrix(y),Z[,ordered_beta])
sum <-  summary(lm(MRSS~.,data=databeta))



SLIDEInt <- interUnion(marginal_vars =marginal_vars ,z =Z )
Interaction         <- SLIDEInt$interaction[,sigIn]
dataSLIDE <- data.frame(y=y,Z[,marginal_vars],Interaction)
sumSlide <- summary(lm(MRSS~.,data = dataSLIDE ))

###############################################################################
topvsSLIDE <- data.frame(Method=c('SLIDE','ER'),values=c(sqrt(sumSlide$r.squared),sqrt(sum$r.squared)))



P <- ggplot(topvsSLIDE, aes(x=Method, y=values, fill=Method))+
  geom_col(width = 0.25,color="black")+coord_cartesian(ylim=c(0.5,0.9))+
  theme_minimal()+scale_fill_manual(values=c("#83b5dd","#f3ae59"))+
  ylab("Spearman Correlation")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))






ggsave(P,filename = "/ix/djishnu/Javad/Lafayatis/Plots/Controlplot_ssc.pdf")
ggsave(P,filename = "/ix/djishnu/Javad/Lafayatis/Plots/Controlplot_ssc.png")
ggsave("/ix/djishnu/Javad/Lafayatis/Plots//Controlplot_SSC.pdf")



