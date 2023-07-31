### This dataset is for Lafayatis Skin Data

#ss <- list.files("/ix/djishnu/Javad/Hierarchical_ER_v2/R/")
#for (i in ss) source(paste0("/ix/djishnu/Javad/Hierarchical_ER_v2/R/", i))

library(SLIDE)
## a) Real, b) Random size-matched marginal + interactors,
################################################################################
Z <- read.table("/ix/djishnu/Javad/CDexpansion/data/z_matrix.csv", sep = ",", header = T, row.names = 1)
y <- read.table("/ix/djishnu/Javad/CDexpansion/data/expansion_y.csv", row.names = 1, sep = ",", header = T)
colnames(y) <- "y"

# Real ##########################################################################


sigK <- c(19, 9)
sigIn <- c("Z19.Z28", "Z9.Z29")


IntData <- interUnion(sigK, Z)
Dataint <- IntData$interaction[, sigIn]
Data_real <- data.frame(y = y, Z[, sigK], Dataint)

SumReal <- summary(lm(y ~ ., data = Data_real))
SumReal$r.squared


# All random ####################################################################
Fullreport <- NULL

for (i in 1:100) {
  sigKRandom <- sample(ncol(Z), size = length(sigK)) ## Random marginal
  IntDataRandom <- interUnion(sigKRandom, Z)
  sigInRandom <- sample(ncol(IntDataRandom$interaction), size = length(sigIn)) ## Ranodm interaction
  IntDataRandom <- IntDataRandom$interaction[, sigInRandom]
  Data_fullRandom <- data.frame(y = y, Z[, sigKRandom], IntDataRandom)
  SumInt <- summary(lm(y ~ ., data = Data_fullRandom))
  SumInt$r.squared
  Fullreport <- rbind(Fullreport, sqrt(SumInt$r.squared))
}

# readRDS("r. 5,hngl.fdrghbn. edxs9g'")

## c) Real marginal + random size-matched interactors (all outside CV)
Partialreport <- NULL
for (i in 1:100) {
  IntData <- interUnion(sigK, Z)
  sigInRandom <- sample(ncol(IntData$interaction), size = length(sigIn)) ## Ranodm interaction
  IntDataRandom <- IntData$interaction[, sigInRandom]
  Data_partialRandom <- data.frame(y = y, Z[, sigK], IntDataRandom)
  SumPInt <- summary(lm(y ~ ., data = Data_partialRandom))
  SumPInt$r.squared
  Partialreport <- rbind(Partialreport, sqrt(SumPInt$r.squared))
}
################################################################################
## Report
library(ggplot2)
library(reshape2)
rawdf <- data.frame(FullRandom = Fullreport, PartialRandom = Partialreport)
df <- melt(rawdf)
colnames(df) <- c("group", "value")

cols <- c("#0000FF", "#00FF00")

# Basic density plot in ggplot2

  P2 <- ggplot(df, aes(x           = value, fill = group)) +
    geom_density(alpha            = 0.7) +
    scale_fill_manual(values      = cols) +
    theme_light() +
    geom_vline(xintercept         = c(sqrt(SumReal$r.squared)), 
               linetype = "longdash",
               colour = "red",size=2) +
    ylab("Density") +
    xlab("Pearson Correlation")
  P2 <- P2 + annotate("text", x     = sqrt(SumReal$r.squared) + 0.01, 
                    y = 55, 
                    label = " ", 
                    angle = "90") +
    xlim(0.4, 0.6)
  P2 <- P2 + theme(panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 axis.text = element_text(size = 20),
                 axis.title.x =element_text(size = 20),
                 axis.title.y = element_text(size = 20))
  
  
  P3 <- P2 +xlim(0, 0.6)+geom_vline(xintercept         = c(sqrt(SumReal$r.squared)), 
                                    linetype = "longdash",
                                    colour = "red",size=1)



saveRDS(df, file                = "/ix/djishnu/Javad/CDexpansion/Plots/ggplotobject_randompar_data.rds")
saveRDS(P2, file                 = "/ix/djishnu/Javad/CDexpansion/Plots/ggplotobject_randompar.rds")
ggsave(P2, filename              = "/ix/djishnu/Javad/CDexpansion/Plots/randomvsPartialPlot_limited.pdf")
ggsave(P2, filename              = "/ix/djishnu/Javad/CDexpansion/Plots/randomvsPartialPlot_limited.png")

ggsave(P3, filename              = "/ix/djishnu/Javad/CDexpansion/Plots/randomvsPartialPlot_all.pdf")
ggsave(P3, filename              = "/ix/djishnu/Javad/CDexpansion/Plots/randomvsPartialPlot_all.png")

################################################################################

ERres <- readRDS("/ix/djishnu/Javad/CDexpansion/data/final_er_output.rds")
beta_positive <- order(ERres$beta, decreasing = T)[1]
beta_negative <- order(ERres$beta, decreasing = F)[1]

ordered_beta <- c(beta_positive, beta_negative)

SLIDEres <- readRDS("/ix/djishnu/Javad/CDexpansion/data/SLIDE_res.rds")

databeta <- data.frame(y = y, Z[, ordered_beta])
sum <- summary(lm(y ~ ., data = databeta))

SLIDEres$SLIDE_res$interaction_vars

SLIDEInt <- interUnion(marginal_vars = SLIDEres$SLIDE_res$marginal_vars, z = Z)
Interaction <- SLIDEInt$interaction[, SLIDEres$SLIDE_res$interaction_vars]
dataSLIDE <- data.frame(y = y, Z[, SLIDEres$SLIDE_res$marginal_vars], SLIDEres$SLIDE_res$interaction_vals)
sumSlide <- summary(lm(y ~ ., data = dataSLIDE))

###############################################################################
topvsSLIDE <- data.frame(Method = c("SLIDE", "ER"), values = c(sqrt(sumSlide$r.squared), sqrt(sum$r.squared)))



P <- ggplot(topvsSLIDE, aes(x=Method, y=values, fill=Method))+
  geom_col(width = 0.25,color="black")+coord_cartesian(ylim=c(0.4,0.6))+
  theme_minimal()+scale_fill_manual(values=c("#83b5dd","#f3ae59"))+
  ylab("Spearman Correlation")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


ggsave(P, filename = "/ix/djishnu/Javad/CDexpansion/Plots/Controlplot_CD4Expansion.pdf")
ggsave(P, filename = "/ix/djishnu/Javad/CDexpansion/Plots/Controlplot_CD4Expansion.png")
saveRDS(P, file = "/ix/djishnu/Javad/CDexpansion/Plots/ggplotobject_cd4Expansion.rds")
