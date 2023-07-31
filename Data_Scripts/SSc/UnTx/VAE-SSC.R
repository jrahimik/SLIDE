## VAE_v2
library(keras)
library(ggplot2)


latent_dim <- 10
keras::backend()



setwd("/ix/djishnu/Javad/Lafayatis")




x <- read.table("Var50_mtrp.csv",
                row.names = 1,
                sep=",",
                header = T)

y <- read.table("SkinScore_MRSS.csv",
                row.names = 1,
                sep = ",",
                header=T)


data_norm<- scale(x,T,T)
input_dim <-dim(data_norm)[2]


# encoder <- keras_model_sequential() %>%
#   layer_dense(units = 128, activation = "relu", input_shape = c(input_dim), kernel_initializer = "glorot_uniform") %>%
#   layer_dense(units = latent_dim*2)

encoder <- keras_model_sequential() %>%
  layer_dense(units = latent_dim*2, activation = "sigmoid", input_shape = c(input_dim), kernel_initializer = "glorot_uniform")
  #layer_dense(units = latent_dim*2)



decoder <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(latent_dim)) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = input_dim, activation = "sigmoid")
## no sampling
vae <- keras_model(inputs = encoder$input, outputs = decoder(encoder$output[, 1:latent_dim]))
# sampling




vae %>% compile(optimizer = "adam", loss = "CosineSimilarity")

# Train the VAE

vae_fit <- vae %>% fit(data_norm, data_norm, epochs = 100, batch_size = 24, validation_split = 0.2)
encoder_output <- stats::predict(encoder, data_norm)

loading <- encoder %>% get_weights()
vaeoadings <- encoder %>% get_weights()
vaeoadings <- vaeoadings[[1]][,1:latent_dim]
sumVAE <- summary(lm(as.matrix(y)~encoder_output[,1:latent_dim]))
vAERsqaured <- sumVAE$r.squared
vaeFactors <- encoder_output[,1:latent_dim]

saveRDS(vaeoadings,"C:/Users/javad/OneDrive - University of Pittsburgh/SLIDE/Lafyatis_SSc/All_Cell_Type/HER_082822/Results/Laodings/VAE.rds")
saveRDS()
################################################################################
library(MOFA2)
data_norm <- list()
data_norm$data$view_1 <-  t(scale(x,T,T))



MOFAmodel <- create_mofa(data_norm$data)
MOFAmodel <- prepare_mofa(MOFAmodel)
MOFAmodel <- run_mofa(MOFAmodel, use_basilisk = TRUE,outfile = "SSC-Mofa_Final.hdf5")
SSC_MOFA <- load_model(file = "SSC-Mofa_Final.hdf5",remove_inactive_factors = TRUE)
factors <- get_factors(SSC_MOFA, factors = "all")
MofaLoading <- MOFA2::get_weights(MOFAmodel)$view_1

saveRDS(MofaLoading,"C:/Users/javad/OneDrive - University of Pittsburgh/SLIDE/Lafyatis_SSc/All_Cell_Type/HER_082822/Results/Laodings/Mofa.rds")


SumMofa<- summary(lm(as.matrix(y)~factors$group1))


source("interUnion.R")


Z_ER <- read.table("z_matrix.csv",header = T,
                                  row.names = 1,sep = ",")
sigK  <- c(10,12,47,56,77)
sigIn <- c("Z12.Z99","Z47.Z6","Z47.Z85","Z77.Z90")


IntData        <- interUnion(sigK,Z_ER)
Dataint        <- IntData$interaction[,sigIn]
Data_real      <- data.frame(y=y,Z_ER[,sigK],Dataint)
SumReal            <- summary(lm(MRSS~.,data = Data_real))
SumReal$r.squared


## ggplot2
topvsSLIDE <- data.frame(Method=c('SLIDE','VAE','MOFA+'),values=c(sqrt(SumReal$r.squared),sqrt(vAERsqaured),sqrt(SumMofa$r.squared)))












P <- ggplot(topvsSLIDE, aes(x=Method, y=values, fill=Method)) +
  geom_col(width = 0.25, color="black") +
  coord_cartesian(ylim=c(0.4,1)) +
  theme_minimal() +
  scale_fill_manual(values=c("#83b5dd","#f3ae59","#00FF00")) +
  ylab("Spearman Correlation") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 12))

P
w <- encoder%>% get_weights()
W <- w[[1]][,1:10]
colnames(x)[order(abs(W[,10]),decreasing = T)][1:10]
################################################################################
#Mofa







