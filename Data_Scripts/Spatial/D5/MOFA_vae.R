## VAE_Day_5_spatial
library(keras)
library(ggplot2)
library(SLIDE)
library(dplyr)
library(keras)

input_dim  <-2342
latent_dim <- 12

# unique(unlist(strsplit(slideRes$interaction_vars,split = "[.]")))
#[1] "Z46"  "Z2"   "Z89"  "Z58"  "Z96"  "Z130" "Z117" "Z7"   "Z113" "Z115" "Z138" "Z30"






x <- read.table("/ix/djishnu/Javad/SLIDEbench_day5_spatial/x.csv",
                row.names = 1,
                sep=",",
                header = T)

y <- read.table("/ix/djishnu/Javad/SLIDEbench_day5_spatial/y.csv",
                row.names = 1,
                sep = ",",
                header=T)

colnames(y) <-"y"


data_norm<- scale(x,T,T)



# encoder <- keras_model_sequential() %>%
#   layer_dense(units = 128, activation = "relu", input_shape = c(input_dim), kernel_initializer = "glorot_uniform") %>%
#   layer_dense(units = latent_dim*2)

encoder <- keras_model_sequential() %>%
  layer_dense(units = latent_dim*2, activation = "sigmoid", input_shape = c(input_dim), kernel_initializer = "glorot_uniform")
#layer_dense(units = latent_dim*2)



decoder <- keras_model_sequential() %>%
  layer_dense(units = c(latent_dim), activation = "relu", input_shape = c(latent_dim)) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = input_dim, activation = "sigmoid")
## no sampling
vae <- keras_model(inputs = encoder$input, outputs = decoder(encoder$output[, 1:latent_dim]))
# sampling




vae %>% compile(optimizer = "adam", loss = "CosineSimilarity")

# Train the VAE

vae_fit <- vae %>% fit(data_norm, data_norm, epochs = 100, batch_size = 24, validation_split = 0.2)
encoder_output <- stats::predict(encoder, data_norm)

sumVAE <- summary(lm(as.matrix(y)~encoder_output[,1:latent_dim]))
vAERsqaured <- sumVAE$r.squared

loading <- encoder %>% get_weights()
vaeoadings <- encoder %>% get_weights()
vaeoadings <- vaeoadings[[1]][,1:latent_dim]

row.names(vaeoadings) <- colnames(x)
colnames(vaeoadings) <- paste0("Z",1:latent_dim)






Z_ER <- read.table("/ix/djishnu/Javad/SLIDEbench_day5_spatial/z_matrix.csv",header = T,
                   row.names = 1,sep = ",")
sigK  <- c(46,89,96,117,138)
sigIn <- c("Z46.Z2","Z89.Z58","Z96.Z130","Z117.Z7","Z117.Z113","Z117.Z115","Z117.Z138","Z138.Z30","Z138.Z115","Z138.Z117")


IntData        <- interUnion(sigK,Z_ER)
Dataint        <- IntData$interaction[,sigIn]
Data_real      <- data.frame(y=as.matrix(y),Z_ER[,sigK],Dataint)
SumReal            <- summary(lm(y~.,data = Data_real))
SumReal$r.squared
saveRDS(vaeoadings,"/ix/djishnu/Javad/SLIDEbench_day5_spatial/MOFA_VAE/VAE.rds")





################################################################################
#Mofa
################################################################################

library(MOFA2)
data_norm <- list()
data_norm$data$view_1 <-  t(scale(x,T,T))
MOFAmodel <- create_mofa(data_norm$data)
MOFAmodel <- prepare_mofa(MOFAmodel)
MOFAmodel <- run_mofa(MOFAmodel, use_basilisk = TRUE)
#CD4_MOFA <- load_model(file = "CD4-Mofa.hdf5",remove_inactive_factors = FALSE)
factors <- get_factors(MOFAmodel, factors = "all")
factors <- factors$group1
MofaLoading <- MOFA2::get_weights(MOFAmodel)$view_1

saveRDS(factors,"/ix/djishnu/Javad/SLIDEbench_day5_spatial/MOFA_VAE/MOFA_facros_v2.rds")
saveRDS(MofaLoading,"/ix/djishnu/Javad/SLIDEbench_day5_spatial/MOFA_VAE/MOFA_loading_v2.rds")


SumMofa<- summary(lm(as.matrix(y)~factors$group1[,sample(size=4,1:20)]))






## ggplot2
#topvsSLIDE <- data.frame(Method=c('SLIDE','VAE','MOFA+'),values=c(sqrt(SumReal$r.squared),sqrt(vAERsqaured),sqrt(SumMofa$r.squared)))



P <- ggplot(topvsSLIDE, aes(x=Method, y=values, fill=Method)) +
  geom_col(width = 0.25, color="black") +
  coord_cartesian(ylim=c(0.1,0.7)) +
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