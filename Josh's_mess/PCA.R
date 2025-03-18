install.packages("corrr")
library('corrr')
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factoextra")
library("factoextra")
data <- read.csv("~/ukase_data_challenge/data/zoonosis_dataset_full.csv")
str(data)
colSums(is.na(numerical_data))

# Keep only numeric columns
numerical_data <- data[,5:964]

# Normalise data
data_normalized <- scale(numerical_data)

# Drop rows/columns with nans
data_normalized <- data_normalized[rowSums(!is.nan(data_normalized))>0,colSums(!is.nan(data_normalized))>0]
head(data_normalized)

# Carry out the PCA and view results
data.pca <- princomp(data_normalized)
summary(data.pca)
which.max(loadings(data.pca)[,2])

# Show pairs plots
data_proj <- predict(data.pca)
pairs(data_proj, verInd = 1:15, horInd = 1:15)

# Plot levels of variance explained
fviz_eig(data.pca, ncp = 20, addlabels = TRUE)


# Coloured pairs plots
panel.points<-function(x,y)
{
  points(x,y,cex=2)
}
data.class <- factor(paste(data$subtype,sep=""))
pairs(data_proj,col=unclass(data.class), verInd = 1:5, horInd = 1:5, cex = 0.1)

# Find most important variables 
lst <- sort(loadings(data.pca)[,1], index.return=TRUE, decreasing=TRUE)
lapply(lst, `[`, lst$x %in% head(unique(lst$x),15))

# Check significance of top 15 PCs
df <- as.data.frame(data_proj)
df2 <- as.data.frame(data$label)
df2[df2=='hzoon'] <- 1
df2[df2=='nz'] <- 0
model <- glm(as.numeric(df2$`data$label`) ~ df$Comp.1 + df$Comp.2 + df$Comp.3 + df$Comp.4 + df$Comp.5 + df$Comp.6 + df$Comp.7 + df$Comp.8 + df$Comp.9 + df$Comp.10 + df$Comp.11 + df$Comp.12 + df$Comp.13 + df$Comp.14 + df$Comp.15, family = 'poisson')
model1 <- glm(as.numeric(df2$`data$label`) ~ df$Comp.1 + df$Comp.2 + df$Comp.3 + df$Comp.4 + df$Comp.5 + df$Comp.6 + df$Comp.7 + df$Comp.11 + df$Comp.13 + df$Comp.14 + df$Comp.15, family = 'poisson')

# Check if there's a significant difference between models including and non-including significant PCs
anova(model, model1)
summary(model)

x <- select(df, Comp.1, Comp.2, Comp.3, Comp.4, Comp.5, Comp.6, Comp.7, Comp.11, Comp.13, Comp.14, Comp.15)

write.csv(x,"~/ukase_data_challenge/cleaned_data/x_significant_top_pcas.csv", row.names = TRUE)

write.csv(df2,"~/ukase_data_challenge/cleaned_data/y_significant_top_pcas.csv", row.names = TRUE)


