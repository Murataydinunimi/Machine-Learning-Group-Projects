library(ggplot2)
library(naniar)
library("factoextra")
library("FactoMineR")
library("corrplot")
library("formattable")
library(gridExtra)

list.files()


setwd("C:/Users/barla/OneDrive/Desktop/Academia/Data Science and Economics/Statistical Learning/Group Project/PCA and Clustering")
data_f<-read.csv("heritage.csv")
View(data_f)
nrow(data_f)
#Convert to the numeric
clean_datas<-data_f[,-c(1,2,3)]
clean_datas<-as.data.frame(apply(clean_datas,2,as.numeric))
row.names(clean_datas)<-data_f[,1]



sum(is.na(clean_datas)) #20 NA


vis_miss(clean_data)# distribution of missing values.Only 1.2 percent of the data is missing.
gg_miss_upset(clean_data)

#only 5 variables have missing values. Investment freedom, government spending, trade freedom, tax burden, financial freedom.
#there is just one case where all of them are missing
#one case where trade, financial fredom and tax burden missing.
#one case where gov.spending, financial freedom, tax burden missing.
#one case where only financial missing.
#one case where tax burden and gov.spending missing.


which(is.na(clean_data), arr.ind=TRUE) # only 5 countries have missing values.

clean_data<-na.omit(clean_data)


m<-cor(clean_data)
corrplot(m, method="number")
m

#PCA

res.pca <-PCA(clean_data,graph=FALSE)

eig.val<-get_eigenvalue(res.pca)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
row.names(eig.val)<-NULL
dims<-matrix(c("Dim.1","Dim.2","Dim.3","Dim.4","Dim.5","Dim.6","Dim.7","Dim.8","Dim.9","Dim.10","Dim.11","Dim.12"))
eig.val<-cbind(dims,eig.val)
colnames(eig.val)[1]<-"Dimensions"
row.names(eig.val)<-NULL
grid.table(eig.val)




plot(eig.val[,1],type="b",main="Scree Diagram",xlab="Number of Component",ylab="Eigenvalues")
abline(h=1,lwd=3,col="red")


plot(eig.val[,2],type="b",main="Scree Diagram",xlab="Number of Component",ylab="% variance explained")

plot(eig.val[,3],type="b",main="Scree Diagram",xlab="Number of Component",ylab="% Cumulative variance")



var <- get_pca_var(res.pca) #This includes scores etc.

var

#loadings
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)+ggtitle(paste0("PCA - Loadings"))




vars<-row.names(var$coord)
corrmat<-as.data.frame(apply(corrmat[,2:6],2,as.numeric))
communality=corrmat[,1]^2+corrmat[,2]^2+corrmat[,3]^2+corrmat[,4]^2+corrmat[,5]^2


corrmat<-cbind(vars,var$coord)
corrmat<-cbind(corrmat,communality)
row.names(corrmat)<-NULL
colnames(corrmat)[1]<-"Indicators"

corrplot(var$cos2, is.corr=FALSE)

grid.table(corrmat)


dev.off()

#quality of representation of the features, similar to correlation matrix
fviz_cos2(res.pca, choice = "var", axes = 1, top=10)

fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


#SCORES

ind <- get_pca_ind(res.pca)
ind$coord # scores of countries
ind$cos2#
ind$contrib #contribution of countries


fviz_cos2(res.pca, choice = "ind")




fviz_contrib(res.pca, choice = "ind", axes = 1:2)
fviz_contrib(res.pca, choice = "ind", axes = 2)

##countries projected
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

png("image.png", width = 800, height = 600)
dev.off()




