df <- read.table("C:/Users/stat_pc/Desktop/midterm.txt", header = TRUE,  stringsAsFactors = F)

mydata <- df[9:30]
mydata$DEPENDENT  <- NULL
mydata$UGDS_WOMEN  <- NULL

### ex1
# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(mydata, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings 
plot(fit,type = "lines") # scree plot 
# Varimax Rotated Principal Components
# retaining 5 components 
library(psych)
fit_pca <- principal(mydata, nfactors = 3, rotate = "varimax")
factor_pca <- data.frame(fit_pca$loadings[,1:3]) # print results
# plot factor 1 by factor 2 
load_pca <- fit_pca$loadings[,1:2] 
plot(load_pca) # set up plot 
text(load_pca, labels = names(mydata), cex = .7) # add variable names

### ex2
library(ggmap)
map <- get_map(location = "America",
               zoom = 4, maptype = "roadmap", color = "bw")
location = data.frame(latitude = df$LATITUDE,
                      longitude = df$LONGITUDE,
                      fa1 = fit_pca$scores[,"RC1"],fa2 = fit_pca$scores[,"RC2"])
ggmap(map) + 
  geom_point(aes(x = longitude,y = latitude, color = fa1),
             size=2, data = location) + 
  scale_color_continuous("Factor 1")

### ex3
# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit_ml <- factanal(mydata, 4, rotation = "varimax")
factor_ml <- data.frame(fit_ml$loadings[,1:4]) # print results
print(fit_ml, digits = 2, cutoff = .3, sort = TRUE)
# plot factor 1 by factor 2 
load_ml <- fit_ml$loadings[,1:2] 
plot(load_ml,type = "n") # set up plot 
text(load_ml,labels = names(mydata),cex = .7) # add variable name
