## Analysis of voting patterns for 2016 Eurovision song contest

library(clusterfly)
library(igraph)
library(magrittr)

## load the voting data
votedata <- read.csv("eurovision-votes_2016.csv")
## remove total votes column
votedata$total_votes <- NULL
## set row names to contestant countries, and remove that column
rownames(votedata) <- votedata$contestant
votedata$contestant <- NULL
## fix up rownames to match column names
rownames(votedata)[25] <- "Czech.Republic"
rownames(votedata)[24] <- "United.Kingdom"
## check
rownames(votedata) %in% colnames(votedata)
## set missing values to 0
votedata[is.na(votedata)] <- 0
## convert to matrix
votedata <- as.matrix(votedata)
## create new rows for countries with no votes
newcountries <- setdiff(colnames(votedata),rownames(votedata))
newnrow <- length(newcountries)
newrows <- matrix(rep(0,newnrow*42),nrow=newnrow,ncol=42)
votedata <- rbind(votedata,newrows)
rownames(votedata)[27:42] <- newcountries
## sort rows and columns
votedata <- votedata[order(rownames(votedata)),]
votedata <- votedata[,order(colnames(votedata))]
## create directed and undirected graphs
votesgraphd <- graph_from_adjacency_matrix(votedata,weighted=TRUE,mode="directed")
votesgraphu <- graph_from_adjacency_matrix(votedata,weighted=TRUE,mode="undirected")
## find clusters within graph
clusters <- cluster_optimal(votesgraphd)
##votesgraph <- graph_from_adjacency_matrix(votedata,weighted=TRUE)

jpeg("Eurovision_votes_2016.jpg",width=739,height=715)
plot(clusters,votesgraphd)
dev.off()

png("Eurovision_votes_2016.png",width=739,height=715)
plot(clusters,votesgraphd)
dev.off()

