library(devtools)
library(gplots) 
library(superheat)
library(phangorn)
library(ape)
library(phylogram)
library(dendextend)

t <- upgma(distance, method = "average")
o <- upgma(o_distance, method = "average")

distance <- read.csv("C:/Users/ugur cabuk/Desktop/t_distance.csv", header=TRUE)
o_distance <- read.csv("C:/Users/ugur cabuk/Desktop/distance.csv", header=TRUE)

#transformed
distance$X
rownames(distance) <- distance$X
distance$X <- NULL
x <- as.matrix(distance)

#original
o_distance$X
rownames(o_distance) <- o_distance$X
o_distance$X <- NULL
o_x <- as.matrix(o_distance)


#transformed
t_hc <- as.hclust.phylo(t)
t_dend <- as.dendrogram(t_hc)

#original
o_hc <- as.hclust.phylo(o)
o_dend <- as.dendrogram(o_hc)


####comparison of the dendrograms###

dnd1 <- ladder(o_dend)
dnd2 <- ladder(t_dend)
dndlist <- dendextend::dendlist(dnd1, dnd2)
pdf("tree.png", width = 8, height = 8, units = 'in', res = 600)
par(cex=0.3, mar=c(5, 8, 4, 1))
dendextend::tanglegram(dndlist, fast = TRUE, margin_inner = 12, color_lines = c("red","red","red","red","black","black","black","black","black","black","black","black","black","red","red","red","red","red","red","red","red","red","red","red","black","black","black","black","black","black","black","black","red","red","red","red"
                                                                                ,"black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","red","red","red","black","black","black","black","black","black","black","black","black","black","black","black"),
                       edge.lwd = 2)

#length(unique(common_subtrees_clusters(o_dend, t_dend)))

#Size of the heatmap
lwid=c(1,3)
lhei=c(1,3)

#ordering for original and transformed matrix
x = x[order(rownames(x)),]

#converting into NA to 0 in diagonal in 2D matrix
diag(x)=NA

#color palette for heatmap
colfunc <- colorRampPalette(c("red","yellow", "green"))

#heatmap with dendro and without it
heatmap.2(x,trace = "none",scale="none",lwid = lwid,lhei = lhei,
          cexCol=0.48, cexRow = 0.48, Rowv = t_dend, Colv = t_dend, col = colfunc(40))


