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
#View(distance)
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

#help(hclust)

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
#help(tanglegram)
#length(unique(common_subtrees_clusters(o_dend, t_dend)))


#clust <- hclust(as.dist(distance), method = "average")

#rhcr <- hclust(dist(x))
#chrc <- hclust(dist(t(x)))

#coul <- colorRampPalette(brewer.pal(4, "YlOrRd"))(1000)

#display.brewer.all()

#colfunc <- colorRampPalette(c("red","yellow", "green","blue"))
#heatmap(x,Rowv = as.dendrogram(dend),
#       Colv = as.dendrogram(dend), col = c("white","red","green","dark green","black"))
#View(x)


#heatmap.2(x,Rowv = hc$order,
#          Colv = hc$order, col =colfunc(20), trace = "none",scale="none",
#          lwid = lwid,lhei = lhei, cexCol=0.48, cexRow = 0.48)

#png("high_res.png", width = 8, height = 8, units = 'in', res = 600)

#Size of the heatmap
lwid=c(1,3)
lhei=c(1,3)

#ordering whether original or transformed
x = x[order(rownames(x)),]
#converting into NA to 0 in diagonal in 2D
diag(x)=NA

#color palette
colfunc <- colorRampPalette(c("red","yellow", "green"))

#heatmap with dendro or without it
heatmap.2(x,trace = "none",scale="none",lwid = lwid,lhei = lhei,
          cexCol=0.48, cexRow = 0.48, Rowv = t_dend, Colv = t_dend, col = colfunc(40))


