#http://kateto.net/network-visualization

install.packages("igraph") 
install.packages("network") 
install.packages("sna")
install.packages("visNetwork")
install.packages("threejs")
install.packages("networkD3")
install.packages("ndtv")


# pch: Type of object
# cex is size of object
plot(x=1:10, y=rep(5,10), pch=2, cex=1, col="dark red")

# color could be number as well
points(x=1:10, y=rep(6, 10), pch=19, cex=3, col="557799")

# color can be presented as rgb also
points(x=1:10, y=rep(4, 10), pch=19, cex=3, col=rgb(.25, .5, .3))

# alpha is the tansparency which should be between [0,1]
plot(x=1:5, y=rep(5,5), pch=19, cex=12, col=rgb(.25, .5, .3, alpha=0.5), xlim=c(0,6)) 

# change background color
par(bg="white")

# change color of diagram
col.tr <- grDevices::adjustcolor("557799", alpha=0.7)

# plot
plot(x=1:5, y=rep(5,5), pch=19, cex=12, col=col.tr, xlim=c(0,6)) 

# to show the list of colors
colors()                          # List all named colors

# search for different shades of a color
grep("gray", colors(), value=T)   # Colors that have "blue" in the name

# well, R has more than 50 shades of Gray!!!

pal1 <- heat.colors(5, alpha=1)   #  5 colors from the heat palette, opaque
pal2 <- rainbow(5, alpha=.5)      #  5 colors from the heat palette, transparent
plot(x=1:10, y=1:10, pch=19, cex=5, col=pal1)


plot(x=1:10, y=1:10, pch=19, cex=5, col=pal2)

palf <- colorRampPalette(c("gray80", "dark red")) 
plot(x=10:1, y=1:10, pch=19, cex=5, col=palf(10))


palf <- colorRampPalette(c(rgb(1,1,1, .2),rgb(.8,0,0, .7)), alpha=TRUE)
plot(x=10:1, y=1:10, pch=19, cex=5, col=palf(10)) 


install.packages('RColorBrewer')
library('RColorBrewer')
display.brewer.all()


display.brewer.pal(8, "Set3")

display.brewer.pal(8, "Spectral")

display.brewer.pal(8, "Blues")

pal3 <- brewer.pal(10, "Set3")
plot(x=10:1, y=10:1, pch=19, cex=6, col=pal3)


plot(x=10:1, y=10:1, pch=19, cex=6, col=rev(pal3)) # backwards

# load additional fonts
install.packages('extrafont')
library('extrafont')

# Import system fonts - may take a while, so DO NOT run this during the workshop.
font_import() 
fonts() # See what font families are available to you now.
loadfonts(device = "win") # use device = "pdf" for pdf plot output.

# plot with a different font
plot(x=10:1, y=10:1, pch=19, cex=3, 
     main="This is a plot", col="orange", 
     family="Sylfaen" )


# Following code did not work
# First you may have to let R know where to find ghostscript on your machine:
# Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.21/bin/gswin64c.exe")

# pdf() will send all the plots we output before dev.off() to a pdf file: 
# pdf(file="ArialBlack.pdf")
# plot(x=10:1, y=10:1, pch=19, cex=6, 
#     main="This is a plot", col="orange", 
#     family="Arial Black" )
# dev.off()

# embed_fonts("ArialBlack.pdf", outfile="ArialBlack_embed.pdf")


nodes <- read.csv("C:/Users/vivek4/Downloads/Data files/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("C:/Users/vivek4/Downloads/Data files/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

# check data
head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

# aggreagte the edges
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL


# This graph is a bipartite graph
nodes2 <- read.csv("C:/Users/vivek4/Downloads/Data files/Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("C:/Users/vivek4/Downloads/Data files/Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)

head(nodes2)
head(links2)

links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)

library('igraph')
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net


E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"

# Find nodes and edges by attribute:
# (that returns oblects of type vertex sequence/edge sequence)
V(net)[media=="BBC"]
E(net)[type=="mention"]

# You can also examine the network matrix directly:
net[1,]
net[5,7]


# Get an edge list or a matrix:
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight")

# Or data frames describing nodes and edges:
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")

plot(net)

net <- simplify(net, remove.multiple = F, remove.loops = T) 

plot(net, edge.arrow.size=.4,vertex.label=NA)

# lets try other dataset

head(nodes2)
head(links2)

net2 <- graph_from_incidence_matrix(links2)
table(V(net2)$type)


# its important to reduce the size of edge arrow
plot(net, edge.arrow.size=.4, edge.curved=.1)

plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black") 



# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net) 


plot(net, edge.color="orange", vertex.color="gray50") 


plot(net) 
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")


edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1)  



# network layouts
net.bg <- sample_pa(80) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)

plot(net.bg, layout=layout_randomly)

l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)

l <- cbind(1:vcount(net.bg), c(1, vcount(net.bg):2))
plot(net.bg, layout=l)

# Randomly placed vertices
l <- layout_randomly(net.bg)
plot(net.bg, layout=l)

# Circle layout
l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)

# 3D sphere layout
l <- layout_on_sphere(net.bg)
plot(net.bg, layout=l)

# this is nice layout
l <- layout_with_fr(net.bg)
plot(net.bg, layout=l)

# four different layouts
par(mfrow=c(2,2), mar=c(0,0,0,0))   # plot four figures - 2 rows, 2 columns
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=l)
plot(net.bg, layout=l)

# reset the layout
dev.off()

plot.new()

l <- layout_with_fr(net.bg)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

par(mfrow=c(2,2), mar=c(0,0,0,0))
plot(net.bg, rescale=F, layout=l*0.4)
plot(net.bg, rescale=F, layout=l*0.6)
plot(net.bg, rescale=F, layout=l*0.8)
plot(net.bg, rescale=F, layout=l*1.0)

