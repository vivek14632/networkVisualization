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
# dev.off()

plot.new()

l <- layout_with_fr(net.bg)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

par(mfrow=c(2,2), mar=c(0,0,0,0))
plot(net.bg, rescale=F, layout=l*0.4)
plot(net.bg, rescale=F, layout=l*0.6)
plot(net.bg, rescale=F, layout=l*0.8)
plot(net.bg, rescale=F, layout=l*1.0)


l <- layout_with_kk(net.bg)
plot(net.bg, layout=l)

plot(net.bg, layout=layout_with_lgl)

plot(net.bg, layout=layout_with_mds)


layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(3,3), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }


# highlight
hist(links$weight)
mean(links$weight)
sd(links$weight)

cut.off <- mean(links$weight) 
net.sp <- delete_edges(net, E(net)[weight<cut.off])
plot(net.sp) 

par(mfrow=c(1,2))

# Community detection based on label propagation:
clp <- cluster_label_prop(net)
class(clp)

# Community detection returns an object of class "communities" 
# which igraph knows how to plot: 
plot(clp, net)

# We can also plot the communities without relying on their built-in plot:
V(net)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])


dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], 
                           to=V(net), weights=NA)

# Set colors to plot the distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]

plot(net, vertex.color=col, vertex.label=dist.from.NYT, edge.arrow.size=.6, 
     vertex.label.color="white")

##########################################################################
################### Highlight path in network#############################
##########################################################################
news.path <- shortest_paths(net, 
                            from = V(net)[media=="MSNBC"], 
                            to  = V(net)[media=="New York Post"],
                            output = "both") # both path nodes and edges

# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(net))
ecol[unlist(news.path$epath)] <- "orange"
# Generate edge width variable to plot the path:
ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4
# Generate node color variable to plot the path:
vcol <- rep("gray40", vcount(net))
vcol[unlist(news.path$vpath)] <- "gold"

plot(net, vertex.color=vcol, edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0)


#######################Highlight edges##################################
inc.edges <- incident(net,  V(net)[media=="Wall Street Journal"], mode="all")

# Set colors to plot the selected edges.
ecol <- rep("gray80", ecount(net))
ecol[inc.edges] <- "orange"
vcol <- rep("grey40", vcount(net))
vcol[V(net)$media=="Wall Street Journal"] <- "gold"
plot(net, vertex.color=vcol, edge.color=ecol)


################Highlight immediate neighbours#############################
neigh.nodes <- neighbors(net, V(net)[media=="Wall Street Journal"], mode="out")

# Set colors to plot the neighbors:
vcol[neigh.nodes] <- "#ff9d00"
plot(net, vertex.color=vcol)


#############Draw attention to a group of nodes############################
par(mfrow=c(1,2))
plot(net, mark.groups=c(1,4,5,8), mark.col="#C5E5E7", mark.border=NA)

# Mark multiple groups:
plot(net, mark.groups=list(c(1,4,5,8), c(15:17)), 
     mark.col=c("#C5E5E7","#ECD89A"), mark.border=NA)



##########################################################################
###################Interactive plotting###################################
##########################################################################
tkid <- tkplot(net) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
plot(net, layout=l)



#################Others ways to present network#########################
netm <- as_adjacency_matrix(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )


# Plot the egree distribution for our network:
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

############two mode network############################
head(nodes2)
head(links2)

net2
plot(net2, vertex.label=NA)


# Media outlets are blue squares, audience nodes are orange circles:
V(net2)$color <- c("steel blue", "orange")[V(net2)$type+1]
V(net2)$shape <- c("square", "circle")[V(net2)$type+1]

# Media outlets will have name labels, audience members will not:
V(net2)$label <- ""
V(net2)$label[V(net2)$type==F] <- nodes2$media[V(net2)$type==F] 
V(net2)$label.cex=.6
V(net2)$label.font=2

plot(net2, vertex.label.color="white", vertex.size=(2-V(net2)$type)*8) 


plot(net2, vertex.label=NA, vertex.size=7, layout=layout.bipartite) 


######using text as node######################

plot(net2, vertex.shape="none", vertex.label=nodes2$media,
     vertex.label.color=V(net2)$color, vertex.label.font=2, 
     vertex.label.cex=.6, edge.color="gray70",  edge.width=2)


########################Using external images##################

install.packages('png')
library('png')

img.1 <- readPNG("C:/Users/vivek4/Downloads/Data files/images/news.png")
img.2 <- readPNG("C:/Users/vivek4/Downloads/Data files/images/user.png")

V(net2)$raster <- list(img.1, img.2)[V(net2)$type+1]

plot(net2, vertex.shape="raster", vertex.label=NA,
     vertex.size=16, vertex.size2=16, edge.width=2)

### adding a random image##################
plot(net2, vertex.shape="raster", vertex.label=NA,
     vertex.size=16, vertex.size2=16, edge.width=2)

img.3 <- readPNG("C:/Users/vivek4/Downloads/Data files/images/puppy.png")
rasterImage(img.3,  xleft=-1.7, xright=0, ybottom=-1.2, ytop=0)

# bipartite projection
par(mfrow=c(1,2))

net2.bp <- bipartite.projection(net2) 

plot(net2.bp$proj1, vertex.label.color="black", vertex.label.dist=1,
     vertex.label=nodes2$media[!is.na(nodes2$media.type)])

plot(net2.bp$proj2, vertex.label.color="black", vertex.label.dist=1,
     vertex.label=nodes2$media[ is.na(nodes2$media.type)]) 

# plotting different networks
E(net)$width <- 1.5
plot(net, edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1],
     vertex.color="gray40", layout=layout_in_circle, edge.curved=.3)

net.m <- net - E(net)[E(net)$type=="hyperlink"] # another way to delete edges:
net.h <- net - E(net)[E(net)$type=="mention"]   # using the minus operator

# Plot the two links separately:
par(mfrow=c(1,2))
plot(net.h, vertex.color="orange", main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", main="Tie: Mention")

l <- layout_with_fr(net)
plot(net.h, vertex.color="orange", layout=l, main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", layout=l, main="Tie: Mention")



multigtr <- graph( edges=c(1,2, 1,2, 1,2), n=2 )
l <- layout_with_kk(multigtr)

# Let's just plot the graph:
plot(multigtr, vertex.color="lightsteelblue", vertex.frame.color="white",
     vertex.size=40, vertex.shape="circle", vertex.label=NA,
     edge.color=c("gold", "tomato", "yellowgreen"), edge.width=5,
     edge.arrow.size=3, edge.curved=0.1, layout=l)


plot(multigtr, vertex.color="lightsteelblue", vertex.frame.color="white", 
     vertex.size=40, vertex.shape="circle", vertex.label=NA,
     edge.color=c("gold", "tomato", "yellowgreen"), edge.width=5,
     edge.arrow.size=3, edge.curved=curve_multiple(multigtr), layout=l)

detach('package:igraph')


############################network package#############################
library('network')

net3 <- network(links,  vertex.attr=nodes, matrix.type="edgelist", 
                loops=F, multiple=F, ignore.eval = F)
net3[,]
net3 %n% "net.name" <- "Media Network" #  network attribute
net3 %v% "media"    # Node attribute
net3 %e% "type"     # Node attribute

net3 %v% "col" <- c("gray70", "tomato", "gold")[net3 %v% "media.type"]
plot(net3, vertex.cex=(net3 %v% "audience.size")/7, vertex.col="col")
l <- plot(net3, vertex.cex=(net3 %v% "audience.size")/7, vertex.col="col")
plot(net3, vertex.cex=(net3 %v% "audience.size")/7, vertex.col="col", coord=l)


detach('package:network')
plot(net3, vertex.cex=(net3 %v% "audience.size")/7, vertex.col="col", interactive=T)



################interactive visualization############################
# library('animation') 
# library('igraph')
# 
# ani.options("convert") # Check that the package knows where to find ImageMagick
# # If it doesn't know where to find it, give it the correct path for your system.
# ani.options(convert="C:/Program Files/ImageMagick-6.8.8-Q16/convert.exe") 
# 
# 
# l <- layout.fruchterman.reingold(net)
# 
# saveGIF( {  col <- rep("grey40", vcount(net))
# plot(net, vertex.color=col, layout=l)
# 
# step.1 <- V(net)[media=="Wall Street Journal"]
# col[step.1] <- "#ff5100"
# plot(net, vertex.color=col, layout=l)
# 
# step.2 <- unlist(neighborhood(net, 1, step.1, mode="out"))
# col[setdiff(step.2, step.1)] <- "#ff9d00"
# plot(net, vertex.color=col, layout=l) 
# 
# step.3 <- unlist(neighborhood(net, 2, step.1, mode="out"))
# col[setdiff(step.3, step.2)] <- "#FFDD1F"
# plot(net, vertex.color=col, layout=l)  },
# interval = .8, movie.name="network_animation.gif" )

################This visulization is good, we can magnify it#############

library('visNetwork') 
visNetwork(nodes, links, width="100%", height="400px", main="Network!")


?visNodes
?visEdges

nodes$shape <- "dot"  
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$title <- nodes$media # Text on click
nodes$label <- nodes$type.label # Node label
nodes$size <- nodes$audience.size # Node size
nodes$borderWidth <- 2 # Node border width

nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"

visNetwork(nodes, links)



links$width <- 1+links$weight/8 # line width
links$color <- "gray"    # line color  
links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
links$smooth <- FALSE    # should the edges be curved?
links$shadow <- FALSE    # edge shadow

visNetwork(nodes, links)


visNetwork(nodes, links) %>%
  visOptions(highlightNearest = TRUE, 
             selectedBy = "type.label")