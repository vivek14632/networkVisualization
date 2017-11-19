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
     family="Arial Black" )


