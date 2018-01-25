install.packages("Rfacebook")
install.packages("httpuv")
library(Rfacebook)
library(httpuv)
#fbOAuth(app_id, app_secret, extended_permissions = TRUE)
fb_oauth=fbOAuth("105047220024243", "3c0ad28cc7ed6312d27d70b01b8aec50", extended_permissions = TRUE)
#fb_oauth <- fbOAuth(app_id="123456789", app_secret="1A2B3C4D")
save(fb_oauth, file="fb_oauth")
getwd()
dir()

library(Rfacebook)
library(httpuv)
library(RColorBrewer)

load("fb_oauth")
me <- getUsers("me", token=fb_oauth)
#getFQL(query, token)
my_friends <- getFriends(token=fb_oauth, simplify=F)
str(my_friends)
table(my_friends$relationship_status)

pie(table(my_friends$relationship_status),col=brewer.pal(5, "Set1"))

table(my_friends$location)
pie(table(my_friends$location),col=brewer.pal(20, "Greens"))
pie(table(my_friends$locale),col=brewer.pal(4, "Blues"))
pie(table(my_friends$gender),col=brewer.pal(3, "Oranges"))

load("fb_oauth")

mat <- getNetwork(token=fb_oauth, format="adj.matrix")

library(igraph)

network <- graph.adjacency(mat, mode="undirected")
getwd()
setwd("D:/R pract/FacebookCreateAPP")

pdf("network_plot.pdf")

plot(network ,vertex.size=5, 
     vertex.label=NA, 
     vertex.label.cex=0.45,
     edge.arrow.size=1,
     edge.curved=TRUE,)

dev.off()