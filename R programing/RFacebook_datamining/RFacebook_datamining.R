install.packages("Rfacebook")
install.packages("httpuv")
install.packages("RColorBrewer")
install.packages("RCurl")
install.packages("rjson")
install.packages("httr")

library(Rfacebook)
library(httpuv)
library(RColorBrewer)
library(RCurl)
library(rjson)
library(httr)

myaccess_token="EAACEdEose0cBACLPjy7lAqK17SF9eV7HY9FNNRAwiPMVxZAFGwo0OWFpaJFoHQRqtFkgw3YJEaE6iLxuVEb4cBojNMlpoCt5RG0RliWJOD6CGmlhjCgiSMCt1ezqYlaKiZAYTjRZBosKLs7DUvIqdKF67ItAcS52hLwqJEATTQ7aCF1MAbmYWugP02NVu4ZD"
options(RCurloptions=list(verbose=FALSE,capath=system.file("CurlssL","cacert.pem",package="RCurl"),ssl.verifypeer=FALSE))

me<-getUsers("me", token=myaccess_token)
me

my_friends<-getFriends(token=myaccess_token, simplify=F)
my_friends
str(my_friends)
table(my_friends)

pie(table(my_friends$relationship_status),col=brewer.pal(5,"Set1"))
pie(table(my_friends$location),col=brewer.pal(20,"Greens"))

textF ="ApplyingR-Datamining Facebook"
linkF=""
updatestatus(textF,token=myaccess_token,link=linkF)

install.packages("igraph")

temp < getNetwork(myaccess_token, format="adj.matrix")
library(igraph)
network<- graph.adjacency(temp,mode="undirected")

set.seed(1)

L<- layout.fruchterman.reingold(network)
L[,1]=(L[,1]-min(L[,1]))/(max(L[,1])-min(L[,1]))*2-1
L[,2]=(L[,2]-min(L[,2]))/(max(L[,2])-min(L[,2]))*2-1

pdf("net_work_plot.pdf", width=50,height=50)

plot(network,layout=L, vertex.size=0, vertex.frame.color="#0000000", edge.curved=FALSE,edge.color=rainbow(500), vertec.label.cex=3, edge.width=6)









