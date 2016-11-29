library("igraph")
library("base")

###################################################################
#  Código de análisis del comportamiento del modelo Erdos-Renyi.  #
###################################################################

# Inicialmente realizamos un modelo de la internet
# Creamos un grafo generado a partir del modelo Erdos-Renyi
# @ Parámetros: n - vértices/nodos ; m - aristas
# @ Brief: Grafo dirigido generado con el modelo Erdos-Renyi

dev.new()
netgraph<-erdos.renyi.game(n=50,p.or.m=104,type="gnm",directed=TRUE)
E(netgraph)$color<-"gray"
V(netgraph)$color<-"darkgray"
V(netgraph)$label.color<-"white"
plot.igraph(netgraph, layout=layout.fruchterman.reingold,vertex.size=10,
		vertex.label.cex=.5, edge.arrow.size=.5, main="Modelo de Endos-Renyi", 
		xlab="Modelo de internet realizado por grafos aleatorios")

# V es una función de igraph que nos permite contabilizar los vértices/nodos
# @ Ver: http://igraph.org/r/doc/V.html
# E es una función de igraph que nos permite contabilizar las aristas
# @ Ver: http://igraph.org/r/doc/E.html
vertices <- V(netgraph)
aristas <- E(netgraph)

# Obtenemos la distribución de grado del grafo definido
# @ Ver: "Las redes complejas como grafos Erdös–Rényi", página 4, ecuación 4.
dev.new()
dis_grad <- degree.distribution(netgraph,cumulative= TRUE,mode= "all")
plot(dis_grad, log="xy", bg= "black", pch=21, xlab="Grado de los vértices", 
	ylab="Frecuencia acumulada",main= "Distribución de grado")

# Graficamos la distancia más larga de un vértice a otro en todo el grafo
# @ Brief: Graficamos el recorrido más largo, es decir el vértice con mayor (...)
# (...) distribución de grado, ver recorrido en color Cyan.
dev.new()
diameter(netgraph)
largest_path<-get.diameter(netgraph)
E(netgraph)$color<-"gray"
E(netgraph,path=largest_path)$color<-"cyan3"
E(netgraph,path=largest_path)$width<-2
V(netgraph)$color<-"darkgray"
V(netgraph)$size<-10
V(netgraph)[largest_path]$color<-"cyan3"
V(netgraph)[largest_path]$size<-12
V(netgraph)[largest_path]$label.color<-"white"

plot.igraph(netgraph,layout=layout.fruchterman.reingold,vertex.label.cex=.5,
		edge.arrow.size=.5, main="Mayor grado de distribución")

# Comparación de modelos Erdos-Renyi vs Barabási
# @ Brief: Creamos dos grafos con la misma cantidad de vértices con ambos (...)
# modelos anteriormente especificados.
dev.new()
par(mfrow=c(1,2),mar=c(.2,.2,.2,.2))
smallworldgraph<-barabasi.game(50,power=1)
bigworldgraph<-erdos.renyi.game(n=20,p.or.m=0.09,type="gnp",directed=T)
E(smallworldgraph)$color<-"aquamarine"
V(smallworldgraph)$color<-"aquamarine2"
V(smallworldgraph)$label.color<-"white"
E(bigworldgraph)$color<-"coral"
V(bigworldgraph)$color<-"coral1"
V(bigworldgraph)$label.color<-"white"
plot(bigworldgraph, layout=layout.fruchterman.reingold,vertex.label.cex=.5,
		edge.arrow.size=.5, main="Modelo de Endos-Renyi")
plot(smallworldgraph,layout= layout.fruchterman.reingold, vertex.label.cex=.5,
		edge.arrow.size=.5, main= "Modelo de Barabási")

# Otros modelos de grafos aleatorios ampliamente utilizados
# @ Brief: Creamos otros grafos generados a través de otros modelos
# Modelos de grafos: Grafo en anillo, grafo de Watts-Strogatz, grafo de Bipartite, (...)
# (...) grafo geométrico.
dev.new()
par(mfrow=c(2,2))
anillograph <- graph.ring(200)
plot(anillograph, layout=layout.fruchterman.reingold, vertex.label= NA, 
	edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Red anillo")
watts_strogatzgraph <- watts.strogatz.game(1, 100, 5, 0.05)
plot(watts_strogatzgraph, layout=layout.fruchterman.reingold, vertex.label= NA, 
	edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Grafo Watts-Strogatz")
bipartitegraph <- sample_bipartite(25,10, p=0.2)
plot(bipartitegraph, layout=layout.fruchterman.reingold, vertex.label= NA, 
	edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Grafo Bipartite")
samplegraph <- sample_grg(100, 0.1, torus=FALSE)
plot(samplegraph, layout=layout.fruchterman.reingold, vertex.label= NA, 
	edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Grafo geométrico")

# Grafo aleatorio generado con Erdos-Renyi simulando la internet
# @ Brief: Aplicamos el modelo Erdos-Renyi en la simulación
# @ Parámetros: n - vértices/nodos ; m - aristas
dev.new()
internetgraph<-erdos.renyi.game(n=200,p.or.m=170,type="gnm",directed=TRUE)
E(internetgraph)$color<-"black"
V(internetgraph)$color<-"darkgray"
V(internetgraph)$label.color<-"white"
plot.igraph(internetgraph, layout=layout.fruchterman.reingold,vertex.size=4,
		vertex.label.cex=.5, edge.arrow.size=.3, main="Simulación de internet", 
		xlab="Modelo de internet realizado por grafos aleatorios")

# Función de distribución de probabilidad
z <- 2*170/200
k <- sum(degree.distribution(internetgraph,cumulative= TRUE,mode= "all"))
PDF <- (2.7182^(-z))*(z^k)/factorial(k)
cat("La función de distribución de probabilidad: ",PDF)

# Esperanza del grafo internet
esperanza_calculada <- 0.35+2.06*log(200)
cat("La esperanza calculada es: ",esperanza_calculada)

# Confirmamos la veracidad de este valor obteniendo el diámetro del grafo
esperanza_real <- diameter(internetgraph)
cat("La esperanza real es: ",esperanza_real)

# El camino de mayor de grado en el grafo
path <- get.diameter(internetgraph)
cat("El camino de mayor grado: ",path)

