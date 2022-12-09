# Bibliometric analysis using the bibliometrix R package
# https://bibliometrix.org/documents/bibliometrix_Report.html

## To cite bibliometrix in publications, please use:
## 
## Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, 
##                                  Journal of Informetrics, 11(4), pp 959-975, Elsevier.
##                         
## 
## https://www.bibliometrix.org
## 
##                         
## For information and bug reports:
##                         - Send an email to info@bibliometrix.org   
##                         - Write a post on https://github.com/massimoaria/bibliometrix/issues
##                         
## Help us to keep Bibliometrix free to download and use by contributing with a small donation to support our research team (https://bibliometrix.org/donate.html)


# Install the bibliometrix package
#install.packages("bibliometrix", dependencies = TRUE)


# Data loading and converting
library(bibliometrix)
myfile <- "20221111-analysis-corpus-web-of-science-export.txt"

# Converting the loaded files into a R bibliographic dataframe
M <- convert2df(file=myfile, dbsource="wos",format="plaintext")


# Section 1: Main Findings
#options(width=160)
results <- biblioAnalysis(M)
summary(results, k=10, pause=F, width=130)

# Visualize main findings
plot(x=results, k=10, pause=F)

# Most cited references
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:20])



# Section 2: Intellectual structure of the field

# Co-citation analysis of references
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)

netstat <- networkStat(NetMatrix)
summary(netstat,k=10)

# Co-citation analysis of sources
M=metaTagExtraction(M,"CR_SO",sep=";")
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)

netstat <- networkStat(NetMatrix)
summary(netstat,k=10)


# Section 3: Historiograph - Direct citation linkages
histResults <- histNetwork(M, sep = ";")

options(width = 130)
net <- histPlot(histResults, n=20, size = 5, labelsize = 4)


# Section 4: The conceptual structure - Co-Word Analysis
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", n = 50, Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,label.n=30,edges.min=2)

netstat <- networkStat(NetMatrix)
summary(netstat,k=10)

# Co-word Analysis through Correspondence Analysis
suppressWarnings(
  CS <- conceptualStructure(M, method="MCA", field="ID", minDegree=5, clust=5, stemming=FALSE, labelsize=15,documents=20)
)


# Section 5: Thematic Map
Map=thematicMap(M, field = "ID", n = 250, minfreq = 4,
                stemming = FALSE, size = 0.7, n.labels=5, repel = TRUE)
plot(Map$map)

# Cluster description
Clusters=Map$words[order(Map$words$Cluster,-Map$words$Occurrences),]
library(dplyr)
CL <- Clusters %>% group_by(.data$Cluster_Label) %>% top_n(5, .data$Occurrences)
CL


# Section 6: The social structure - Collaboration Analysis

# Author collaboration network
NetMatrix <- biblioNetwork(M, analysis = "collaboration",  network = "authors", sep = ";")
net=networkPlot(NetMatrix,  n = 50, Title = "Author collaboration",type = "auto", size=10,size.cex=T,edgesize = 3,labelsize=1)

netstat <- networkStat(NetMatrix)
summary(netstat,k=15)

# Institutional collaboration network
NetMatrix <- biblioNetwork(M, analysis = "collaboration",  network = "universities", sep = ";")
net=networkPlot(NetMatrix,  n = 50, Title = "Edu collaboration",type = "auto", size=4,size.cex=F,edgesize = 3,labelsize=1)

netstat <- networkStat(NetMatrix)
summary(netstat,k=15)

# Country collaboration network
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration",  network = "countries", sep = ";")
net=networkPlot(NetMatrix,  n = dim(NetMatrix)[1], Title = "Country collaboration",type = "circle", size=10,size.cex=T,edgesize = 1,labelsize=0.6, cluster="none")


