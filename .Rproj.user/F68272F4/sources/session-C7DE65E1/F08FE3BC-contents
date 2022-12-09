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
NetMatrix <- biblioNetwork(M, 
                           analysis = "co-citation", 
                           network = "references", 
                           sep = ";")
net=networkPlot(NetMatrix, 
                n = 105, 
                Title = "Co-Citation Network of Publications Based on Shared References", 
                type = "auto", 
                labelsize=0.65,
                label.cex = F,
                halo=T,
                community.repulsion = 0.01,
                size=5,
                size.cex=F, 
                remove.multiple=T, 
                remove.isolates=T,
                edgesize = 1, 
                edges.min=3,
                )

netstat <- networkStat(NetMatrix)
summary(netstat,k=10)


# Co-citation analysis of sources
M=metaTagExtraction(M,
                    "CR_SO",
                    sep=";"
                    )

NetMatrix <- biblioNetwork(M, 
                           analysis = "co-citation", 
                           network = "sources", 
                           sep = ";")

#dev.off() ## use only if you get an error in graphics
net=networkPlot(NetMatrix, 
                n = 105, 
                Title = "Co-Citation Network of Journals Based on Shared References", 
                type = "auto",
                labelsize=0.7,
                label.cex = F,
                halo=T,
                community.repulsion = 0.1,
                #size=2,
                size.cex=T, 
                remove.multiple=T, 
                remove.isolates = T,
                edgesize = 1, 
                edges.min=6
                )

netstat <- networkStat(NetMatrix)
summary(netstat,k=10)

# Section 3: Historiograph - Direct citation linkages
histResults <- histNetwork(M, 
                           sep = ";")

options(width = 140)
net <- histPlot(histResults, 
                n=30, 
                size = 2, 
                labelsize = 2)

# Section 4: The conceptual structure - Co-Word Analysis
NetMatrix <- biblioNetwork(M, 
                           analysis = "co-occurrences", 
                           network = "keywords", 
                           sep = ";")

net=networkPlot(NetMatrix, 
                normalize="association",
                n = 105, 
                Title = "Co-occurrence Network of Keywords Based on Appearing in Multiple Papers", 
                type = "mds", 
                labelsize=0.6,
                label.cex=F,
                halo = T,
                cluster = "optimal",
                community.repulsion = 0.2,
                #size=10, 
                size.cex=T,
                remove.multiple=T,
                remove.isolates=T,
                weighted = T,
                edgesize = 1,
                edges.min=5
                )

netstat <- networkStat(NetMatrix)
summary(netstat,k=10)

# Co-word Analysis through Correspondence Analysis
suppressWarnings(
  CS <- conceptualStructure(M, 
                            method="MCA", 
                            field="ID", 
                            minDegree=5, 
                            clust=5, 
                            stemming=FALSE, 
                            labelsize=15,
                            documents=20
                            )
                )

# Section 5: Thematic Map
Map=thematicMap(M, 
                field = "ID", 
                n = 250, 
                minfreq = 4,
                stemming = FALSE, 
                size = 0.7, 
                n.labels=5, 
                repel = TRUE)

plot(Map$map)

# Cluster description
Clusters=Map$words[order(Map$words$Cluster,-Map$words$Occurrences),]
library(dplyr)
CL <- Clusters %>% group_by(.data$Cluster_Label) %>% top_n(5, .data$Occurrences)
CL


# Section 6: The social structure - Collaboration Analysis

# Author collaboration network
NetMatrix <- biblioNetwork(M, 
                           analysis = "collaboration", 
                           network = "authors", 
                           sep = ";"
                           )

net=networkPlot(NetMatrix,  
                n = 40, 
                Title = "Author collaboration",
                type = "auto", 
                size.cex=T, 
                #size=10,
                edgesize = 1, 
                normalize = "salton",
                label.cex=TRUE,
                community.repulsion = 0.01,
                edges.min=1
                )

netstat <- networkStat(NetMatrix)
summary(netstat,k=15)

# Institutional collaboration network
NetMatrix <- biblioNetwork(M, 
                           analysis = "collaboration", 
                           network = "universities", 
                           sep = ";"
                           )

net=networkPlot(NetMatrix,  
                n = 20, 
                Title = "University collaboration",
                type = "auto", 
                size.cex=T,
                edgesize = 1,
                label.cex=TRUE,
                community.repulsion = 0.02,
                edges.min=1
                )

netstat <- networkStat(NetMatrix)
summary(netstat,k=15)

# Country collaboration network
M <- metaTagExtraction(M, 
                       Field = "AU_CO", 
                       sep = ";"
                       )

NetMatrix <- biblioNetwork(M, 
                           analysis = "collaboration", 
                           network = "countries", 
                           sep = ";"
                           )

net=networkPlot(NetMatrix,  
                n = dim(NetMatrix)[1], 
                Title = "Country collaboration",
                type = "circle", 
                size.cex=T,edgesize = 1,
                label.cex=TRUE, 
                cluster="none"
                )

#biblioshiny()
