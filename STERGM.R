library(statnet)
library(tergm)
library(ergm)
library(sna)
library(tsna)
library(igraph)
library(readr)
library(readxl)
library(networkDynamic)
library(ndtv)
library(tidyverse)

#Network MECHANISMS
arcos_redes<-read.csv("NetworkData.csv")
table(arcos_redes$School)

classes<-read_delim("LTA classes.csv", 
                    ",", escape_double = FALSE, trim_ws = TRUE)[,c("id","class_t0","class_t1")]
table(classes$class_t0)
table(classes$class_t1)
classes$class_t0<-if_else(classes$class_t0==1 |classes$class_t0==3,1,2)
classes$class_t1<-if_else(classes$class_t1==1 |classes$class_t1==3,1,2)

#Including two separate static networks------------
college<-13
static_edges1<-arcos_redes[arcos_redes$School==college & arcos_redes$Timepoint==1,c("StudyID","FriendStudyID")]
colnames(static_edges1)<-c("tail","head")
statics_nodes1<-data.frame(vertex.id = unique(c(static_edges1$tail,static_edges1$head)))%>%
  left_join(classes,by = c("vertex.id"="id"))
statics_nodes1[is.na(statics_nodes1)]<-0

static_networkt0<-network(static_edges1,vertex.attr = statics_nodes1,
                        vertex.attrnames = c("vertex.id","class_t0","class_t1"),
                        directed = TRUE,bipartite = FALSE)

static_edges2<-arcos_redes[arcos_redes$School==college & arcos_redes$Timepoint==2,c("StudyID","FriendStudyID")]
colnames(static_edges2)<-c("tail","head")
statics_nodes2<-data.frame(vertex.id = unique(c(static_edges2$tail,static_edges2$head)))%>%
  left_join(classes,by = c("vertex.id"="id"))
statics_nodes2[is.na(statics_nodes2)]<-0

static_networkt1<-network(static_edges2,vertex.attr = statics_nodes2,
                          vertex.attrnames = c("vertex.id","class_t0","class_t1"),
                          directed = TRUE,bipartite = FALSE)


stergm.fit1<-stergm(list(static_networkt0,static_networkt1),
                    formation = ~edges+mutual+transitiveties+nodematch("class_t1"),
                    dissolution = ~edges+mutual+transitiveties,
                    estimate = "CMLE",control = control.stergm(init.method = "zeros",CMLE.MCMC.interval = 8000,SA.burnin = 10000,
                                                               SA.restarts = 250, force.main = T,MCMC.init.maxedges = 50000,
                                                               MCMC.init.maxchanges = 50000,
                                                               CMLE.control.form = control.ergm(force.main = T,MCMC.interval = 8000,
                                                                                                MCMC.samplesize = 8000),
                                                               CMLE.control.diss =  control.ergm(force.main = T,MCMC.interval = 8000,
                                                                                                 MCMC.samplesize = 8000),
                                                               SA.runlength = 100,CMLE.MCMC.burnin = 1024*64,
                                                               SA.phase1.minruns = 50,SA.robust = T,SAN.maxit = 10,
                                                               SAN.nsteps.times = 30))

#mcmc.diagnostics(stergm.fit1)
summary(stergm.fit1)
gof.fit1<-gof(stergm.fit1,GOF = ~idegree)
#gof.fit1
plot(gof.fit1)

#Including two separate static networks for school 4, 14 and 15------------
college<-15
static_edges1<-arcos_redes[arcos_redes$School==college & arcos_redes$Timepoint==1,c("StudyID","FriendStudyID")]
colnames(static_edges1)<-c("tail","head")
statics_nodes1<-data.frame(vertex.id = unique(c(static_edges1$tail,static_edges1$head)))%>%
  left_join(classes,by = c("vertex.id"="id"))
statics_nodes1[is.na(statics_nodes1)]<-0

static_edges2<-arcos_redes[arcos_redes$School==college & arcos_redes$Timepoint==2,c("StudyID","FriendStudyID")]
colnames(static_edges2)<-c("tail","head")
statics_nodes2<-data.frame(vertex.id = unique(c(static_edges2$tail,static_edges2$head)))%>%
  left_join(classes,by = c("vertex.id"="id"))
statics_nodes2[is.na(statics_nodes2)]<-0

uniques_nodes<-statics_nodes1[statics_nodes1$vertex.id%in%statics_nodes2$vertex.id,"vertex.id"]

#filtering the edges and nodes

static_edges1<-static_edges1%>%
  filter(tail%in%uniques_nodes)%>%
  filter(head%in%uniques_nodes)
static_edges2<-static_edges2%>%
  filter(tail%in%uniques_nodes)%>%
  filter(head%in%uniques_nodes)

statics_nodes1<-statics_nodes1%>%
  filter(vertex.id%in%uniques_nodes)
statics_nodes2<-statics_nodes2%>%
  filter(vertex.id%in%uniques_nodes)

static_networkt0<-network(static_edges1,vertex.attr = statics_nodes1,
                          vertex.attrnames = c("vertex.id","class_t0","class_t1"),
                          directed = TRUE,bipartite = FALSE)
static_networkt1<-network(static_edges2,vertex.attr = statics_nodes2,
                          vertex.attrnames = c("vertex.id","class_t0","class_t1"),
                          directed = TRUE,bipartite = FALSE)
