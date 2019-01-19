library(igraph)
library(ggplot2)
library(tidyverse)
library(maps)

rm(list = ls())
setwd("C:/Users/wzxwa/Desktop/658/Final")
edges <- read.csv('airline_edges.csv')
nodes <- read.csv('airline_nodes.csv')

##############################################################################################################################
# Overall
net <- graph.data.frame(edges, nodes, directed=F)
V(net)[degree(net) > 150]$s_label <- V(net)$name[degree(net) > 150]
plot(net, vertex.size = sqrt(degree(net)), vertex.label=V(net)$s_label, layout=layout_with_fr)

net.df <- igraph::as_data_frame(net, what='both')
route <- net.df$edges %>%
  mutate(all = net.df$edges$Oneworld + net.df$edges$Skyteam + net.df$edges$StarAlliance) %>%
  mutate(id = rownames(net.df$edges)) %>%
  gather('from', 'to', key = "Airport_type", value = "Airport")
route <- merge(route, net.df$vertices %>% select(airport_name, name, latitude, longitude, country, city),
                     by.x = "Airport", by.y = "name")


# world map
world.map <- map_data("world")
world.map <- world.map %>% filter(region != 'Antarctica')
sqrt_degree <- sqrt(degree(net))
ggplot() +
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill='white', colour='black') +
  geom_point(data=net.df$vertices,
             aes(x=longitude, y=latitude),
             size=2, alpha=sqrt_degree/max(sqrt_degree), colour='red') +
  geom_line(data=route,
            aes(x=longitude, y=latitude, group=id),
            alpha = 0.03 * route$all, colour = "red")

#centrality
centr_degree(net)$centralization
centr_betw(net, directed = F)$centralization
centr_clo(net)$centralization
centr_eigen(net)$centralization

# Community
#community = edge.betweenness.community(net)
#plot(net, vertex.label=NA,
#     vertex.color=community$membership, vertex.size = sqrt(degree(net)),
#     mark.groups=by(seq_along(community$membership), community$membership, invisible))
#ggplot() +
#  geom_map(data=world.map, map=world.map,
#           aes(x=long, y=lat, group=group, map_id=region),
#           fill='white', colour='black') +
#  geom_point(data=net.df$vertices,
#             aes(x=longitude, y=latitude),
#             size=2, alpha=1, colour=community$membership) +
#  geom_line(data=route,
#            aes(x=longitude, y=latitude, group=id),
#            alpha = 0.03 * route$all)

##############################################################################################################################
# Oneworld
net <- graph.data.frame(edges[edges$Oneworld > 0, ], nodes, directed=F)
net <- induced.subgraph(net, V(net)[degree(net) > 0])
V(net)[degree(net) > 150]$s_label <- V(net)$name[degree(net) > 150]
plot(net, vertex.size = sqrt(degree(net)), vertex.label=V(net)$s_label, layout=layout_with_fr)

net.df <- igraph::as_data_frame(net, what='both')
route <- net.df$edges %>%
  mutate(all = net.df$edges$Oneworld + net.df$edges$Skyteam + net.df$edges$StarAlliance) %>%
  mutate(id = rownames(net.df$edges)) %>%
  gather('from', 'to', key = "Airport_type", value = "Airport")
route <- merge(route, net.df$vertices %>% select(airport_name, name, latitude, longitude, country, city),
               by.x = "Airport", by.y = "name")


# world map
world.map <- map_data("world")
world.map <- world.map %>% filter(region != 'Antarctica')
sqrt_degree <- sqrt(degree(net))
ggplot() +
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill='white', colour='black') +
  geom_point(data=net.df$vertices,
             aes(x=longitude, y=latitude),
             size=2, alpha=sqrt_degree/max(sqrt_degree), colour='red') +
  geom_line(data=route,
            aes(x=longitude, y=latitude, group=id),
            alpha = 0.03 * route$Oneworld, colour = "red")

#centrality
centr_degree(net)$centralization
centr_betw(net, directed = F)$centralization
centr_clo(net)$centralization
centr_eigen(net)$centralization

# Community
community = edge.betweenness.community(net)
plot(net, vertex.label=NA,
     vertex.color=community$membership, vertex.size = sqrt(degree(net)),
     mark.groups=by(seq_along(community$membership), community$membership, invisible))
ggplot() +
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill='white', colour='black') +
  geom_point(data=net.df$vertices,
             aes(x=longitude, y=latitude),
             size=2, alpha=1, colour=community$membership) +
  geom_line(data=route,
            aes(x=longitude, y=latitude, group=id),
            alpha = 0.03 * route$Oneworld)

##############################################################################################################################
# Skyteam
net <- graph.data.frame(edges[edges$Skyteam > 0, ], nodes, directed=F)
net <- induced.subgraph(net, V(net)[degree(net) > 0])
V(net)[degree(net) > 150]$s_label <- V(net)$name[degree(net) > 150]
plot(net, vertex.size = sqrt(degree(net)), vertex.label=V(net)$s_label, layout=layout_with_fr)

net.df <- igraph::as_data_frame(net, what='both')
route <- net.df$edges %>%
  mutate(all = net.df$edges$Oneworld + net.df$edges$Skyteam + net.df$edges$StarAlliance) %>%
  mutate(id = rownames(net.df$edges)) %>%
  gather('from', 'to', key = "Airport_type", value = "Airport")
route <- merge(route, net.df$vertices %>% select(airport_name, name, latitude, longitude, country, city),
               by.x = "Airport", by.y = "name")


# world map
world.map <- map_data("world")
world.map <- world.map %>% filter(region != 'Antarctica')
sqrt_degree <- sqrt(degree(net))
ggplot() +
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill='white', colour='black') +
  geom_point(data=net.df$vertices,
             aes(x=longitude, y=latitude),
             size=2, alpha=sqrt_degree/max(sqrt_degree), colour='red') +
  geom_line(data=route,
            aes(x=longitude, y=latitude, group=id),
            alpha = 0.03 * route$Skyteam, colour = "red")

#centrality
centr_degree(net)$centralization
centr_betw(net, directed = F)$centralization
centr_clo(net)$centralization
centr_eigen(net)$centralization

# Community
community = edge.betweenness.community(net)
plot(net, vertex.label=NA,
     vertex.color=community$membership, vertex.size = sqrt(degree(net)),
     mark.groups=by(seq_along(community$membership), community$membership, invisible))
ggplot() +
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill='white', colour='black') +
  geom_point(data=net.df$vertices,
             aes(x=longitude, y=latitude),
             size=2, alpha=1, colour=community$membership) +
  geom_line(data=route,
            aes(x=longitude, y=latitude, group=id),
            alpha = 0.03 * route$Skyteam)
##############################################################################################################################
# StarAlliance
net <- graph.data.frame(edges[edges$StarAlliance > 0, ], nodes, directed=F)
net <- induced.subgraph(net, V(net)[degree(net) > 0])
V(net)[degree(net) > 150]$s_label <- V(net)$name[degree(net) > 150]
plot(net, vertex.size = sqrt(degree(net)), vertex.label=V(net)$s_label, layout=layout_with_fr)

net.df <- igraph::as_data_frame(net, what='both')
route <- net.df$edges %>%
  mutate(all = net.df$edges$Oneworld + net.df$edges$Skyteam + net.df$edges$StarAlliance) %>%
  mutate(id = rownames(net.df$edges)) %>%
  gather('from', 'to', key = "Airport_type", value = "Airport")
route <- merge(route, net.df$vertices %>% select(airport_name, name, latitude, longitude, country, city),
               by.x = "Airport", by.y = "name")


# world map
world.map <- map_data("world")
world.map <- world.map %>% filter(region != 'Antarctica')
sqrt_degree <- sqrt(degree(net))
ggplot() +
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill='white', colour='black') +
  geom_point(data=net.df$vertices,
             aes(x=longitude, y=latitude),
             size=2, alpha=sqrt_degree/max(sqrt_degree), colour='red') +
  geom_line(data=route,
            aes(x=longitude, y=latitude, group=id),
            alpha = 0.03 * route$StarAlliance, colour = "red")

#centrality
centr_degree(net)$centralization
centr_betw(net, directed = F)$centralization
centr_clo(net)$centralization
centr_eigen(net)$centralization

# Community
community = edge.betweenness.community(net)
plot(net, vertex.label=NA,
     vertex.color=community$membership, vertex.size = sqrt(degree(net)),
     mark.groups=by(seq_along(community$membership), community$membership, invisible))
ggplot() +
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill='white', colour='black') +
  geom_point(data=net.df$vertices,
             aes(x=longitude, y=latitude),
             size=2, alpha=1, colour=community$membership) +
  geom_line(data=route,
            aes(x=longitude, y=latitude, group=id),
            alpha = 0.03 * route$StarAlliance)

sort(degree(net), decreasing=T)

# StarAlliance
net <- graph.data.frame(edges[edges$StarAlliance > 0, ], nodes, directed=F)
net <- induced.subgraph(net, V(net)[degree(net) > 0])
V(net)[degree(net) > 150]$s_label <- V(net)$name[degree(net) > 150]
plot(net, vertex.size = sqrt(degree(net)), vertex.label=V(net)$s_label, layout=layout_with_fr)

net.df <- igraph::as_data_frame(net, what='both')
route <- net.df$edges %>%
  mutate(all = net.df$edges$Oneworld + net.df$edges$Skyteam + net.df$edges$StarAlliance) %>%
  mutate(id = rownames(net.df$edges)) %>%
  gather('from', 'to', key = "Airport_type", value = "Airport")
route <- merge(route, net.df$vertices %>% select(airport_name, name, latitude, longitude, country, city),
               by.x = "Airport", by.y = "name")


# world map
world.map <- map_data("world")
world.map <- world.map %>% filter(region != 'Antarctica')
sqrt_degree <- sqrt(degree(net))
ggplot() +
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill='white', colour='black') +
  geom_point(data=net.df$vertices,
             aes(x=longitude, y=latitude),
             size=2, alpha=sqrt_degree/max(sqrt_degree), colour='red') +
  geom_line(data=route,
            aes(x=longitude, y=latitude, group=id),
            alpha = 0.03 * route$StarAlliance, colour = "red")

#centrality
centr_degree(net)$centralization
centr_betw(net, directed = F)$centralization
centr_clo(net)$centralization
centr_eigen(net)$centralization

# Community
community = edge.betweenness.community(net)
plot(net, vertex.label=NA,
     vertex.color=community$membership, vertex.size = sqrt(degree(net)),
     mark.groups=by(seq_along(community$membership), community$membership, invisible))
ggplot() +
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill='white', colour='black') +
  geom_point(data=net.df$vertices,
             aes(x=longitude, y=latitude),
             size=2, alpha=1, colour=community$membership) +
  geom_line(data=route,
            aes(x=longitude, y=latitude, group=id),
            alpha = 0.03 * route$StarAlliance)




