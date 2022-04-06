y1 <- read.csv("ATLANTIC-floverint_int.csv", sep = ";")
y2 <- read.csv("ATLANTIC-floverint_plant.csv", sep = ";")

library(zoo)

y3<-data.frame(y1, cbind(zoo(, 1:nrow(y1)), as.zoo(y2)))
y3<-y3[order(y3$genera_plan_ver,y3$order),]
y3$order<-na.locf(y3$order)

library (dplyr)
library (vcd)

y4 <- y3 %>%
  select(order, pollinator_group) %>% 
  count(order, pollinator_group, sort=T)

library(networkD3)

nodes <- data.frame(
  name=c(as.character(y4$order), 
         as.character(y4$pollinator_group)) %>% unique()
)

y4$order <- match(y4$order, nodes$name)-1 
y4$pollinator_group <- match(y4$pollinator_group, nodes$name)-1

library(sfo)
library(plotly)
library(magrittr)
library(htmlwidgets)
library(htmltools)

# Make the Network
network <- sankeyNetwork(Links = y4, Nodes = nodes,
                   Source = "pollinator_group", Target = "order",
                   Value = "n", NodeID = "name", 
                   sinksRight=FALSE, fontSize = 14, nodeWidth = 50, height = 600, width = 1200)

network <- htmlwidgets::prependContent(network, htmltools::tags$h1("ATLANTIC POLLINATION: a data set of flowers and interaction with nectar-feeding vertebrates from the Atlantic Forest", style = "font-size: 23px"))
network <- htmlwidgets::appendContent(network, htmltools::tags$p("Data Source: https://doi.org/10.1002/ecy.3595 | Image credit: https://img.freepik.com/ | Visualization by @fblpalmeira"))

network <- htmlwidgets::onRender(network, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
    var labels = ["Pollinator groups", "Flowering plants"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 12)
        .text(labels[i]);
    })
  }
')

network <- htmlwidgets::onRender(
  network,
  'function(el, x) { 
    d3.selectAll(".legend text").style("fill", "white");
    d3.select("body").style("background-color", "white");
    d3.select("h1").style("color", "red").style("font-family", "sans-serif");
    d3.select("body")
      .style("background-image", "url(https://img.freepik.com/free-photo/tropical-bromeliad-plant-with-red-leaves-hummingbird-painted-watercolor-illustration-is-highlighted-white-background-spring-summer-flower-wedding-invitations-postcards_615930-32.jpg?w=740)")
      .style("background-repeat", "no-repeat")
      .style("background-position", "right top")
      .style("background-size", "400px");
  }'
)

network

saveNetwork(network, "network.html", selfcontained = TRUE)

