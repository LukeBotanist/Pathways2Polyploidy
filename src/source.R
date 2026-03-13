##setting labels and shapes
cols.list.outgrp <- c(
  "vas" = "#5b9bd5", # lightblue
  "tri" = "#5fa659", # green
  "wyo" = "#c6793a", # brown
  "rig" = "#b8434a",
  "trip"= "#aea642",
  "arb"="#5752ae",
  "nov"= "gray") # red,


labels.col.outgrp <- c(
  "vas" = "A. t. vaseyana",
  "tri" = "A. t. tridentata", 
  "wyo" = "A. t. wyomingensis",
  "rig" = "A. rigida",
  "trip"= "A. tripartita",
  "arb"="A. arbuscula",
  "nov"= "A. nova")

cols.list.ingrp3tax <- c(
  "vas" = "#5b9bd5",
  "tri" = "#5fa659",
  "wyo" = "#c6793a",
  "unassigned"="grey50")

labs.list.ingrp3tax <- c(
  "vas" = "A. t. vaseyana",
  "tri" = "A. t. tridentata",
  "wyo" = "A. t. wyomingensis",
  "unassigned"="unassigned")

shp.list.ploidy <- c("unconfirmed"=19,
                     "2"=1,
                     "3"=13,
                     "4"=15)

labs.list.ploidy <- c("unconfirmed"="unconfirmed",
                      "2"="2x",
                      "3"="3x",
                      "4"="4x")


site_coords_triploid <- data.frame(lat=c(43.322, 44.604335, 39.339), 
                                   lon=c(-115.998, -117.270221, -111.520),
                                   labs=c("Orchard Common Garden\nID","Big Lookout Mountain\nOR", "Majors Flat\nCommon Garden, UT"),
                                   elev=c(1686, 1868, 2105))