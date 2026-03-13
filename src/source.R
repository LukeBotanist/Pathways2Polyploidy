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


scale_plot_for_width <- function(p, width_cm = 8.9){
  
  base_width <- 8.9
  s <- width_cm / base_width
  
  if (inherits(p, "ggtree")) {
    
    p +
      ggtree::theme_tree() +
      scale_x_continuous(expand = expansion(mult = c(0,0))) +
      theme(
        legend.title = element_text(size = 9 * s),
        legend.text = element_text(size = 8 * s),
        legend.key.size = unit(0.5 * s, "cm"),
        legend.spacing = unit(0.15 * s, "cm"),
        plot.margin = margin(5,5,5,5),
        axis.title = element_text(size = 9 * s)
      )
    
    
  } else {
    
    p +
      theme_classic(base_size = 8 * s) +
      theme(
        axis.title = element_text(size = 9 * s),
        axis.text = element_text(size = 8 * s),
        legend.title = element_text(size = 9 * s),
        legend.text = element_text(size = 8 * s),
        legend.key.size = unit(0.5 * s, "cm"),
        legend.spacing = unit(0.15 * s, "cm"),
        plot.margin = margin(5,5,5,5)
      )
    
  }
}


theme_pub <- function(base_size = 8, base_family = "Arial") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.title = element_text(size = base_size),
      axis.text  = element_text(size = base_size),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.margin = margin(1,1,1,1)
    )
}
