
########## Shapes #################
Full <- read.table("~/rhizogenomics/data/synthetic/combinatiorial/measurments/shape_full.txt",
                   header = TRUE)
#Area <- get_phenotype(Full = Full, Phenotype = "area")

# Take median of measurments per date per pot
Area <- aggregate(area ~ Name + Experiment + DPS, data = Full,FUN = median)
Hull.Area <- aggregate(hull.area ~ Name + Experiment + DPS, data = Full,FUN = median)
Solidity <- aggregate(solidity ~ Name + Experiment + DPS, data = Full,FUN = median)
Perimeter <- aggregate(perimeter ~ Name + Experiment + DPS, data = Full,FUN = median)
Width <- aggregate(width ~ Name + Experiment + DPS, data = Full,FUN = median)
Height <- aggregate(height ~ Name + Experiment + DPS, data = Full,FUN = median)
Longest_axis <- aggregate(longest_axis ~ Name + Experiment + DPS, data = Full,FUN = median)
Center_of_mass_x <- aggregate(center.of.mass.x ~ Name + Experiment + DPS, data = Full,FUN = median)
Center_of_mass_y <- aggregate(center.of.mass.y ~ Name + Experiment + DPS, data = Full,FUN = median)
Hull_vertices <- aggregate(hull_vertices ~ Name + Experiment + DPS, data = Full,FUN = median)
Ellipse_center_x <- aggregate(ellipse_center_x ~ Name + Experiment + DPS, data = Full,FUN = median)
Ellipse_center_y <- aggregate(ellipse_center_y ~ Name + Experiment + DPS, data = Full,FUN = median)
Ellipse_major_axis <- aggregate(ellipse_major_axis ~ Name + Experiment + DPS, data = Full,FUN = median)
Ellipse_minor_axis <- aggregate(ellipse_minor_axis ~ Name + Experiment + DPS, data = Full,FUN = median)
Ellipse_angle <- aggregate(ellipse_angle ~ Name + Experiment + DPS, data = Full,FUN = median)
Ellipse_eccentricity <- aggregate(ellipse_eccentricity ~ Name + Experiment + DPS, data = Full,FUN = median)

# Combine into one
Dat <- Area
Dat$hull.area <- Hull.Area$hull.area
Dat$solidity <- Solidity$solidity
Dat$perimeter <- Perimeter$perimeter
Dat$width <- Width$width
Dat$height <- Height$height
Dat$longes_axis <- Longest_axis$longest_axis
Dat$center.of.mass.x <- Center_of_mass_x$center.of.mass.x
Dat$center.of.mass.y <- Center_of_mass_x$center.of.mass.y
Dat$hull_vertices <- Hull_vertices$hull_vertices
Dat$ellipse_center_x <- Ellipse_center_y$ellipse_center_x
Dat$ellipse_center_y <- Ellipse_center_y$ellipse_center_y
Dat$ellipse_major_axis <- Ellipse_major_axis$ellipse_major_axis
Dat$ellipse_minor_axis <- Ellipse_minor_axis$ellipse_minor_axis
Dat$ellipse_angle<- Ellipse_angle$ellipse_angle
Dat$ellipse_eccentricity <- Ellipse_eccentricity$ellipse_eccentricity

head(Dat)
comb.shapes <- Dat

devtools::use_data(comb.shapes,
                   pkg = "~/rhizogenomics/github/combinatorixData/",
                   overwrite = TRUE)
rm(list = ls())

########## Color ############
Full <- read.table("~/rhizogenomics/experiments/2015/2015-11-21.comb_color/col_full.txt",
                   header = TRUE)

#' Calculates median from histogram
median_from_hist <- function(counts,bins){
  med <- bins[which(cumsum(counts/sum(counts)) >= 0.5)[1]]

  return(med)
}

#' Calculates mean from histogram
mean_from_hist <- function(counts,bins){
  x.mean <- sum(counts*bins) / sum(counts)

  return(x.mean)
}

# Get median per feature per picture and save
Dat <- aggregate(cbind(blue,green,red,lightness,green.magenta,blue.yellow,hue,saturation,value) ~ Name +
                   Experiment + DPS + filename,
                 data = Full,FUN = median_from_hist,
                 bins = 0:255)
head(Dat)

# Now we aggregate multiple pictures per plant
Dat <- aggregate(cbind(blue,green,red,lightness,green.magenta,blue.yellow,hue,saturation,value) ~ Name +
                   Experiment + DPS,
                 data = Dat,FUN = median)

head(Dat)

comb.color <- Dat

devtools::use_data(comb.color,
                   pkg = "~/rhizogenomics/github/combinatorixData/",
                   overwrite = TRUE)
rm(list = ls())



########## Designs ###############
# Combine designs
Designs <- list(COMB1.2 = read.table("~/rhizogenomics/data/synthetic/combinatiorial/maps/COMB1.2_design.txt"),
                COMB2 = read.table("~/rhizogenomics/data/synthetic/combinatiorial/maps/COMB2_design.txt"),
                COMB3 = read.table("~/rhizogenomics/data/synthetic/combinatiorial/maps/COMB3_design.txt"),
                COMB4 = read.table("~/rhizogenomics/data/synthetic/combinatiorial/maps/COMB4_design.txt"),
                COMB5 = read.table("~/rhizogenomics/data/synthetic/combinatiorial/maps/COMB5_design.txt"))
Designs$COMB5[ Designs$COMB5 == 1 ] <- 0
strains <- unique(as.vector(sapply(Designs,colnames)))
strains <- strains[ strains != "X57x" & strains != "X72x" ]

Designs <- lapply(Designs, function(mat){
  new <- setdiff(strains,colnames(mat))
  dat <- matrix(0,nrow = nrow(mat), ncol = length(new))
  colnames(dat) <- new
  dat <- cbind(mat,dat)
  dat <- dat[,strains]
  return(dat)
})
names(Designs) <- NULL
Design <- do.call(rbind,Designs)
head(Design)

devtools::use_data(Design,
                   pkg = "~/rhizogenomics/github/combinatorixData/",
                   overwrite = TRUE)
rm(list = ls())

