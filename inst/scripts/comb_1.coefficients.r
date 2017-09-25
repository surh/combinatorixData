sizes <- system.file("raw/2016-02-24.coefficients_area_phenotypes.txt",
                     package = "combinatorixData",mustWork = TRUE)
color <- system.file("raw/2016-02-24.coefficients_color_phenotypes.txt",
                     package = "combinatorixData",mustWork = TRUE)

sizes <- read.table(sizes,header = TRUE)
head(sizes)

levels(sizes$Phenotype)[ levels(sizes$Phenotype) == "PC1_log" ] <- "PC1_log_shape"
levels(sizes$Phenotype)


color <- read.table(color, header = TRUE)
head(color)

levels(color$Phenotype)[ grep(pattern = "^PC[1-3]",
                              x =  levels(color$Phenotype)) ] <- paste(levels(color$Phenotype)[ grep(pattern = "^PC[1-3]",
                                                                                                     x =  levels(color$Phenotype)) ], "_color", sep = "")
levels(color$Phenotype)

# Combine
Dat <- rbind(sizes,color)
head(Dat)

Dat <- droplevels(Dat)
comb.phenotypes <- Dat

devtools::use_data(comb.phenotypes,pkg = "~/rhizogenomics/github/combinatorixData/",
                   overwrite = TRUE)

