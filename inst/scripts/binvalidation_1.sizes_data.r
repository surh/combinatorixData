# Read and process contaminated
contam_file <- system.file("raw/2017-09-05.contaminated_wells.txt",
                           package = "combinatorixData",mustWork = TRUE)
Contam <- read.table(contam_file, sep = "\t")
Contam
Contam <- apply(Contam, 1, function(x){
  x <- as.character(x)

  x[2] <- sub(pattern = "[(]",replacement = "",x = x[2])
  x[2] <- sub(pattern = "[)]",replacement = "",x = x[2])

  wells <- strsplit(x = x[2],split = ",")
  wells <- do.call(c, wells)

  wells <- sub(pattern = "^ +",replacement = "",x = wells)
  wells <- sub(pattern = " +$",replacement = "",x = wells)
  dat <-  data.frame(Plate = x[1], Well = wells)

  #wells <- paste(x[1], wells, sep = ".")

  return(dat)
})
Contam <- do.call(rbind,Contam)
Contam

# Read and process sizes from imaging
sizes_file1 <- system.file("raw/2017-09-04.binary_sizes_biomass.txt",
                           package = "combinatorixData",mustWork = TRUE)
sizes_file2 <- system.file("raw/2017-09-07.binary_sizes_green.txt",
                           package = "combinatorixData",mustWork = TRUE)
Sizes1 <- read.table(sizes_file1, sep = "\t", header = TRUE)
Sizes2 <- read.table(sizes_file2, sep = "\t", header = TRUE)

# Combine two sets of measurements
Sizes <- rbind(Sizes1, Sizes2)
head(Sizes)

########## CORRECT NA ASSIGNMENTS #####
Sizes$Strain[ Sizes$Strain == "na" ] <- "33"
Sizes$Plate[ Sizes$Plate == "na"] <- "3"
Sizes$Plate[ Sizes$Plate == "naB"] <- "4"
Sizes$PlateID <- as.character(Sizes$PlateID)
Sizes$PlateID[ Sizes$PlateID == "na.na" ] <- "33.3"
Sizes$PlateID[ Sizes$PlateID == "na.naB" ] <- "33.4"
Sizes$PlateID <- factor(Sizes$PlateID)
##########################################

# Define picture replicate (some plates were imaged twice)
Sizes$PicRep <- "A"
Sizes$PicRep[ grep(pattern = "B$",x = as.character(Sizes$PlateID)) ] <- "B"

# Rename levels of columns and rows to match plate labels
levels(Sizes$Col) <- c("1","2","3","4")
levels(Sizes$Row) <- c("A","B","C")

# Homogenize Plate ID labels for cases when there are two
Sizes$PlateID <- sub(pattern = "B$",replacement = "", x = as.character(Sizes$PlateID))

# Add contaminated column
Sizes$Contaminated <- paste(Sizes$PlateID,Sizes$Row, Sizes$Col, sep = "") %in% paste(Contam$Plate,Contam$Well, sep = "")

# Add Bacteria column
Sizes$Bacteria <- "+Bacteria"
Sizes$Bacteria[Sizes$Col == "4"] <- "No Bacteria"
Sizes$Bacteria <- relevel(factor(Sizes$Bacteria), ref = "No Bacteria")

# Add treatmen column
Sizes$Treatment <- as.character(Sizes$Strain)
Sizes$Treatment[ Sizes$Bacteria == "No Bacteria" ] <- "No Bacteria"
Sizes$Treatment <- relevel(factor(Sizes$Treatment), ref = "No Bacteria")

Sizes <- droplevels(Sizes)

head(Sizes)

binvalidation.sizes <- Sizes
devtools::use_data(binvalidation.sizes,
                   pkg = "~/rhizogenomics/github/combinatorixData/",
                   overwrite = TRUE)

