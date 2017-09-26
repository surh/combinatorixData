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

# Read and process green from imaging
green_file1 <- system.file("raw/2017-09-25.binary_green_biomass.txt",
                           package = "combinatorixData",mustWork = TRUE)
green_file2 <- system.file("raw/2017-09-25.binary_green_green.txt",
                           package = "combinatorixData",mustWork = TRUE)
Green1 <- read.table(green_file1, sep = "\t", header = TRUE)
Green2 <- read.table(green_file2, sep = "\t", header = TRUE)

# Combine two sets of measurements
Green <- rbind(Green1, Green2)
head(Green)

########## CORRECT NA ASSIGNMENTS #####
Green$Strain[ Green$Strain == "na" ] <- "33"
Green$Plate[ Green$Plate == "na"] <- "3"
Green$Plate[ Green$Plate == "naB"] <- "4"
Green$PlateID <- as.character(Green$PlateID)
Green$PlateID[ Green$PlateID == "na.na" ] <- "33.3"
Green$PlateID[ Green$PlateID == "na.naB" ] <- "33.4"
Green$PlateID <- factor(Green$PlateID)
##########################################

# Define picture replicate (some plates were imaged twice)
Green$PicRep <- "A"
Green$PicRep[ grep(pattern = "B$",x = as.character(Green$PlateID)) ] <- "B"

# Rename levels of columns and rows to match plate labels
levels(Green$Col) <- c("1","2","3","4")
levels(Green$Row) <- c("A","B","C")

# Homogenize Plate ID labels for cases when there are two
Green$PlateID <- sub(pattern = "B$",replacement = "", x = as.character(Green$PlateID))

# Add contaminated column
Green$Contaminated <- paste(Green$PlateID,Green$Row, Green$Col, sep = "") %in% paste(Contam$Plate,Contam$Well, sep = "")

# Add Bacteria column
Green$Bacteria <- "+Bacteria"
Green$Bacteria[Green$Col == "4"] <- "No Bacteria"
Green$Bacteria <- relevel(factor(Green$Bacteria), ref = "No Bacteria")

# Add treatmen column
Green$Treatment <- as.character(Green$Strain)
Green$Treatment[ Green$Bacteria == "No Bacteria" ] <- "No Bacteria"
Green$Treatment <- relevel(factor(Green$Treatment), ref = "No Bacteria")

Green <- droplevels(Green)

head(Green)

binvalidation.green <- Green
devtools::use_data(binvalidation.green,
                   pkg = "~/rhizogenomics/github/combinatorixData/",
                   overwrite = TRUE)
