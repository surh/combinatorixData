# Read and process contaminated
contam_file <- system.file("raw/2017-09-05.contaminated_wells.txt", package = "combinatorixData",mustWork = TRUE)
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
Sizes <- read.table("sizes.txt", sep = "\t", header = TRUE)
head(Sizes)
Sizes$PicRep <- "A"
Sizes$PicRep[ grep(pattern = "B$",x = as.character(Sizes$PlateID)) ] <- "B"
levels(Sizes$Col) <- c("1","2","3","4")
levels(Sizes$Row) <- c("A","B","C")
Sizes$PlateID <- sub(pattern = "B$",replacement = "", x = as.character(Sizes$PlateID))
Sizes$Contaminated <- paste(Sizes$PlateID,Sizes$Row, Sizes$Col, sep = "") %in% paste(Contam$Plate,Contam$Well, sep = "")
Sizes$Bacteria <- "+Bacteria"
Sizes$Bacteria[Sizes$Col == "4"] <- "No Bacteria"
head(Sizes)
