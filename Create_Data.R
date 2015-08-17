head <- sapply(read.table(file = "clipboard", sep = "\t", nrow = 3),paste, collapse = "_")
head

library(data.table)
tableau <- read.table(file = "clipboard",sep = "\t", header = TRUE, stringsAsFactors = FALSE)
tableau <- as.data.table(tableau)

unique(tableau[grep("\\*", tableau[,Country]),Country])

OPEC_List <- c(unique(tableau[grep("\\*", tableau[,Country]),Country]))
OPEC_List <- as.data.table(OPEC_List)

setkey(tableau,Country)
setkey(OPEC_List,OPEC_List)
Rig_DT <- tableau[OPEC_List, OPEC := TRUE]

Rig_DT[,Date := as.Date(Rigs_DT[,Date],format = "%m/%d/%Y")]

#save("Rig_DT", file = "Rig_DT.R")
load("Rig_DT.R")



