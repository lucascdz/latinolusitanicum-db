# libraries
library(tidyverse)
library(jsonlite)
source("./R/VelezGetDataSet.R")
source("./R/AddMarkersToValues.R")
source("./R/VelezDataFrames2List.R")
source("./R/VelezConvertJsonToTTL.R")

# arguments
velezDataDir <- "/Users/lucascdz/FILES/doutorado/corpus/1608_Velez.1744/Velez.1744.indextotiusartis.0.4.5_tsv/"

# get a list with all needed DFs
VelezDFList <- GetFullDataSet(velezDataDir)

# ADD MARKERS TO ##values## AND <urls>
VelezDFList <- AddMarkersToValues(VelezDFList)

# CONVERT ALL DF TO LISTS ('Complete' or 'Lila-style')
VelezCompleteList <- CompleteList(VelezDFList, velezDataDir)

# CONVERT LIST TO JSON FILE ('Complete' or 'Lila-style')
VelezCompleteJson <- jsonlite::toJSON(VelezCompleteList, pretty = T) %>%
   write(., paste0(velezDataDir,"VelezComplete.json"))

# CONVERT JSON TO TURTLE FILE ('Complete' or 'Lila-style')
ConvertJsonToTurtle(velezDataDir, "VelezComplete.json", "VelezCompletePrefixes.txt")

### OK ###
