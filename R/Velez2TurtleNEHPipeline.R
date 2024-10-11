# libraries
library(tidyverse)
library(jsonlite)
source("./R/VelezGetDataSetLiLa.R")
source("./R/VelezAddMarkersToValuesLiLa.R")
source("./R/VelezDataFrame2ListLiLa.R")
source("./R/VelezConvertJsonToTTL.R")

# arguments
velezDataDir <- "/Users/lucascdz/FILES/doutorado/corpus/1608_Velez.1744/Velez.1744.indextotiusartis.0.5.1_tsv/"

# get a list with all needed DFs
VelezDFList <- GetDataSet(velezDataDir)

# ADD MARKERS TO ##values## AND <url>s
VelezDFList <- AddMarkersToValues(VelezDFList)

# CONVERT ALL DF TO LISTS ('Lila-style')
VelezLilaList <- ConvertToList(VelezDFList, velezDataDir)

# CONVERT LIST TO JSON FILE ('Lila-style')
VelezLilaJson <- jsonlite::toJSON(VelezLilaList, pretty = T) %>%
   write(., paste0(velezDataDir,"VelezLila.json"))

# CONVERT JSON TO TURTLE FILE ('Lila-style')
ConvertJsonToTurtle(velezDataDir, "VelezLila.json", "VelezLilaPrefixes.txt")

### OK ###
