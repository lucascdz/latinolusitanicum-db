library(tidyverse)
library(jsonlite)

iniTime <- date()

# get a list of source Dataframes
source("./R/Turtle_getDataset.R")
SourceDFList <- Turtle_getData(sourceDir,sourcePrefix,targetPub,targetFolder)

# add markers to string.values (##)
source("./R/Turtle_addMarkers.R")
SourceDFList <- AddMarkers(SourceDFList)

# get resources metadata
source("./R/Turtle_getMetadata.R")

# create Data list (NB each SourceDFList df first column must have the ontology class related IDs (e.g. 'ontolex.LexicalEntry')
source("./R/Turtle_asList.R")
SourceDataList <- ConvertToList(SourceDFList, sourceDir, resource.LexicogList, resource.LexiconList)

# convert List to Json
SourceDataJson <- jsonlite::toJSON(SourceDataList, pretty = T)
write(SourceDataJson, paste0(targetFolder,gsub("([A-z]*).*", "\\1", sourcePrefix),".json"))

# CONVERT JSON TO TURTLE FILE ('Lila-style') (OBS. 'prefixes' defined inside the function)
source("./R/Turtle_fromJSON.R")
ConvertFromJson(SourceDataJson, sourcePrefix, targetFolder)

endTime <- date()
print(iniTime)
print(endTime)
