# libraries
library(tidyverse)
library(jsonlite)

print(date())
source("./R/TTLgetDataSet.R")
source("./R/TTLaddMarkers.R")
source("./R/TTLconvertToList.R")
source("./R/TTLfromJson.R")

# arguments
dictDataDir <- "/Users/lucascdz/FILES/doutorado/corpus/1762_Fonseca/Fonseca.1798.parvumlexicum-0.4.1_tsv/"
dictPrefix <- "Fonseca1798\\."
target <- "LiLa"

# get a list with all needed DFs
SourceDFList <- TTLGetDataSet(dictDataDir,dictPrefix,target)
# Velez = 30-40 sec. / 27 sec./ 22 sec.
# Fonseca = 10 min. / 13 min.

# ADD MARKERS TO ##string.values##
SourceDFList <- AddMarkersToValues(SourceDFList)

#
# CREATE LISTS FOR BOTH RESOURCES 'lexicog:LexicographicResource' AND 'lime:Lexicon'
if(identical(dictPrefix, "Velez1744\\.")){
   resource.LexicogList <- list(a="lexicog:LexicographicResource", rdfs.label="##Velez's Latin-Portuguese Dictionary - Lexicographic Resource##@en", dcterms.language="##la##", lexicog.entry=str_flatten_comma(SourceDFList$EntriesDF$lexicog.Entry))
   resource.LexiconList <- list(a="lime:Lexicon , crm:E31", rdfs.label="##Velez's Latin-Portuguese Dictionary (Index Totius Artis, 1744)##", owl.versionInfo=paste0("##Released in ", date(), "##"), rdfs.seeAlso="<http://clp.dlc.ua.pt/DICIweb/default.asp?url=Obras&Livro=8&Edicao=9>", dcterms.title="##Antonii Vellesii Amiensis Index Totius Artis. Eborae: ex Typographia Academiae, 1744.##@la", dcterms.creator="<http://www.wikidata.org/entity/Q123385445>", dcterms.contributor="##Lucas Consolin Dezotti, Marco Passarotti, Francesco Mambrini, Paolo Ruffolo, Giovanni Moretti##", dcterms.description="##A Latin-Portuguese bilingual dictionary curated by Antonio Velez as an index for the Latin Grammar of Manuel Alvarez, published by Eborensis Typographia Academica in 1744.##@en", dcterms.license = "<http://www.wikidata.org/entity/Q42553662>", dcterms.publisher="<http://www.wikidata.org/entity/Q89883181>", dcterms.language="<http://lexvo.org/id/iso639-1/la>, <http://www.lexvo.org/page/iso639-3/lat>, <http://id.loc.gov/vocabulary/iso639-2/lat>", lime.language="##la##", lime.lexicalEntries=paste0("##", nrow(SourceDFList$LexicalEntriesDF), "##^^xsd:integer"), lime.linguisticCatalog="<http://www.lexinfo.net/ontologies/2.0/lexinfo>", lime.entry=str_flatten_comma(SourceDFList$LexicalEntriesDF$ontolex.LexicalEntry))
} else if(identical(dictPrefix, "Fonseca1798\\.")){
   resource.LexicogList<- list(a="lexicog:LexicographicResource", rdfs.label="##Fonseca's Latin-Portuguese Dictionary - Lexicographic Resource##@en", dcterms.language="##la##", lexicog.entry=str_flatten_comma(SourceDFList$EntriesDF$lexicog.Entry))
   resource.LexiconList <- list(a="lime:Lexicon , crm:E31", rdfs.label="##Fonseca's Latin-Portuguese Dictionary (Parvum Lexicon Latinum, 1798)##@en", owl.versionInfo=paste0("##Released in ", date(), "##"), rdfs.seeAlso="<http://clp.dlc.ua.pt/DICIweb/default.asp?url=Obras&Livro=16&Edicao=17>", dcterms.title="##Petri Iosephi a Fonseca [...] Parvum Lexicon Latinum Lusitana interpretatione adiecta. Olisipone ex Typographia Regia, 1798.##@la", dcterms.creator="<http://www.wikidata.org/entity/Q55876159>", dcterms.contributor="##Lucas Consolin Dezotti, Marco Passarotti, Francesco Mambrini, Paolo Ruffolo, Giovanni Moretti##", dcterms.description="##A Latin-Portuguese bilingual dictionary curated by Pedro José da Fonseca, published in Lisbon by the Typographia Regia in 1798.##@en", dcterms.license = "<http://www.wikidata.org/entity/Q42553662>", dcterms.publisher="<http://www.wikidata.org/entity/Q89883181>", dcterms.language="<http://lexvo.org/id/iso639-1/la>, <http://www.lexvo.org/page/iso639-3/lat>, <http://id.loc.gov/vocabulary/iso639-2/lat>", lime.language="##la##", lime.lexicalEntries=paste0("##", nrow(SourceDFList$LexicalEntriesDF), "##^^xsd:integer"), lime.linguisticCatalog="<http://www.lexinfo.net/ontologies/2.0/lexinfo>", lime.entry=str_flatten_comma(SourceDFList$LexicalEntriesDF$ontolex.LexicalEntry))
}




######
# NOTA BENE !!!
# CONSIDER EXCLUDING ENTRIES WITHOUT LEXICAL SENSE (but preserve those mentioned in 'decomp')

# CHANGE IN 'subClass': now including 'lexinfo:contraction'... check gsub() that adds 'ontolex:'
#############



#
# CONVERT ALL DF TO LISTS (OBS. important:: the first column HAS TO BE the instance ID that will be referred to the ontology class (e.g. 'ontolex.LexicalEntry')

SourceDataList <- ConvertToList(SourceDFList, dictDataDir, resource.LexicogList, resource.LexiconList)
# VELEZ = 5-7 seconds
# FONSECA = 30 sec.

# cf. SourceDataList_old <- ConvertToList_old(SourceDFList, dictDataDir, resource.LexicogList, resource.LexiconList)
# VELEZ = 6 seconds MESMA coisa, porém é muito cheio de variáveis...


# CONVERT LIST TO JSON FILE ('Lila-style') (tempo = last task)
SourceDataJson <- jsonlite::toJSON(SourceDataList, pretty = T)
write(SourceDataJson, paste0(dictDataDir,gsub("([A-z]*).*", "\\1", dictPrefix),"Data.json"))

# CONVERT JSON TO TURTLE FILE ('Lila-style') (OBS. 'prefixes' defined inside the function)
ConvertJsonToTurtle(dictDataDir, SourceDataJson)

### OK ###
print(date())
## Fonseca total time = 13 min.
