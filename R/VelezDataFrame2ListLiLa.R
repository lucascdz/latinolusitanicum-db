# argument
# SourceDFList <- VelezDFList
# dictDataDir <- velezDataDir

ConvertToList <- function(SourceDFList, dictDataDir){

   # library
   source("./R/DataFrame2List.R")

   # COMPACT AND REPLACE 'prefixes' by 'LiLa url'
   # 'EntriesDF'
   SourceDFList$EntriesDF$lexicog.subComponent <- gsub("velez_comp.", "<http://lila-erc.eu/data/lexicalResources/LatinPortuguese/Velez/id/LexComponent/", SourceDFList$EntriesDF$lexicog.subComponent) %>%
      gsub("\\, <", "> \\, <", .) %>%
      gsub("(.*)", "\\1>", .) %>%
      gsub("^>$", "", .)

   # 'ComponentsDF'
   SourceDFList$ComponentsDF$lexicog.LexicographicComponent <- gsub("velez_comp.", "<http://lila-erc.eu/data/lexicalResources/LatinPortuguese/Velez/id/LexComponent/", SourceDFList$ComponentsDF$lexicog.LexicographicComponent) %>%
      gsub("(.*)", "\\1>", .)

   # 'LexicalEntriesDF'
   #SourceDFList$LexicalEntriesDF$ontolex.canonicalForm <- SourceDFList$LexicalEntriesDF$dcterms.identifier
   SourceDFList$LexicalEntriesDF$ontolex.sense <- gsub("velez_ls.", "<http://lila-erc.eu/data/lexicalResources/LatinPortuguese/Velez/id/LexicalSense/", SourceDFList$LexicalEntriesDF$ontolex.sense) %>%
      gsub("\\, <", "> \\, <", .) %>%
      gsub("(.*)", "\\1>", .) %>%
      gsub("^>$", "", .)
   SourceDFList$LexicalEntriesDF$ontolex.evokes <- gsub("velez_lc.", "<http://lila-erc.eu/data/lexicalResources/LatinPortuguese/Velez/id/LexicalConcept/", SourceDFList$LexicalEntriesDF$ontolex.evokes) %>%
      gsub("\\, <", "> \\, <", .) %>%
      gsub("(.*)", "\\1>", .) %>%
      gsub("^>$", "", .)
   #SourceDFList$LexicalEntriesDF <- SourceDFList$LexicalEntriesDF[,c(1,2,5,9,11,12)]

   # 'LexicalSensesDF'
   SourceDFList$LexicalSensesDF$ontolex.LexicalSense <- gsub("velez_ls.", "<http://lila-erc.eu/data/lexicalResources/LatinPortuguese/Velez/id/LexicalSense/", SourceDFList$LexicalSensesDF$ontolex.LexicalSense) %>%
      gsub("(.*)", "\\1>", .)
   SourceDFList$LexicalSensesDF$ontolex.isLexicalizedSenseOf <- gsub("velez_lc.", "<http://lila-erc.eu/data/lexicalResources/LatinPortuguese/Velez/id/LexicalConcept/", SourceDFList$LexicalSensesDF$ontolex.isLexicalizedSenseOf) %>%
      gsub("(.*)", "\\1>", .)
   SourceDFList$LexicalSensesDF$lexicog.usageExample <- gsub("velez_ue.", "<http://lila-erc.eu/data/lexicalResources/LatinPortuguese/Velez/id/UsageExample/", SourceDFList$LexicalSensesDF$lexicog.usageExample) %>%
      gsub("\\, <", "> \\, <", .) %>%
      gsub("(.*)", "\\1>", .) %>%
      gsub("^>$", "", .)
   #SourceDFList$LexicalSensesDF <- SourceDFList$LexicalSensesDF[,c(1,2,4,5,7)]

   # 'LexicalConceptsDF'
   SourceDFList$LexicalConceptsDF$ontolex.LexicalConcept <- gsub("velez_lc.", "<http://lila-erc.eu/data/lexicalResources/LatinPortuguese/Velez/id/LexicalConcept/", SourceDFList$LexicalConceptsDF$ontolex.LexicalConcept) %>%
      gsub("(.*)", "\\1>", .)

   # 'UsageExamplesDF'
   SourceDFList$UsageExamplesDF$lexicog.UsageExample <- gsub("velez_ue.", "<http://lila-erc.eu/data/lexicalResources/LatinPortuguese/Velez/id/UsageExample/", SourceDFList$UsageExamplesDF$lexicog.UsageExample) %>%
      gsub("(.*)", "\\1>", .)


   # CREATE LISTS FOR EACH DATA FRAME
   EntriesList <- DataFrame2List(SourceDFList$EntriesDF)
   ComponentsList <- DataFrame2List(SourceDFList$ComponentsDF)
   LexicalEntriesList <- DataFrame2List(SourceDFList$LexicalEntriesDF)
   #FormsList <- DataFrame2List(SourceDFList$FormsDF)
   LexicalSensesList <- DataFrame2List(SourceDFList$LexicalSensesDF)
   LexicalConceptsList <- DataFrame2List(SourceDFList$LexicalConceptsDF)
   UsageExamplesList <- DataFrame2List(SourceDFList$UsageExamplesDF)

   # CREATE LISTS FOR 'lexicog:LexicographicResource' AND 'lime:Lexicon'
   velez_resource.LexicogList <- list(a="lexicog:LexicographicResource", rdfs.label="##Velez's Latin-Portuguese Dictionary (Index Totius Artis, 1744) - lexicog Lexicographic Resource##@en", dcterms.language="##la##", lexicog.entry=str_flatten_comma(SourceDFList$EntriesDF$lexicog.Entry))
   velez_resource.LexiconList <- list(a="lime:Lexicon , crm:E31", rdfs.label="##Velez's Latin-Portuguese Dictionary (Index Totius Artis, 1744)##@en", owl.versionInfo=paste0("##Released in ", date(), "##"), rdfs.seeAlso="<http://clp.dlc.ua.pt/DICIweb/default.asp?url=Obras&Livro=8&Edicao=9>", dcterms.title="##Antonius Velesius Amiensis. 1744. Index Totius Artis. Eborensis Typographia Academica.##@la", dcterms.creator="<http://viaf.org/viaf/287517777>", dcterms.contributor="##Lucas Consolin Dezotti, Marco Passarotti, Francesco Mambrini, Paolo Ruffolo, Giovanni Moretti##", dcterms.description="##A Latin-Portuguese bilingual dictionary curated by Antonio Velez as an index for the Latin Grammar of Manuel Alvarez, published by Eborensis Typographia Academica in 1744.##@en", dcterms.license = "<http://www.wikidata.org/entity/Q42553662>", dcterms.publisher="<http://www.wikidata.org/entity/Q89883181>", dcterms.language="<http://id.loc.gov/vocabulary/iso639-2/lat>, <http://lexvo.org/id/iso639-1/la>", lime.language="##la##", lime.lexicalEntries=paste0("##", nrow(SourceDFList$LexicalEntriesDF), "##^^xsd:integer"), lime.linguisticCatalog="<http://www.lexinfo.net/ontologies/2.0/lexinfo>", lime.entry=str_flatten_comma(SourceDFList$LexicalEntriesDF$ontolex.LexicalEntry))

   # UNIFY IN ONE BIG LIST AND SAVE IT INTO RDS AND JSON FILES
   LilaList <- list(LEXICOGRAPHIC_RESOURCE=list(lila_resource.Lexicog=velez_resource.LexicogList), LEXICOGRAPHIC_ENTRIES=EntriesList, LEXICOGRAPHIC_COMPONENTS=ComponentsList, LEXICON=list(lila_resource.Lexicon=velez_resource.LexiconList), LEXICAL_ENTRIES=LexicalEntriesList, LEXICAL_SENSES=LexicalSensesList, LEXICAL_CONCEPTS=LexicalConceptsList, USAGE_EXAMPLES=UsageExamplesList)
   saveRDS(LilaList, paste0(dictDataDir,"VelezLilaList.rds"))

   return(LilaList)
}
