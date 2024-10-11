# argument
# SourceDFList <- VelezDFList
# dictDataDir <- velezDataDir

# library
ConvertToList <- function(SourceDFList, dictDataDir){

   source("./R/DataFrame2List.R")


   # CREATE LISTS FOR EACH DATA FRAME
   EntriesList <- DataFrame2List(SourceDFList$EntriesDF)
   ComponentsList <- DataFrame2List(SourceDFList$ComponentsDF)
   LexicalEntriesList <- DataFrame2List(SourceDFList$LexicalEntriesDF)
   FormsList <- DataFrame2List(SourceDFList$FormsDF)
   LexicalSensesList <- DataFrame2List(SourceDFList$LexicalSensesDF)
   LexicalConceptsList <- DataFrame2List(SourceDFList$LexicalConceptsDF)
   UsageExamplesList <- DataFrame2List(SourceDFList$UsageExamplesDF)

   # CREATE LISTS FOR 'lexicog:LexicographicResource' AND 'lime:Lexicon'
   velez_resource.LexicogList <- list(a="lexicog:LexicographicResource", rdfs.label="##Velez (1744) Index Totius Artis - lexicog Lexicographic Resource##@en", dcterms.language="##la##", lexicog.entry=str_flatten_comma(SourceDFList$EntriesDF$lexicog.Entry))
   velez_resource.LexiconList <- list(a="lime:Lexicon , crm:E31", rdfs.label="##Velez (1744) Index Totius Artis##@en", rdfs.seeAlso="<http://clp.dlc.ua.pt/DICIweb/default.asp?url=Obras&Livro=8&Edicao=9>", dcterms.title="##Antonius Velesius Amiensis. 1744. Index Totius Artis. Eborensis Typographia Academica.##@la", dcterms.creator="##Lucas Consolin Dezotti##", dcterms.contributor="##Marco Passarotti, Francesco Mambrini, Paolo Ruffolo, Giovanni Moretti##", dcterms.description="##A Latin-Portuguese bilingual dictionary curated by Antonio Velez as an index for the Latin Grammar of Manuel Alvarez, published by Eborensis Typographia Academica in 1744.##@en", dcterms.license = "<http://www.wikidata.org/entity/Q42553662>", dcterms.publisher="<http://www.wikidata.org/entity/Q89883181>", dcterms.language="<http://id.loc.gov/vocabulary/iso639-2/lat>, <http://lexvo.org/id/iso639-1/la>", lime.language="##la##", lime.lexicalEntries=paste0("##", nrow(SourceDFList$LexicalEntriesDF), "##^^xsd:integer"), lime.linguisticCatalog="<http://www.lexinfo.net/ontologies/2.0/lexinfo>", lime.entry=str_flatten_comma(SourceDFList$LexicalEntriesDF$ontolex.LexicalEntry))

   # UNIFY IN ONE BIG LIST AND SAVE IT INTO RDS AND JSON FILES
   CompleteList <- list(LEXICOGRAPHIC_RESOURCE=list(velez_resource.Lexicog=velez_resource.LexicogList), LEXICOGRAPHIC_ENTRIES=EntriesList, LEXICOGRAPHIC_COMPONENTS=ComponentsList, LEXICON=list(velez_resource.Lexicon=velez_resource.LexiconList), LEXICAL_ENTRIES=LexicalEntriesList, LEXICAL_FORMS=FormsList, LEXICAL_SENSES=LexicalSensesList, LEXICAL_CONCEPTS=LexicalConceptsList, LEXICOGRAPHIC_EXAMPLES=UsageExamplesList)
   saveRDS(CompleteList, paste0(dictDataDir,"VelezCompleteList.rds"))

   return(CompleteList)
}

