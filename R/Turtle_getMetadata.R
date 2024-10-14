# CREATE LISTS FOR BOTH RESOURCES 'lexicog:LexicographicResource' AND 'lime:Lexicon'
if(identical(sourcePrefix, "Velez1744\\.")){
   resource.LexicogList <- list(
      a="lexicog:LexicographicResource", 
      rdfs.label="##Velez's Index Totius Artis (1744) - Lexicographic Resource##", 
      dcterms.language="##la##", 
      lexicog.entry=str_flatten_comma(SourceDFList$LexicogEntriesDF$lexicog.Entry))
   resource.LexiconList <- list(
      a="lime:Lexicon , crm:E31", 
      rdfs.label="##Velez's Index Totius Artis (1744) - Lexical Resource##", 
      owl.versionInfo=paste0("##Released in ", date(), "##"), 
      rdfs.seeAlso="<http://clp.dlc.ua.pt/DICIweb/default.asp?url=Obras&Livro=8&Edicao=9>", 
      dcterms.title="##Antonii Vellesii Amiensis Index Totius Artis. Eborae: ex Typographia Academiae, 1744.##@la", 
      dcterms.creator="<http://www.wikidata.org/entity/Q123385445>", 
      dcterms.contributor="##Lucas Dezotti, Marco Passarotti, Francesco Mambrini, Paolo Ruffolo, Giovanni Moretti##", 
      dcterms.description="##A Latin-Portuguese bilingual dictionary curated by Antonio Velez as an index for the Latin Grammar of Manuel Alvarez, published by Eborensis Typographia Academica in 1744.##@en", 
      dcterms.license = "<http://www.wikidata.org/entity/Q42553662>", 
      dcterms.publisher="<http://www.wikidata.org/entity/Q89883181>", 
      dcterms.language="<http://lexvo.org/id/iso639-1/la>, <http://www.lexvo.org/page/iso639-3/lat>, <http://id.loc.gov/vocabulary/iso639-2/lat>", 
      lime.language="##la##", 
      lime.lexicalEntries=paste0("##", nrow(SourceDFList$LexicalEntriesDF), "##^^xsd:integer"), 
      lime.linguisticCatalog="<http://www.lexinfo.net/ontologies/2.0/lexinfo>", 
      lime.entry=str_flatten_comma(SourceDFList$LexicalEntriesDF$ontolex.LexicalEntry))
}
if(identical(sourcePrefix, "Fonseca1798\\.")){
   resource.LexicogList<- list(
      a="lexicog:LexicographicResource", 
      rdfs.label="##Fonseca's Parvum Lexicon Latinum (1798) - Lexicographic Resource##", 
      dcterms.language="##la##", 
      lexicog.entry=str_flatten_comma(SourceDFList$LexicogEntriesDF$lexicog.Entry))
   resource.LexiconList <- list(
      a="lime:Lexicon , crm:E31", 
      rdfs.label="##Fonseca's Parvum Lexicon Latinum (1798) - Lexical Resource##", 
      owl.versionInfo=paste0("##Released in ", date(), "##"), 
      rdfs.seeAlso="<http://clp.dlc.ua.pt/DICIweb/default.asp?url=Obras&Livro=16&Edicao=17>", 
      dcterms.title="##Petri Iosephi a Fonseca [...] Parvum Lexicon Latinum Lusitana interpretatione adiecta. Olisipone ex Typographia Regia, 1798.##@la", 
      dcterms.creator="<http://www.wikidata.org/entity/Q55876159>", 
      dcterms.contributor="##Lucas Dezotti, Marco Passarotti, Francesco Mambrini, Paolo Ruffolo, Giovanni Moretti##", 
      dcterms.description="##A Latin-Portuguese bilingual dictionary curated by Pedro José da Fonseca, published in Lisbon by the Typographia Regia in 1798.##@en", 
      dcterms.license = "<http://www.wikidata.org/entity/Q42553662>", 
      dcterms.publisher="<http://www.wikidata.org/entity/Q89883181>", 
      dcterms.language="<http://lexvo.org/id/iso639-1/la>, <http://www.lexvo.org/page/iso639-3/lat>, <http://id.loc.gov/vocabulary/iso639-2/lat>", 
      lime.language="##la##", 
      lime.lexicalEntries=paste0("##", nrow(SourceDFList$LexicalEntriesDF), "##^^xsd:integer"), 
      lime.linguisticCatalog="<http://www.lexinfo.net/ontologies/2.0/lexinfo>", 
      lime.entry=str_flatten_comma(SourceDFList$LexicalEntriesDF$ontolex.LexicalEntry))
}
if(identical(sourcePrefix, "Cardoso1570\\.")){
   resource.LexicogList<- list(
      a="lexicog:LexicographicResource", 
      rdfs.label="##Cardoso's Dictionarium latinolusitanicum (1570) - Lexicographic Resource##", 
      dcterms.language="##la##", 
      lexicog.entry=str_flatten_comma(SourceDFList$LexicogEntriesDF$lexicog.Entry))
   resource.LexiconList <- list(
      a="lime:Lexicon , crm:E31", 
      rdfs.label="##Cardoso's Dictionarium latinolusitanicum (1570) - Lexical Resource##", 
      owl.versionInfo=paste0("##Released in ", date(), "##"), 
      rdfs.seeAlso="<http://clp.dlc.ua.pt/DICIweb/default.asp?url=Obras&Livro=5&Edicao=5>", 
      dcterms.title="##Dictionarium latinolusitanicum et vice versa Lusitanicolatinum, cum adagiorum ferè omnium iuxta seriem alphabeticam perutili expositione: Ecclesiasticorum etiam vocabulorum interpretatione. Item de monetis, ponderibus, et mensuris, ad praesentem usum accomodatis. Noue omnia per Hieronymum Cardosum Lusitanum congesta. Recognita vero omnia per Sebast. Stokhamerum Germanum. [...] Excussit Ioan. Barrerius. Conimbricae. 12. kal. Iulii. 1570##@la", 
      dcterms.creator="<http://www.wikidata.org/entity/Q16832220>", 
      dcterms.contributor="##Lucas Dezotti, Marco Passarotti, Francesco Mambrini, Paolo Ruffolo, Giovanni Moretti##", 
      dcterms.description="##A Latin-Portuguese bilingual dictionary curated by Jeronimo Cardoso, published in Coimbra by João de Barreira in 1570.##@en", 
      dcterms.license = "<http://www.wikidata.org/entity/Q42553662>", 
      dcterms.publisher="<http://www.wikidata.org/entity/Q89883181>", 
      dcterms.language="<http://lexvo.org/id/iso639-1/la>, <http://www.lexvo.org/page/iso639-3/lat>, <http://id.loc.gov/vocabulary/iso639-2/lat>", 
      lime.language="##la##", 
      lime.lexicalEntries=paste0("##", nrow(SourceDFList$LexicalEntriesDF), "##^^xsd:integer"), 
      lime.linguisticCatalog="<http://www.lexinfo.net/ontologies/2.0/lexinfo>", 
      lime.entry=str_flatten_comma(SourceDFList$LexicalEntriesDF$ontolex.LexicalEntry))
}
