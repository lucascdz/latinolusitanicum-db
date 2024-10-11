# arguments
# dictDataDir <- velezDataDir
# JsonFilename <- "Velez1744List.json"
# prefixesFilename <- "Velez1744prefixes.txt"

ConvertFromJson <- function(dictDataDir, SourceDataJson_alt){

   Json2Turtle <- gsub("    \"", "    ", SourceDataJson_alt) %>%
      gsub("  \"", "# ", .) %>%
      gsub("\"\n      \\}\n    \\]\n  \\}\n\\}", " .\n\n# END OF FILE", .) %>%
      gsub("\"\\]\n    \\}\n  \\},", " .\n\n", .) %>%
      gsub("\"\n      \\}\n    \\]\n  \\},", " .\n\n", .) %>%
      gsub("\": \\{", "", .) %>%
      gsub("\"\n      \\}\n    \\],", " .\n", .) %>%
      gsub("\"\\]?,\n", " ;\n", .) %>%
      gsub("\": \\[?\"", " ", .) %>%
      gsub("##", "\"", .) %>%
      gsub("(Entry:n\\d*\\w?)\": \\[\n      \\{", "\\1\n      a lexicog:Entry ;", .) %>%
      gsub("(LxgComp:n\\d*\\w?\\.e\\d*\\w?)\": \\[\n      \\{", "\\1\n      a lexicog:LexicographicComponent ;", .) %>%
      gsub("(LexEntry:n\\d*\\w?\\.e\\d*\\w?)\": \\[\n      \\{\n        subClass ", "\\1\n      a ontolex:LexicalEntry , ontolex:", .) %>%
      gsub("(Form:n\\d*\\w?\\.e\\d*\\w?\\.f\\d*\\w?)\": \\[\n      \\{", "\\1\n      a ontolex:Form ;", .) %>%
      gsub("(LexSense:n\\d*\\w?\\.sg\\d*\\w?\\.s\\d*\\w?)\": \\[\n      \\{", "\\1\n      a ontolex:LexicalSense ;", .) %>%
      gsub("(UsageEx:n\\d*\\w?\\.sg\\d*\\w?\\.s\\d*\\w?.u\\d*\\w?)\": \\[\n      \\{", "\\1\n      a lexicog:UsageExample ;", .)

   Json2Turtle <- gsub("lexicog\\.", "lexicog:", Json2Turtle) %>%
      gsub("resource\\.", "resource:", .) %>%
      gsub("decomp\\.", "decomp:", .) %>%
      gsub("rdfs\\.", "rdfs:", .) %>%
      gsub("dcterms\\.", "dcterms:", .) %>%
      gsub("owl\\.", "owl:", .) %>%
      gsub("lime\\.", "lime:", .) %>%
      gsub("ontolex\\.", "ontolex:", .) %>%
      gsub("lexinfo\\.", "lexinfo:", .) %>%
      gsub("lexinfo:note.1", "lexinfo:note", .) %>%
      gsub("lexinfo:net", "lexinfo.net", .) %>%
      gsub("skos\\.", "skos:", .) %>%
      gsub("definition.la", "definition", .) %>%
      gsub("definition.pt", "definition", .) %>%
      gsub("value.la", "value", .) %>%
      gsub("value.pt", "value", .) %>%
      gsub("label.la", "label", .) %>%
      gsub("label.pt", "label", .) %>%
      gsub("label.en", "label", .)

   # ADD PREFIXES
   #   Prefixes <- read_file(paste0(dictDataDir,"prefixes.txt"))
   Prefixes <- paste0("@prefix crm: <http://www.cidoc-crm.org/cidoc-crm/> .\n@prefix dcterms: <http://purl.org/dc/terms/> .\n@prefix owl: <http://www.w3.org/2002/07/owl#> .\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix skos: <http://www.w3.org/2004/02/skos#> .\n@prefix void: <http://rdfs.org/ns/void#> .\n@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n@prefix lime: <http://www.w3.org/ns/lemon/lime#> .\n@prefix ontolex: <http://www.w3.org/ns/lemon/ontolex#> .\n@prefix lexicog: <http://www.w3.org/ns/lemon/lexicog#> .\n@prefix lexinfo: <http://www.lexinfo.net/ontology/2.0/lexinfo#> .\n@prefix decomp: <http://www.w3.org/ns/lemon/decomp#> .\n@prefix lilaLemma: <http://lila-erc.eu/data/id/lemma/> .\n@prefix lilaIpoLemma: <http://lila-erc.eu/data/id/hypolemma/> .\n@prefix lilaPrefix: <http://lila-erc.eu/data/id/prefix/> .\n\n@prefix resource: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',dictPrefix),"/> .\n@prefix Entry: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',dictPrefix),"/id/Entry/> .\n@prefix LxgComp: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',dictPrefix),"/id/LexicographicComponent/> .\n@prefix LexEntry: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',dictPrefix),"/id/LexicalEntry/> .\n@prefix Form: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',dictPrefix),"/id/Form/> .\n@prefix LexSense: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',dictPrefix),"/id/LexicalSense/> .\n@prefix UsageEx: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',dictPrefix),"/id/UsageExample/> .\n\n")
   Json2Turtle <- gsub("\\{", Prefixes, Json2Turtle)

   write(Json2Turtle, paste0(dictDataDir,gsub("([A-z]*).*", "\\1", dictPrefix),"Data.ttl"))

   return(print("The turtle file was saved in the source directory."))

}

