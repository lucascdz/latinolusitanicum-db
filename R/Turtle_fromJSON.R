# arguments
# targetFolder <- velezDataDir
# jsonFile <- read_file('/Users/lucascdz/FILES/atomiclab/projects/Latin-Portuguese-dictionaries/Velez.json')
# sourcePrefix <- 

ConvertFromJson <- function(jsonFile, sourcePrefix, targetFolder){
   
   # fix Classes
   Json2Turtle <- gsub("    \"", "    ", jsonFile) %>%
      gsub("  \"", "# ", .) %>%
      gsub("\"\n      \\}\n    \\]\n  \\}\n\\}", " .\n\n# END OF FILE", .) %>%
      gsub("\"\\]\n    \\}\n  \\},", " .\n\n", .) %>%
      gsub("\"\n      \\}\n    \\]\n  \\},", " .\n\n", .) %>%
      gsub("\": \\{", "", .) %>%
      gsub("\"\n      \\}\n    \\],", " .\n", .) %>%
      gsub("\"\\]?,\n", " ;\n", .) %>%
      gsub("\": \\[?\"", " ", .) %>%
      gsub("##", "\"", .) %>%
      gsub("(lexicogEntry:n\\d*\\w?)\": \\[\n      \\{", "\\1\n      a lexicog:Entry ;", .) %>%
      gsub("(lexicogComponent:n\\d*\\w?\\.e\\d*\\w?_comp)\": \\[\n      \\{", "\\1\n      a lexicog:LexicographicComponent ;", .) %>%
      gsub("(lexicalEntry:n\\d*\\w?\\.e\\d*\\w?)\": \\[\n      \\{\n        subClass ", "\\1\n      a ontolex:LexicalEntry , ", .) %>%
      gsub("(lexicalForm:n\\d*\\w?\\.e\\d*\\w?\\.f\\d*\\w?)\": \\[\n      \\{", "\\1\n      a ontolex:Form ;", .) %>%
      gsub("(lexicalSense:n\\d*\\w?\\.sg\\d*\\w?\\.s\\d*\\w?)\": \\[\n      \\{", "\\1\n      a ontolex:LexicalSense ;", .) %>%
      gsub("(usageExample:n\\d*\\w?\\.sg\\d*\\w?\\.s\\d*\\w?.u\\d*\\w?)\": \\[\n      \\{", "\\1\n      a lexicog:UsageExample ;", .)
   
   # fix Properties
   Json2Turtle <- gsub("lexicog\\.", "lexicog:", Json2Turtle) %>%
      gsub("lilaResource\\.", "lilaResource:", .) %>%
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
   Prefixes <- paste0("# Prefixes
@prefix crm: <http://www.cidoc-crm.org/cidoc-crm/> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix skos: <http://www.w3.org/2004/02/skos#> .
@prefix void: <http://rdfs.org/ns/void#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix lime: <http://www.w3.org/ns/lemon/lime#> .
@prefix ontolex: <http://www.w3.org/ns/lemon/ontolex#> .
@prefix lexicog: <http://www.w3.org/ns/lemon/lexicog#> .
@prefix lexinfo: <http://www.lexinfo.net/ontology/2.0/lexinfo#> .
@prefix decomp: <http://www.w3.org/ns/lemon/decomp#> .
@prefix lilaLemma: <http://lila-erc.eu/data/id/lemma/> .
@prefix lilaIpoLemma: <http://lila-erc.eu/data/id/hypolemma/> .
@prefix lilaPrefix: <http://lila-erc.eu/data/id/prefix/> .
@prefix lilaResource: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',sourcePrefix),"/> .
@prefix lexicogEntry: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',sourcePrefix),"/id/LexicogEntry/> .
@prefix lexicogComponent: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',sourcePrefix),"/id/LexicogComponent/> .
@prefix lexicalEntry: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',sourcePrefix),"/id/LexicalEntry/> .
@prefix lexicalForm: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',sourcePrefix),"/id/LexicalForm/> .
@prefix lexicalSense: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',sourcePrefix),"/id/LexicalSense/> .
@prefix usageExample: <http://lila-erc.eu/data/lexicalResources/LatinPortuguese/",gsub('\\d*\\\\\\.','',sourcePrefix),"/id/UsageExample/> .
\n")
   Turtle <- gsub("\\{", Prefixes, Json2Turtle)
   
   write(Turtle, paste0(targetFolder,gsub("([A-z]*).*", "\\1", sourcePrefix),".ttl"))
   
   return(print("The turtle file was saved in the source directory."))
   
}

