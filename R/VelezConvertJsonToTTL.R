# arguments
# dictDataDir <- velezDataDir
# JsonFilename <- "Velez1744List.json"
# prefixesFilename <- "Velez1744prefixes.txt"

ConvertJsonToTurtle <- function(dictDataDir, JsonFilename, prefixesFilename){

   DictDataJson <- read_file(paste0(dictDataDir,JsonFilename))

   Json2Turtle <- gsub("    \"", "    ", DictDataJson) %>%
      gsub("  \"", "# ", .) %>%
      gsub("\n    \\}\n  \\}\n\\}", " .\n\n# END OF FILE", .) %>%
      gsub("\n    \\}\n  \\},", " .\n\n", .) %>%
      gsub("\": \\{", "", .) %>%
      gsub("\n    \\},", " .\n", .) %>%
      gsub(",\n", " ;\n", .) %>%
      gsub("\": \\[\".\"", " ", .) %>%
      gsub(".\"\"\\]", "", .) %>%
      gsub("\": \\[\"", " ", .) %>%
      gsub("\"\\]", "", .) %>%
      gsub("##", "\"", .)

   Json2Turtle <- gsub("lexicog\\.", "lexicog:", Json2Turtle) %>%
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
      gsub(";\n      subClass ", ", ontolex:", .)

   Json2Turtle <- gsub("velez_entry.", "velez_entry:", Json2Turtle) %>%
      gsub("velez_comp\\.", "velez_comp:", .) %>%
      gsub("velez_le\\.", "velez_le:", .) %>%
      gsub("velez_form\\.", "velez_form:", .) %>%
      gsub("velez_ls\\.", "velez_ls:", .) %>%
      gsub("velez_lc\\.", "velez_lc:", .) %>%
      gsub("velez_ue\\.", "velez_ue:", .) %>%
      gsub("lila_resource\\.", "lila_resource:", .) %>%
      gsub("lila_lemma\\.", "lila_lemma:", .) %>%
      gsub("lila_hypolemma\\.", "lila_hypolemma:", .)

   # ADD PREFIXES
   VelezPrefixes <- read_file(paste0(dictDataDir,prefixesFilename))
   Json2Turtle <- gsub("\\{", VelezPrefixes, Json2Turtle)

   write(Json2Turtle, paste0(dictDataDir,gsub(".json", ".ttl", JsonFilename)))

   return(print("The turtle file was saved in the source directory."))

}
