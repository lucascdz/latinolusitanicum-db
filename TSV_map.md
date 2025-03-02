
**LexicalEntries**
decomp.subterm	lexinfo.note	ontolex.canonicalForm	ontolex.lexicalForm	ontolex.sense	rdfs.label	skos.note

| attribute | value |
|--|--|
| ontolex.LexicalEntry | lexical entry unique identifier (id) |
| subClass | ontolex:Affix OR ontolex:Word OR ontolex:MultiwordExpression OR lexinfo:contraction |
| senseGroup | keys to LexicalSenses table |
| ontolex.canonicalForm | lilaLemma:Id OR lilaIpoLemma:Id |
| rdfs.label | normalized lemma form |
| lexinfo.note | information about the lexical item placed BEFORE the semantic description in printed dictionary |
| skos.note | information about the lexical item placed AFTER the semantic description in printed dictionary |

**LexicalSenses**

| attribute | value |
|--|--|
| ontolex.LexicalSense | lexical sense unique identifier (id) |
| lexinfo.note | information about the lexical unit placed BEFORE the definition/translation in printed dictionary |
| skos.definition @la | definition/translation in latin |
| skos.definition @pt | definition/translation in portuguese |
| skos.note | information about the lemma placed AFTER the definition/translation in printed dictionary |

**UsageExamples**

| attribute | value |
|--|--|
| lexicog.UsageExample | usage example unique identifier (id) |
| lexinfo.note | information about the lexicographic example placed BEFORE its mention in printed dictionary |
| rdfs.value @la | dictionary example (in latin) |
| rdfs.value @pt | its translation  (in portuguese) |
| skos.note | information about the lexicographic example placed AFTER its mention in printed dictionary |

**Decomp**

| attribute | value |
|--|--|
| ontolex.LexicalEntry | MWE entry unique identifier (id) |
| decomp.subterm | component-related lexical entry IDs |

Besides, two files provide mappings between them (namely, _map-lexentries-lexsenses.tsv_ and _map-lexsenses-usageexamples.tsv_).

