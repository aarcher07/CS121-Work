
```r
words <- readLines(url("http://dtkaplan.github.io/ScientificComputing/Syllabus/Daily/Day-07/word_list_moby_crossword-flat/word_list_moby_crossword.flat.txt"))

Summary <- function(words) {
    # 100 Longest Words
    numberofcharacters <- nchar(words)
    DescendingCharacters <- order(-numberofcharacters)
    longest100 <- words[DescendingCharacters[1:100]]
    LongestWords <- words[which(numberofcharacters %in% longest100)]
    
    # Word Length
    wordLength <- table(numberofcharacters)
    
    
    NoUwords <- words[(grep("[^u]", words))]
    qNoUwords <- NoUwords[(grep("[q]", NoUwords))]
    
    # Words starting with a given letter of the alphabet
    letters <- c("^a", "^b", "^c", "^d", "^e", "^f", "^g", "^h", "^i", "^j", 
        "^k", "^l", "^m", "^n", "^o", "^p", "^q", "^r", "^s", "^t", "^u", "^v", 
        "^w", "^x", "^y", "^z")
    
    Sums <- vector(length = 26)
    for (k in 1:26) {
        lettersnew <- letters[k]
        Sums[k] <- sum(grepl(lettersnew, words))
    }
    return(c(qNoUwords, LongestWords, wordLength, Sums))
}


Summary(words)
```

```
## Warning: closing unused connection 5
## (http://dtkaplan.github.io/ScientificComputing/Syllabus/Daily/Day-07/word_list_moby_crossword-flat/word_list_moby_crossword.flat.txt)
```

```
##                                                             
##           "acequia"          "acequias"          "acquaint" 
##                                                             
##      "acquaintance"     "acquaintances"  "acquaintanceship" 
##                                                             
## "acquaintanceships"        "acquainted"       "acquainting" 
##                                                             
##         "acquaints"           "acquest"          "acquests" 
##                                                             
##         "acquiesce"        "acquiesced"      "acquiescence" 
##                                                             
##     "acquiescences"       "acquiescent"     "acquiescently" 
##                                                             
##        "acquiesces"       "acquiescing"           "acquire" 
##                                                             
##          "acquired"          "acquirer"         "acquirers" 
##                                                             
##          "acquires"         "acquiring"       "acquisition" 
##                                                             
##      "acquisitions"       "acquisitive"            "acquit" 
##                                                             
##           "acquits"         "acquitted"        "acquitting" 
##                                                             
##        "adequacies"          "adequacy"          "adequate" 
##                                                             
##        "adequately"           "alfaqui"          "alfaquin" 
##                                                             
##         "alfaquins"          "alfaquis"          "aliquant" 
##                                                             
##           "aliquot"          "aliquots"      "antimosquito" 
##                                                             
##       "antiquarian"    "antiquarianism"      "antiquarians" 
##                                                             
##       "antiquaries"         "antiquary"        "antiquated" 
##                                                             
##           "antique"          "antiqued"          "antiquer" 
##                                                             
##         "antiquers"          "antiques"         "antiquing" 
##                                                             
##       "antiquities"         "antiquity"          "applique" 
##                                                             
##         "appliqued"       "appliqueing"         "appliques" 
##                                                             
##              "aqua"          "aquacade"         "aquacades" 
##                                                             
##             "aquae"        "aquamarine"       "aquamarines" 
##                                                             
##          "aquanaut"         "aquanauts"           "aquaria" 
##                                                             
##          "aquarial"          "aquarian"         "aquarians" 
##                                                             
##          "aquarist"         "aquarists"          "aquarium" 
##                                                             
##         "aquariums"             "aquas"           "aquatic" 
##                                                             
##          "aquatics"          "aquatint"        "aquatinted" 
##                                                             
##       "aquatinting"         "aquatints"          "aquatone" 
##                                                             
##         "aquatones"           "aquavit"          "aquavits" 
##                                                             
##          "aqueduct"         "aqueducts"           "aqueous" 
##                                                             
##           "aquifer"          "aquifers"          "aquiline" 
##                                                             
##           "aquiver"         "arabesque"        "arabesques" 
##                                                             
##          "arquebus"        "arquebuses"           "asquint" 
##                                                             
##           "banquet"         "banqueted"        "banqueting" 
##                                                             
##          "banquets"           "baroque"          "baroques" 
##                                                             
##            "barque"           "barques"            "basque" 
##                                                             
##           "basques"          "bedquilt"         "bedquilts" 
##                                                             
##          "beliquor"        "beliquored"       "beliquoring" 
##                                                             
##         "beliquors"          "bequeath"        "bequeathed" 
##                                                             
##       "bequeathing"         "bequeaths"           "bequest" 
##                                                             
##          "bequests"           "bezique"          "beziques" 
##                                                             
##            "bisque"           "bisques"            "bosque" 
##                                                             
##           "bosques"           "bosquet"          "bosquets" 
##                                                             
##           "bouquet"          "bouquets"          "boutique" 
##                                                             
##         "boutiques"           "briquet"          "briquets" 
##                                                             
##        "briquetted"       "briquetting"           "brusque" 
##                                                             
##         "brusquely"          "brusquer"         "brusquest" 
##                                                             
##            "buqsha"           "buqshas"         "burlesque" 
##                                                             
##        "burlesqued"        "burlesques"       "burlesquing" 
##                                                             
##           "cacique"          "caciques"            "caique" 
##                                                             
##           "caiques"            "calque"           "calqued" 
##                                                             
##           "calques"          "calquing"            "casque" 
##                                                             
##           "casqued"           "casques"           "cazique" 
##                                                             
##          "caziques"          "chaqueta"         "chaquetas" 
##                                                             
##           "charqui"          "charquid"          "charquis" 
##                                                             
##            "cheque"           "chequer"         "chequered" 
##                                                             
##        "chequering"          "chequers"           "cheques" 
##                                                             
##          "cinquain"         "cinquains"            "cinque" 
##                                                             
##           "cinques"            "cirque"           "cirques" 
##                                                             
##            "claque"           "claquer"          "claquers" 
##                                                             
##           "claques"          "claqueur"         "claqueurs" 
##                                                             
##            "clique"           "cliqued"         "cliqueier" 
##                                                             
##        "cliqueiest"           "cliques"           "cliquey" 
##                                                             
##          "cliquier"         "cliquiest"          "cliquing" 
##                                                             
##          "cliquish"            "cliquy"           "coequal" 
##                                                             
##          "coequals"          "coequate"         "coequated" 
##                                                             
##         "coequates"        "coequating"        "colloquial" 
##                                                             
##     "colloquialism"    "colloquialisms"        "colloquies" 
##                                                             
##          "colloquy"        "communique"       "communiques" 
##                                                             
##           "conquer"         "conquered"        "conquering" 
##                                                             
##         "conqueror"        "conquerors"          "conquers" 
##                                                             
##          "conquest"         "conquests"          "conquian" 
##                                                             
##         "conquians"       "consequence"      "consequences" 
##                                                             
##        "consequent"     "consequential"      "consequently" 
##                                                             
##       "consequents"            "coquet"        "coquetries" 
##                                                             
##          "coquetry"           "coquets"          "coquette" 
##                                                             
##         "coquetted"         "coquettes"        "coquetting" 
##                                                             
##          "coquille"         "coquilles"           "coquina" 
##                                                             
##          "coquinas"           "coquito"          "coquitos" 
##                                                             
##          "cotquean"         "cotqueans"   "counterquestion" 
##                                                             
##  "counterquestions"          "critique"         "critiqued" 
##                                                             
##         "critiques"        "critiquing"           "croquet" 
##                                                             
##         "croqueted"        "croqueting"          "croquets" 
##                                                             
##         "croquette"        "croquettes"           "croquis" 
##                                                             
##           "cumquat"          "cumquats"          "daiquiri" 
##                                                             
##         "daiquiris"     "delinquencies"       "delinquency" 
##                                                             
##        "delinquent"       "delinquents"      "desquamation" 
##                                                             
##     "desquamations"            "diquat"           "diquats" 
##                                                             
##  "disqualification" "disqualifications"      "disqualified" 
##                                                             
##      "disqualifies"        "disqualify"     "disqualifying" 
##                                                             
##          "disquiet"        "disquieted"       "disquieting" 
##                                                             
##         "disquiets"        "earthquake"       "earthquakes" 
##                                                             
##          "eloquent"        "eloquently"           "enquire" 
##                                                             
##          "enquired"          "enquires"         "enquiries" 
##                                                             
##         "enquiring"           "enquiry"      "equabilities" 
##                                                             
##        "equability"           "equable"           "equably" 
##                                                             
##             "equal"           "equaled"          "equaling" 
##                                                             
##          "equalise"         "equalised"         "equalises" 
##                                                             
##        "equalising"        "equalities"          "equality" 
##                                                             
##          "equalize"         "equalized"         "equalizes" 
##                                                             
##        "equalizing"          "equalled"         "equalling" 
##                                                             
##           "equally"            "equals"      "equanimities" 
##                                                             
##        "equanimity"            "equate"           "equated" 
##                                                             
##           "equates"          "equating"          "equation" 
##                                                             
##         "equations"           "equator"        "equatorial" 
##                                                             
##          "equators"         "equerries"           "equerry" 
##                                                             
##        "equestrian"       "equestrians"       "equilateral" 
##                                                             
##       "equilibrium"            "equine"          "equinely" 
##                                                             
##           "equines"        "equinities"          "equinity" 
##                                                             
##           "equinox"         "equinoxes"             "equip" 
##                                                             
##          "equipage"         "equipages"         "equipment" 
##                                                             
##        "equipments"          "equipped"          "equipper" 
##                                                             
##         "equippers"         "equipping"            "equips" 
##                                                             
##          "equiseta"         "equitable"          "equitant" 
##                                                             
##           "equites"          "equities"            "equity" 
##                                                             
##       "equivalence"      "equivalences"        "equivalent" 
##                                                             
##       "equivalents"         "equivocal"        "equivocate" 
##                                                             
##       "equivocated"       "equivocates"      "equivocating" 
##                                                             
##      "equivocation"     "equivocations"          "equivoke" 
##                                                             
##         "equivokes"           "esquire"          "esquired" 
##                                                             
##          "esquires"         "esquiring"          "exequial" 
##                                                             
##          "exequies"            "exequy"         "exquisite" 
##                                                             
##             "faqir"            "faqirs"            "faquir" 
##                                                             
##           "faquirs"             "fique"            "fiques" 
##                                                             
##       "forequarter"      "forequarters"       "frequencies" 
##                                                             
##         "frequency"          "frequent"        "frequented" 
##                                                             
##        "frequenter"       "frequenters"       "frequentest" 
##                                                             
##       "frequenting"        "frequently"         "frequents" 
##                                                             
##         "grotesque"       "grotesquely"         "harlequin" 
##                                                             
##        "harlequins"       "headquarter"     "headquartered" 
##                                                             
##    "headquartering"      "headquarters"          "henequen" 
##                                                             
##         "henequens"          "henequin"         "henequins" 
##                                                             
##          "heniquen"         "heniquens"       "hindquarter" 
##                                                             
##      "hindquarters"          "illiquid"      "inadequacies" 
##                                                             
##        "inadequacy"        "inadequate"      "inadequately" 
##                                                             
##     "inconsequence"    "inconsequences"   "inconsequential" 
##                                                             
## "inconsequentially"      "inequalities"        "inequality" 
##                                                             
##        "inequities"          "inequity"        "infrequent" 
##                                                             
##      "infrequently"        "iniquities"        "iniquitous" 
##                                                             
##          "iniquity"           "inquest"          "inquests" 
##                                                             
##           "inquiet"         "inquieted"        "inquieting" 
##                                                             
##          "inquiets"           "inquire"          "inquired" 
##                                                             
##          "inquirer"         "inquirers"          "inquires" 
##                                                             
##         "inquiries"         "inquiring"       "inquiringly" 
##                                                             
##           "inquiry"       "inquisition"      "inquisitions" 
##                                                             
##       "inquisitive"     "inquisitively"   "inquisitiveness" 
##                                                             
## "inquisitivenesses"        "inquisitor"     "inquisitorial" 
##                                                             
##       "inquisitors"     "interquartile"          "jacquard" 
##                                                             
##         "jacquards"        "jacqueline"           "jonquil" 
##                                                             
##          "jonquils"           "kumquat"          "kumquats" 
##                                                             
##           "lacquer"         "lacquered"        "lacquering" 
##                                                             
##          "lacquers"           "lacquey"         "lacqueyed" 
##                                                             
##        "lacqueying"          "lacqueys"           "liquate" 
##                                                             
##          "liquated"          "liquates"         "liquating" 
##                                                             
##      "liquefaction"     "liquefactions"       "liquefiable" 
##                                                             
##         "liquefied"         "liquefier"        "liquefiers" 
##                                                             
##         "liquefies"           "liquefy"        "liquefying" 
##                                                             
##           "liqueur"          "liqueurs"            "liquid" 
##                                                             
##         "liquidate"        "liquidated"        "liquidates" 
##                                                             
##       "liquidating"       "liquidation"      "liquidations" 
##                                                             
##       "liquidities"         "liquidity"          "liquidly" 
##                                                             
##           "liquids"         "liquified"         "liquifies" 
##                                                             
##           "liquify"        "liquifying"            "liquor" 
##                                                             
##          "liquored"         "liquoring"           "liquors" 
##                                                             
##        "loquacious"       "loquacities"         "loquacity" 
##                                                             
##            "loquat"           "loquats"           "macaque" 
##                                                             
##          "macaques"         "mannequin"        "mannequins" 
##                                                             
##            "manque"          "maquette"         "maquettes" 
##                                                             
##             "maqui"            "maquis"            "marque" 
##                                                             
##           "marquee"          "marquees"           "marques" 
##                                                             
##          "marquess"        "marquesses"           "marquis" 
##                                                             
##          "marquise"         "marquises"            "masque" 
##                                                             
##           "masquer"        "masquerade"       "masqueraded" 
##                                                             
##       "masquerader"      "masqueraders"       "masquerades" 
##                                                             
##      "masquerading"          "masquers"           "masques" 
##                                                             
##           "mesquit"          "mesquite"         "mesquites" 
##                                                             
##          "mesquits"           "mezquit"          "mezquite" 
##                                                             
##         "mezquites"          "mezquits"          "miquelet" 
##                                                             
##         "miquelets"      "misquotation"     "misquotations" 
##                                                             
##          "misquote"         "misquoted"         "misquotes" 
##                                                             
##        "misquoting"          "moquette"         "moquettes" 
##                                                             
##          "moresque"         "moresques"            "mosque" 
##                                                             
##           "mosques"          "mosquito"        "mosquitoes" 
##                                                             
##         "mosquitos"          "musquash"        "musquashes" 
##                                                             
##          "mystique"         "mystiques"          "nonequal" 
##                                                             
##         "nonequals"          "nonquota"           "oblique" 
##                                                             
##          "obliqued"         "obliquely"       "obliqueness" 
##                                                             
##     "obliquenesses"          "obliques"         "obliquing" 
##                                                             
##       "obliquities"         "obliquity"         "obloquies" 
##                                                             
##           "obloquy"         "obsequies"        "obsequious" 
##                                                             
##      "obsequiously"    "obsequiousness"  "obsequiousnesses" 
##                                                             
##           "obsequy"            "opaque"           "opaqued" 
##                                                             
##          "opaquely"        "opaqueness"      "opaquenesses" 
##                                                             
##           "opaquer"           "opaques"          "opaquest" 
##                                                             
##          "opaquing"           "oquassa"          "oquassas" 
##                                                             
##          "outquote"         "outquoted"         "outquotes" 
##                                                             
##        "outquoting"     "overqualified"          "paraquat" 
##                                                             
##         "paraquats"          "paraquet"         "paraquets" 
##                                                             
##          "paroquet"         "paroquets"           "parquet" 
##                                                             
##         "parqueted"        "parqueting"       "parquetries" 
##                                                             
##         "parquetry"          "parquets"           "pasquil" 
##                                                             
##          "pasquils"           "perique"          "periques" 
##                                                             
##        "perquisite"       "perquisites"          "physique" 
##                                                             
##         "physiques"           "picquet"          "picquets" 
##                                                             
##       "picturesque"   "picturesqueness" "picturesquenesses" 
##                                                             
##        "piquancies"          "piquancy"           "piquant" 
##                                                             
##             "pique"            "piqued"            "piques" 
##                                                             
##            "piquet"           "piquets"           "piquing" 
##                                                             
##           "piroque"          "piroques"            "plaque" 
##                                                             
##           "plaques"          "postique"         "postiques" 
##                                                             
##          "pratique"         "pratiques"     "predelinquent" 
##                                                             
##      "prerequisite"     "prerequisites"     "propinquities" 
##                                                             
##       "propinquity"            "pulque"           "pulques" 
##                                                             
##              "qaid"             "qaids"            "qindar" 
##                                                             
##           "qindars"            "qintar"           "qintars" 
##                                                             
##            "qiviut"           "qiviuts"              "qoph" 
##                                                             
##             "qophs"               "qua"             "quack" 
##                                                             
##           "quacked"        "quackeries"          "quackery" 
##                                                             
##          "quacking"          "quackish"          "quackism" 
##                                                             
##         "quackisms"            "quacks"              "quad" 
##                                                             
##           "quadded"          "quadding"        "quadrangle" 
##                                                             
##       "quadrangles"      "quadrangular"          "quadrans" 
##                                                             
##          "quadrant"        "quadrantes"         "quadrants" 
##                                                             
##           "quadrat"          "quadrate"         "quadrated" 
##                                                             
##         "quadrates"        "quadrating"          "quadrats" 
##                                                             
##           "quadric"          "quadrics"          "quadriga" 
##                                                             
##         "quadrigae"     "quadrilateral"    "quadrilaterals" 
##                                                             
##         "quadrille"        "quadrilles"          "quadroon" 
##                                                             
##         "quadroons"         "quadruped"       "quadrupedal" 
##                                                             
##        "quadrupeds"         "quadruple"        "quadrupled" 
##                                                             
##        "quadruples"        "quadruplet"       "quadruplets" 
##                                                             
##       "quadrupling"             "quads"            "quaere" 
##                                                             
##           "quaeres"          "quaestor"         "quaestors" 
##                                                             
##             "quaff"           "quaffed"           "quaffer" 
##                                                             
##          "quaffers"          "quaffing"            "quaffs" 
##                                                             
##              "quag"            "quagga"           "quaggas" 
##                                                             
##          "quaggier"         "quaggiest"            "quaggy" 
##                                                             
##          "quagmire"         "quagmires"        "quagmirier" 
##                                                             
##       "quagmiriest"          "quagmiry"             "quags" 
##                                                             
##           "quahaug"          "quahaugs"            "quahog" 
##                                                             
##           "quahogs"              "quai"            "quaich" 
##                                                             
##          "quaiches"           "quaichs"            "quaigh" 
##                                                             
##           "quaighs"             "quail"           "quailed" 
##                                                             
##          "quailing"            "quails"            "quaint" 
##                                                             
##          "quainter"         "quaintest"          "quaintly" 
##                                                             
##        "quaintness"      "quaintnesses"             "quais" 
##                                                             
##             "quake"            "quaked"            "quaker" 
##                                                             
##           "quakers"            "quakes"           "quakier" 
##                                                             
##          "quakiest"           "quakily"           "quaking" 
##                                                             
##             "quaky"             "quale"            "qualia" 
##                                                             
##     "qualification"    "qualifications"         "qualified" 
##                                                             
##         "qualifier"        "qualifiers"         "qualifies" 
##                                                             
##           "qualify"        "qualifying"       "qualitative" 
##                                                             
##         "qualities"           "quality"             "qualm" 
##                                                             
##          "qualmier"         "qualmiest"          "qualmish" 
##                                                             
##            "qualms"            "qualmy"           "quamash" 
##                                                             
##         "quamashes"          "quandang"         "quandangs" 
##                                                             
##        "quandaries"          "quandary"          "quandong" 
##                                                             
##         "quandongs"             "quant"            "quanta" 
##                                                             
##           "quantal"           "quanted"           "quantic" 
##                                                             
##          "quantics"        "quantified"        "quantifies" 
##                                                             
##          "quantify"       "quantifying"          "quanting" 
##                                                             
##      "quantitative"        "quantities"          "quantity" 
##                                                             
##          "quantize"         "quantized"         "quantizes" 
##                                                             
##        "quantizing"          "quantong"         "quantongs" 
##                                                             
##            "quants"           "quantum"        "quarantine" 
##                                                             
##       "quarantined"       "quarantines"      "quarantining" 
##                                                             
##             "quare"             "quark"            "quarks" 
##                                                             
##           "quarrel"         "quarreled"        "quarreling" 
##                                                             
##        "quarrelled"       "quarrelling"          "quarrels" 
##                                                             
##       "quarrelsome"          "quarried"          "quarrier" 
##                                                             
##         "quarriers"          "quarries"            "quarry" 
##                                                             
##         "quarrying"             "quart"           "quartan" 
##                                                             
##          "quartans"            "quarte"           "quarter" 
##                                                             
##       "quarterback"     "quarterbacked"    "quarterbacking" 
##                                                             
##      "quarterbacks"         "quartered"        "quartering" 
##                                                             
##       "quarterlies"         "quarterly"     "quartermaster" 
##                                                             
##    "quartermasters"          "quartern"         "quarterns" 
##                                                             
##          "quarters"           "quartes"           "quartet" 
##                                                             
##          "quartets"           "quartic"          "quartics" 
##                                                             
##          "quartile"         "quartiles"            "quarto" 
##                                                             
##           "quartos"            "quarts"            "quartz" 
##                                                             
##          "quartzes"            "quasar"           "quasars" 
##                                                             
##             "quash"           "quashed"           "quashes" 
##                                                             
##          "quashing"             "quasi"             "quass" 
##                                                             
##           "quasses"           "quassia"          "quassias" 
##                                                             
##           "quassin"          "quassins"             "quate" 
##                                                             
##          "quatorze"         "quatorzes"          "quatrain" 
##                                                             
##         "quatrains"            "quatre"           "quatres" 
##                                                             
##            "quaver"          "quavered"          "quaverer" 
##                                                             
##         "quaverers"         "quavering"           "quavers" 
##                                                             
##           "quavery"              "quay"           "quayage" 
##                                                             
##          "quayages"          "quaylike"             "quays" 
##                                                             
##          "quayside"         "quaysides"             "quean" 
##                                                             
##            "queans"          "queasier"         "queasiest" 
##                                                             
##          "queasily"        "queasiness"      "queasinesses" 
##                                                             
##            "queasy"          "queazier"         "queaziest" 
##                                                             
##            "queazy"             "queen"           "queened" 
##                                                             
##          "queening"         "queenlier"        "queenliest" 
##                                                             
##           "queenly"            "queens"             "queer" 
##                                                             
##           "queered"           "queerer"          "queerest" 
##                                                             
##          "queering"          "queerish"           "queerly" 
##                                                             
##         "queerness"       "queernesses"            "queers" 
##                                                             
##             "quell"           "quelled"           "queller" 
##                                                             
##          "quellers"          "quelling"            "quells" 
##                                                             
##            "quench"        "quenchable"          "quenched" 
##                                                             
##          "quencher"         "quenchers"          "quenches" 
##                                                             
##         "quenching"        "quenchless"          "quenelle" 
##                                                             
##         "quenelles"          "quercine"           "querida" 
##                                                             
##          "queridas"           "queried"           "querier" 
##                                                             
##          "queriers"           "queries"           "querist" 
##                                                             
##          "querists"             "quern"            "querns" 
##                                                             
##         "querulous"       "querulously"     "querulousness" 
##                                                             
##   "querulousnesses"             "query"          "querying" 
##                                                             
##             "quest"           "quested"           "quester" 
##                                                             
##          "questers"          "questing"          "question" 
##                                                             
##      "questionable"        "questioned"        "questioner" 
##                                                             
##       "questioners"       "questioning"     "questionnaire" 
##                                                             
##    "questionnaires"     "questionniare"    "questionniares" 
##                                                             
##         "questions"           "questor"          "questors" 
##                                                             
##            "quests"           "quetzal"         "quetzales" 
##                                                             
##          "quetzals"             "queue"            "queued" 
##                                                             
##          "queueing"            "queuer"           "queuers" 
##                                                             
##            "queues"           "queuing"              "quey" 
##                                                             
##             "queys"            "quezal"          "quezales" 
##                                                             
##           "quezals"           "quibble"          "quibbled" 
##                                                             
##          "quibbler"         "quibblers"          "quibbles" 
##                                                             
##         "quibbling"            "quiche"           "quiches" 
##                                                             
##             "quick"           "quicken"         "quickened" 
##                                                             
##        "quickening"          "quickens"           "quicker" 
##                                                             
##          "quickest"           "quickie"          "quickies" 
##                                                             
##           "quickly"         "quickness"       "quicknesses" 
##                                                             
##            "quicks"         "quicksand"        "quicksands" 
##                                                             
##          "quickset"         "quicksets"       "quicksilver" 
##                                                             
##      "quicksilvers"              "quid"        "quiddities" 
##                                                             
##          "quiddity"          "quidnunc"         "quidnuncs" 
##                                                             
##             "quids"        "quiescence"       "quiescences" 
##                                                             
##         "quiescent"             "quiet"           "quieted" 
##                                                             
##           "quieten"         "quietened"        "quietening" 
##                                                             
##          "quietens"           "quieter"          "quieters" 
##                                                             
##          "quietest"          "quieting"          "quietism" 
##                                                             
##         "quietisms"          "quietist"         "quietists" 
##                                                             
##           "quietly"         "quietness"       "quietnesses" 
##                                                             
##            "quiets"          "quietude"         "quietudes" 
##                                                             
##           "quietus"         "quietuses"             "quiff" 
##                                                             
##            "quiffs"             "quill"           "quillai" 
##                                                             
##          "quillais"           "quilled"           "quillet" 
##                                                             
##          "quillets"          "quilling"            "quills" 
##                                                             
##             "quilt"           "quilted"           "quilter" 
##                                                             
##          "quilters"          "quilting"         "quiltings" 
##                                                             
##            "quilts"         "quinaries"           "quinary" 
##                                                             
##           "quinate"            "quince"           "quinces" 
##                                                             
##          "quincunx"        "quincunxes"          "quinella" 
##                                                             
##         "quinellas"            "quinic"          "quiniela" 
##                                                             
##         "quinielas"            "quinin"           "quinina" 
##                                                             
##          "quininas"           "quinine"          "quinines" 
##                                                             
##           "quinins"           "quinnat"          "quinnats" 
##                                                             
##            "quinoa"           "quinoas"           "quinoid" 
##                                                             
##          "quinoids"            "quinol"          "quinolin" 
##                                                             
##         "quinolins"           "quinols"           "quinone" 
##                                                             
##          "quinones"          "quinsies"            "quinsy" 
##                                                             
##             "quint"          "quintain"         "quintains" 
##                                                             
##           "quintal"          "quintals"           "quintan" 
##                                                             
##          "quintans"           "quintar"          "quintars" 
##                                                             
##      "quintessence"     "quintessences"    "quintessential" 
##                                                             
##           "quintet"          "quintets"           "quintic" 
##                                                             
##          "quintics"          "quintile"         "quintiles" 
##                                                             
##           "quintin"          "quintins"            "quints" 
##                                                             
##         "quintuple"        "quintupled"        "quintuples" 
##                                                             
##        "quintuplet"       "quintuplets"       "quintupling" 
##                                                             
##              "quip"           "quipped"          "quipping" 
##                                                             
##          "quippish"            "quippu"           "quippus" 
##                                                             
##             "quips"          "quipster"         "quipsters" 
##                                                             
##             "quipu"            "quipus"             "quire" 
##                                                             
##            "quired"            "quires"           "quiring" 
##                                                             
##             "quirk"           "quirked"          "quirkier" 
##                                                             
##         "quirkiest"          "quirkily"          "quirking" 
##                                                             
##            "quirks"            "quirky"             "quirt" 
##                                                             
##           "quirted"          "quirting"            "quirts" 
##                                                             
##          "quisling"         "quislings"              "quit" 
##                                                             
##            "quitch"          "quitches"             "quite" 
##                                                             
##          "quitrent"         "quitrents"             "quits" 
##                                                             
##           "quitted"           "quitter"          "quitters" 
##                                                             
##          "quitting"           "quittor"          "quittors" 
##                                                             
##            "quiver"          "quivered"          "quiverer" 
##                                                             
##         "quiverers"         "quivering"           "quivers" 
##                                                             
##           "quivery"           "quixote"          "quixotes" 
##                                                             
##          "quixotic"        "quixotries"          "quixotry" 
##                                                             
##              "quiz"        "quizmaster"       "quizmasters" 
##                                                             
##           "quizzed"           "quizzer"          "quizzers" 
##                                                             
##           "quizzes"          "quizzing"              "quod" 
##                                                             
##             "quods"             "quoin"           "quoined" 
##                                                             
##          "quoining"            "quoins"             "quoit" 
##                                                             
##           "quoited"          "quoiting"            "quoits" 
##                                                             
##           "quomodo"          "quomodos"           "quondam" 
##                                                             
##            "quorum"           "quorums"             "quota" 
##                                                             
##          "quotable"          "quotably"            "quotas" 
##                                                             
##         "quotation"        "quotations"             "quote" 
##                                                             
##            "quoted"            "quoter"           "quoters" 
##                                                             
##            "quotes"             "quoth"            "quotha" 
##                                                             
##          "quotient"         "quotients"           "quoting" 
##                                                             
##             "qursh"           "qurshes"            "qurush" 
##                                                             
##          "qurushes"           "racquet"          "racquets" 
##                                                             
##          "ramequin"         "ramequins"        "reacquaint" 
##                                                             
##      "reacquainted"     "reacquainting"       "reacquaints" 
##                                                             
##         "reacquire"        "reacquired"        "reacquires" 
##                                                             
##       "reacquiring"         "reconquer"       "reconquered" 
##                                                             
##      "reconquering"        "reconquers"        "reconquest" 
##                                                             
##       "reconquests"           "reequip"        "reequipped" 
##                                                             
##       "reequipping"          "reequips"        "relinquish" 
##                                                             
##      "relinquished"      "relinquishes"     "relinquishing" 
##                                                             
##    "relinquishment"   "relinquishments"           "relique" 
##                                                             
##          "reliques"          "remarque"         "remarques" 
##                                                             
##           "request"         "requested"        "requesting" 
##                                                             
##          "requests"           "requiem"          "requiems" 
##                                                             
##            "requin"           "requins"           "require" 
##                                                             
##          "required"       "requirement"      "requirements" 
##                                                             
##          "requirer"         "requirers"          "requires" 
##                                                             
##         "requiring"         "requisite"        "requisites" 
##                                                             
##       "requisition"     "requisitioned"    "requisitioning" 
##                                                             
##      "requisitions"          "requital"         "requitals" 
##                                                             
##           "requite"          "requited"          "requiter" 
##                                                             
##         "requiters"          "requites"         "requiting" 
##                                                             
##            "risque"             "roque"            "roques" 
##                                                             
##            "roquet"          "roqueted"         "roqueting" 
##                                                             
##           "roquets"           "rorqual"          "rorquals" 
##                                                             
##            "sacque"           "sacques"          "seaquake" 
##                                                             
##         "seaquakes"            "sequel"           "sequela" 
##                                                             
##          "sequelae"           "sequels"          "sequence" 
##                                                             
##         "sequenced"         "sequences"        "sequencies" 
##                                                             
##        "sequencing"          "sequency"           "sequent" 
##                                                             
##        "sequential"      "sequentially"          "sequents" 
##                                                             
##         "sequester"       "sequestered"      "sequestering" 
##                                                             
##        "sequesters"            "sequin"          "sequined" 
##                                                             
##           "sequins"          "sequitur"         "sequiturs" 
##                                                             
##           "sequoia"          "sequoias"           "siliqua" 
##                                                             
##          "siliquae"           "silique"          "siliques" 
##                                                             
##       "soliloquize"      "soliloquized"      "soliloquizes" 
##                                                             
##     "soliloquizing"         "soliloquy"        "soliloquys" 
##                                                             
##          "soliquid"         "soliquids"             "squab" 
##                                                             
##         "squabbier"        "squabbiest"          "squabble" 
##                                                             
##         "squabbled"         "squabbles"        "squabbling" 
##                                                             
##           "squabby"            "squabs"             "squad" 
##                                                             
##          "squadded"         "squadding"          "squadron" 
##                                                             
##        "squadroned"       "squadroning"         "squadrons" 
##                                                             
##            "squads"          "squalene"         "squalenes" 
##                                                             
##           "squalid"         "squalider"        "squalidest" 
##                                                             
##            "squall"          "squalled"          "squaller" 
##                                                             
##         "squallers"         "squallier"        "squalliest" 
##                                                             
##         "squalling"           "squalls"           "squally" 
##                                                             
##           "squalor"          "squalors"            "squama" 
##                                                             
##           "squamae"          "squamate"          "squamose" 
##                                                             
##          "squamous"          "squander"        "squandered" 
##                                                             
##       "squandering"         "squanders"            "square" 
##                                                             
##           "squared"          "squarely"           "squarer" 
##                                                             
##          "squarers"           "squares"          "squarest" 
##                                                             
##          "squaring"          "squarish"            "squash" 
##                                                             
##          "squashed"          "squasher"         "squashers" 
##                                                             
##          "squashes"         "squashier"        "squashiest" 
##                                                             
##         "squashing"           "squashy"             "squat" 
##                                                             
##           "squatly"            "squats"          "squatted" 
##                                                             
##          "squatter"        "squattered"       "squattering" 
##                                                             
##         "squatters"         "squattest"         "squattier" 
##                                                             
##        "squattiest"         "squatting"           "squatty" 
##                                                             
##             "squaw"            "squawk"          "squawked" 
##                                                             
##          "squawker"         "squawkers"         "squawking" 
##                                                             
##           "squawks"            "squaws"            "squeak" 
##                                                             
##          "squeaked"          "squeaker"         "squeakers" 
##                                                             
##         "squeakier"        "squeakiest"         "squeaking" 
##                                                             
##           "squeaks"           "squeaky"            "squeal" 
##                                                             
##          "squealed"          "squealer"         "squealers" 
##                                                             
##         "squealing"           "squeals"         "squeamish" 
##                                                             
##          "squeegee"         "squeegeed"       "squeegeeing" 
##                                                             
##         "squeegees"           "squeeze"          "squeezed" 
##                                                             
##          "squeezer"         "squeezers"          "squeezes" 
##                                                             
##         "squeezing"             "squeg"          "squegged" 
##                                                             
##         "squegging"            "squegs"           "squelch" 
##                                                             
##         "squelched"         "squelches"        "squelchier" 
##                                                             
##       "squelchiest"        "squelching"          "squelchy" 
##                                                             
##             "squib"          "squibbed"         "squibbing" 
##                                                             
##            "squibs"             "squid"          "squidded" 
##                                                             
##         "squidding"            "squids"          "squiffed" 
##                                                             
##           "squiffy"          "squiggle"         "squiggled" 
##                                                             
##         "squiggles"        "squigglier"       "squiggliest" 
##                                                             
##        "squiggling"          "squiggly"          "squilgee" 
##                                                             
##         "squilgeed"       "squilgeeing"         "squilgees" 
##                                                             
##            "squill"           "squilla"          "squillae" 
##                                                             
##          "squillas"           "squills"           "squinch" 
##                                                             
##         "squinched"         "squinches"        "squinching" 
##                                                             
##         "squinnied"         "squinnier"         "squinnies" 
##                                                             
##        "squinniest"           "squinny"        "squinnying" 
##                                                             
##            "squint"          "squinted"          "squinter" 
##                                                             
##         "squinters"         "squintest"         "squintier" 
##                                                             
##        "squintiest"         "squinting"           "squints" 
##                                                             
##           "squinty"            "squire"           "squired" 
##                                                             
##          "squireen"         "squireens"           "squires" 
##                                                             
##          "squiring"          "squirish"            "squirm" 
##                                                             
##          "squirmed"          "squirmer"         "squirmers" 
##                                                             
##         "squirmier"        "squirmiest"         "squirming" 
##                                                             
##           "squirms"           "squirmy"          "squirrel" 
##                                                             
##        "squirreled"       "squirreling"       "squirrelled" 
##                                                             
##      "squirrelling"         "squirrels"            "squirt" 
##                                                             
##          "squirted"          "squirter"         "squirters" 
##                                                             
##         "squirting"           "squirts"            "squish" 
##                                                             
##          "squished"          "squishes"         "squishier" 
##                                                             
##        "squishiest"         "squishing"           "squishy" 
##                                                             
##           "squoosh"         "squooshed"         "squooshes" 
##                                                             
##        "squooshing"            "squush"          "squushed" 
##                                                             
##          "squushes"         "squushing"        "statuesque" 
##                                                             
##     "subequatorial"        "subsequent"      "subsequently" 
##                                                             
##         "technique"        "techniques"           "tequila" 
##                                                             
##          "tequilas"             "toque"            "toques" 
##                                                             
##            "toquet"           "toquets"          "torquate" 
##                                                             
##            "torque"           "torqued"           "torquer" 
##                                                             
##          "torquers"           "torques"         "torqueses" 
##                                                             
##          "torquing"        "tourniquet"       "tourniquets" 
##                                                             
##          "tranquil"        "tranquiler"       "tranquilest" 
##                                                             
##     "tranquilities"       "tranquility"       "tranquilize" 
##                                                             
##      "tranquilized"      "tranquilizer"     "tranquilizers" 
##                                                             
##      "tranquilizes"     "tranquilizing"       "tranquiller" 
##                                                             
##      "tranquillest"    "tranquillities"      "tranquillity" 
##                                                             
##      "tranquillize"     "tranquillized"     "tranquillizer" 
##                                                             
##    "tranquillizers"     "tranquillizes"    "tranquillizing" 
##                                                             
##        "tranquilly"             "tuque"            "tuques" 
##                                                             
##          "turquois"         "turquoise"        "turquoises" 
##                                                             
##            "ubique"        "ubiquities"      "ubiquitities" 
##                                                             
##        "ubiquitity"        "ubiquitous"      "ubiquitously" 
##                                                             
##          "ubiquity"           "unequal"         "unequaled" 
##                                                             
##         "unequally"          "unequals"       "unequivocal" 
##                                                             
##     "unequivocally"            "unique"          "uniquely" 
##                                                             
##        "uniqueness"           "uniquer"           "uniques" 
##                                                             
##          "uniquest"       "unqualified"    "unquantifiable" 
##                                                             
##      "unquenchable"    "unquestionable"    "unquestionably" 
##                                                             
##     "unquestioning"           "unquiet"         "unquieter" 
##                                                             
##        "unquietest"          "unquiets"           "unquote" 
##                                                             
##          "unquoted"          "unquotes"         "unquoting" 
##                                                             
##        "unrequited"          "usquabae"         "usquabaes" 
##                                                             
##             "usque"          "usquebae"         "usquebaes" 
##                                                             
##            "usques"          "vanquish"        "vanquished" 
##                                                             
##        "vanquishes"       "vanquishing"           "vaquero" 
##                                                             
##          "vaqueros"     "ventriloquism"    "ventriloquisms" 
##                                                             
##     "ventriloquist"    "ventriloquists"       "ventriloquy" 
##                                                             
##      "ventriloquys"            "yanqui"           "yanquis" 
##                   2                   3                   4 
##                "85"               "908"              "3686" 
##                   5                   6                   7 
##              "8258"             "14374"             "21727" 
##                   8                   9                  10 
##             "26447"             "16658"              "9199" 
##                  11                  12                  13 
##              "5296"              "3166"              "1960" 
##                  14                  15                  16 
##              "1023"               "557"               "261" 
##                  17                  18                  19 
##               "132"                "48"                "16" 
##                  20                  21                     
##                 "5"                 "3"              "6557" 
##                                                             
##              "6848"             "10385"              "6436" 
##                                                             
##              "4364"              "4937"              "3950" 
##                                                             
##              "4080"              "4013"              "1106" 
##                                                             
##              "1312"              "3710"              "6270" 
##                                                             
##              "2208"              "3978"              "8693" 
##                                                             
##               "568"              "7141"             "12591" 
##                                                             
##              "5951"              "2934"              "1932" 
##                                                             
##              "2927"                "82"               "438" 
##                     
##               "398"
```

