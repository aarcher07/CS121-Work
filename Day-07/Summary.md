
```r
words <- readLines(url("http://dtkaplan.github.io/ScientificComputing/Syllabus/Daily/Day-07/word_list_moby_crossword-flat/word_list_moby_crossword.flat.txt"))

Summary <- function(words) {
    # 100 Longest Words
    numberofcharacters <- nchar(words)
    DescendingCharacters <- sort(numberofcharacters, decreasing = TRUE)
    longest100 <- DescendingCharacters[1:100]
    LongestWords <- words[which(numberofcharacters == longest100)]
    
    # Word Length
    wordLength <- table(numberofcharacters)
    
    
    qwords <- words[(grep("[q]", words))]
    uwords <- words[(grep("[u]", words))]
    
    dups <- duplicated(c(uwords, qwords))  #Duplicated combines the vectors and detects which words in uwords are repeated in qwords. The words that are duplicated are given a 'TRUE' value and those that are not are given 'FALSE.'
    UinQwords <- dups[(length(uwords) + 1):(length(qwords) + length(uwords))]  #Removes the uword vector from dups, leaving qwords.
    
    indexofUinQ <- which(UinQwords == "TRUE")  #Determines the indices of duplicated values
    qwordsWithoutU <- qwords[-indexofUinQ]  #Removes duplicated indices             
    
    
    # Words starting with a given letter of the alphabet
    letters <- c("^a", "^b", "^c", "^d", "^e", "^f", "^g", "^h", "^i", "^j", 
        "^k", "^l", "^m", "^n", "^o", "^p", "^q", "^r", "^s", "^t", "^u", "^v", 
        "^w", "^x", "^y", "^z")
    
    Sums <- vector(length = 26)
    for (k in 1:26) {
        lettersnew <- letters[k]
        Sums[k] <- sum(grepl(lettersnew, words))
    }
    return(c(qwordsWithoutU, LongestWords, wordLength, table(Sums)))
}


Summary(words)
```

```
## Warning: longer object length is not a multiple of shorter object length
## Warning: closing unused connection 5
## (http://dtkaplan.github.io/ScientificComputing/Syllabus/Daily/Day-07/word_list_moby_crossword-flat/word_list_moby_crossword.flat.txt)
```

```
##                                                                      
##                "faqir"               "faqirs"                 "qaid" 
##                                                                      
##                "qaids"               "qindar"              "qindars" 
##                                                                      
##               "qintar"              "qintars"                 "qoph" 
##                                                                      
##                "qophs"   "absentmindednesses"   "antiadministration" 
##                                                                      
##    "antimaterialistic"    "antimiscegenation"    "antitechnological" 
##                                                                      
##    "blameworthinesses"    "characterizations"   "chemotherapeutical" 
##                                                                      
##   "counteraccusations"   "counteraggressions"    "countercomplaints" 
##                                                                      
##    "countercriticisms"   "counterpropagation"   "counterretaliation" 
##                                                                      
##   "counterrevolutions"   "countersuggestions"    "counterterrorisms" 
##                                                                      
##    "counterterrorists"    "cyclophosphamides"  "disinterestednesses" 
##                                                                      
##   "electrocardiograms"   "electrocardiograph"   "feeblemindednesses" 
##                                                                      
##    "halfheartednesses"    "homogeneousnesses"   "hypernationalistic" 
##                                                                      
##   "hypersensitiveness"   "hypersensitivities"    "impenetrabilities" 
##                                                                      
##    "inattentivenesses"  "inconsideratenesses"    "inquisitivenesses" 
##                                                                      
##    "intergovernmental"    "internationalisms"    "internationalized" 
##                                                                      
##    "internationalizes"   "interrelatednesses"    "irreconcilability" 
##                                                                      
## "microminiaturization"   "misinterpretations"   "misrepresentations" 
##                                                                      
##    "misunderstandings"    "multidisciplinary"    "nearsightednesses" 
##                                                                      
##    "nondenominational"    "nondiscrimination"   "obstreperousnesses" 
##                                                                      
##    "picturesquenesses"   "postfertilizations"    "professionalizing" 
##                                                                      
##    "reclassifications"    "rehospitalization"   "remunerativenesses" 
##                                                                      
##    "responsiblenesses"   "simultaneousnesses"   "straightforwardest" 
##                                                                      
##    "subclassification"   "superintellectuals"   "superintelligences" 
##                                                                    2 
##    "trustworthinesses"   "unscrupulousnesses"                   "85" 
##                      3                      4                      5 
##                  "908"                 "3686"                 "8258" 
##                      6                      7                      8 
##                "14374"                "21727"                "26447" 
##                      9                     10                     11 
##                "16658"                 "9199"                 "5296" 
##                     12                     13                     14 
##                 "3166"                 "1960"                 "1023" 
##                     15                     16                     17 
##                  "557"                  "261"                  "132" 
##                     18                     19                     20 
##                   "48"                   "16"                    "5" 
##                     21                     82                    398 
##                    "3"                    "1"                    "1" 
##                    438                    568                   1106 
##                    "1"                    "1"                    "1" 
##                   1312                   1932                   2208 
##                    "1"                    "1"                    "1" 
##                   2927                   2934                   3710 
##                    "1"                    "1"                    "1" 
##                   3950                   3978                   4013 
##                    "1"                    "1"                    "1" 
##                   4080                   4364                   4937 
##                    "1"                    "1"                    "1" 
##                   5951                   6270                   6436 
##                    "1"                    "1"                    "1" 
##                   6557                   6848                   7141 
##                    "1"                    "1"                    "1" 
##                   8693                  10385                  12591 
##                    "1"                    "1"                    "1"
```

