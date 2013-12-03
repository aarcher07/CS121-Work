
```r
formatWord <- function(word, translation, class) {
    if (is.na(translation)) {
        return(paste("<span class=", "'", class, "'>", word, "</span>", sep = ""))
    } else {
        return(paste("<span class=", "'", class, "'", " title='", translation, 
            "'>", word, "</span>", sep = ""))
    }
}
```



```r
formatWord("Hello", "hi there!", class = "hiword")
```

```
## [1] "<span class='hiword' title='hi there!'>Hello</span>"
```



```r
cat("<style> .hiword {background:pink;} </style>", formatWord("Hello", "hi there!", 
    class = "hiword"), "to", "all", "of", "you", "in", formatWord("Television Land.", 
    "TV viewers", class = "hiword"))
```

<style> .hiword {background:pink;} </style> <span class='hiword' title='hi there!'>Hello</span> to all of you in <span class='hiword' title='TV viewers'>Television Land.</span>



```r
formatWord <- function(word, translation, class) {
    if (is.na(translation)) {
        return(paste("<span class", "=", "'", class, "'>", word, "</span>", 
            sep = ""))
    } else {
        return(paste("<span class=", "'", class, "'", "title='", translation, 
            "'>", word, "</span>", sep = ""))
    }
}
```



```r

formatWord <- function(word, translation, class) {
    if (is.na(translation)) {
        return(paste("<span class", "=", "'", class, "'>", word, "</span>", 
            sep = ""))
    } else {
        return(paste("<span class", "=", "'", class, "'", " title='", translation, 
            "'>", word, "</span>", sep = ""))
    }
}
```



```r
words <- c("who", "is", "the", "cow", "?")
tips <- c("Someone", NA, "article", "noun", "interrogation")
styles <- c("hiword", "", "color1", "color2", "hiword")

cat("<style> .hiword {background:pink;}  .color1{color:green;} .color2{color:orange;}  </style>", 
    formatWord(words[1], tips[1], styles[1]), formatWord(words[2], tips[2], 
        styles[2]), formatWord(words[3], tips[3], styles[3]), formatWord(words[4], 
        tips[4], styles[4]), formatWord(words[5], tips[5], styles[5]))
```

<style> .hiword {background:pink;}  .color1{color:green;} .color2{color:orange;}  </style> <span class='hiword' title='Someone'>who</span> <span class=''>is</span> <span class='color1' title='article'>the</span> <span class='color2' title='noun'>cow</span> <span class='hiword' title='interrogation'>?</span>

    

```r
cat("<style> .big {font-family:'Brush Script MT';
font-size:30px;} .small {font-family:'Brush Script MT';
font-size:10px;} </style>",   
    
    "<p class='big'> When </p>  <p class='small'> in the course of human events, it becomes necessary for one people to dissolve the political bands which have connected them with another, and to assume among the powers of the earth the separate and equal station to which the Laws of Nature and of Natures God entitle them, a decent respect to the opinions of mankind requires that they should declare the causes which impel them to the separation.
We hold these truths to be self-evident: that all men are created equal, that they are endowed by their Creator with certain unalienable Rights, that among these are Life, Liberty, and the pursuit of Happiness. </p>")
```

<style> .big {font-family:'Brush Script MT';
font-size:30px;} .small {font-family:'Brush Script MT';
font-size:10px;} </style> <p class='big'> When </p>  <p class='small'> in the course of human events, it becomes necessary for one people to dissolve the political bands which have connected them with another, and to assume among the powers of the earth the separate and equal station to which the Laws of Nature and of Natures God entitle them, a decent respect to the opinions of mankind requires that they should declare the causes which impel them to the separation.
We hold these truths to be self-evident: that all men are created equal, that they are endowed by their Creator with certain unalienable Rights, that among these are Life, Liberty, and the pursuit of Happiness. </p>

