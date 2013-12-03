
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




