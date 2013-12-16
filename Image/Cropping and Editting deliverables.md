
```r
ShowImage <- function(image) {
    size <- dim(image)
    canvas(x = c(1, size[2]), y = c(1, size[1]), asp = 1)  #The Column Number goes on the x axis, the row number goes on the y axis
    rasterImage(image, 1, 1, size[2], size[1])  # 1 and 1 represents where the plot starts on the row and column 
}
```



```r
ShowImage(puppy)
```

```
## Error: object 'puppy' not found
```

```r
ShowImage(puppy[30:100, 90:200, ])  #The dog's head
```

```
## Error: object 'puppy' not found
```

```r
ShowImage(puppy[160:198, 1:40, ])  #Left Paw
```

```
## Error: object 'puppy' not found
```

```r
ShowImage(puppy[110:140, 90:120, ])  #Octagonal Dog Tag
```

```
## Error: object 'puppy' not found
```

```r

red <- puppy[, , 1]
```

```
## Error: object 'puppy' not found
```

```r
pair <- cbind(red, red)  #This combines the two red plane array's together by the column, creating double image.
```

```
## Error: object 'red' not found
```

```r
ShowImage(pair)
```

```
## Error: object 'pair' not found
```

```r

framed <- cbind(red[, 1:20], red, red[, 197:216])  #Borders the red with the two end columns of the red image
```

```
## Error: object 'red' not found
```

```r
ShowImage(framed)
```

```
## Error: object 'framed' not found
```

```r

reverse <- cbind(red[, rev(1:20)], red, red[, rev(197:216)])  #Borders the image with the reverse of the  two end columns of the red images
```

```
## Error: object 'red' not found
```

```r
ShowImage(reverse)
```

```
## Error: object 'reverse' not found
```

```r

all4 <- rbind(reverse[rev(1:20), ], reverse, reverse[rev(179:198), ])  #Borders the reverse image with the two end rows of image
```

```
## Error: object 'reverse' not found
```

```r

ShowImage(all4)
```

```
## Error: object 'all4' not found
```



```r

ImagePlot <- function(image, width, mirrorimage = T) {
    
    size <- dim(image)
    
    end2 <- size[2] - width + 1
    if (mirrorimage) {
        framed <- cbind(image[, rev(1:width)], image, image[, rev(end2:size[2])])
        
        end <- size[1] - width + 1
        
        framed2 <- rbind(framed[rev(1:width), ], framed, framed[rev(end:size[1]), 
            ])
        
        ShowImage(framed2)
    } else {
        framed <- cbind(image[, (1:width)], image, image[, (end2:size[2])])
        
        end <- size[1] - width + 1
        
        framed2 <- rbind(framed[(1:width), ], framed, framed[(end:size[1]), 
            ])
        
        ShowImage(framed2)
    }
    
}
```



```r
Jamaica <- readJPEG(getURLContent("http://www.sandals.com/assets/img/sng/highlight-photo-01.jpg"))
```

```
## Error: could not find function "readJPEG"
```

```r
ImagePlot(Jamaica[, , 3], 20, F)
```

```
## Error: object 'Jamaica' not found
```



```r
red <- Jamaica[, , 1]
```

```
## Error: object 'Jamaica' not found
```

```r
green <- Jamaica[, , 2]
```

```
## Error: object 'Jamaica' not found
```

```r
blue <- Jamaica[, , 3]
```

```
## Error: object 'Jamaica' not found
```

```r


weirdPuppy <- array(c(green, blue, red, alpha), dim = c(dim(red), 3))
```

```
## Error: object 'green' not found
```

```r
canvas(x = c(1, 216), y = c(1, 198), asp = 1)
```

```
## Error: could not find function "canvas"
```

```r
rasterImage(weirdPuppy, 1, 1, 216, 198)
```

```
## Error: object 'weirdPuppy' not found
```



```r

ColourFrame <- function(image, width) {
    
    size <- dim(image)
    
    end2 <- size[2] - width + 1
    
    end <- size[1] - width + 1
    
    
    canvas(x = c(1, size[2]), y = c(1, size[1]), asp = 1)
    
    Array <- array(dim = c(2 * width + size[1], 2 * width + size[2], 3))
    for (k in 1:3) {
        picture <- image[, , k]
        
        framed <- cbind(picture[, (1:width)], picture, picture[, (end2:size[2])])
        
        framed2 <- rbind(framed[(1:width), ], framed, framed[(end:size[1]), 
            ])
        
        Array[, , k] <- framed2
        
    }
    rasterImage(Array, 1, 1, size[2], size[1])
}
```



```r
ColourFrame(Jamaica, 50)
```

```
## Error: object 'Jamaica' not found
```
