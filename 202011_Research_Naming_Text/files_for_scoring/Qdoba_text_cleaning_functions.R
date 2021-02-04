# Function to strip extra white space, lowercase, stem, stopwords
clean.text <- function(data.fun, 
                       links.out = T,
                       lower.all = T,
                       stop.words.out = F,
                       stop.exceptions = c(""),
                       stem.words.out = F,
                       punct.out = T,
                       numb.out = T,
                       white.space.out = T,
                       rt.begin.out = F,
                       singles.out = F,
                       beg.end.space = T,
                       pictwitter.remove = F){
    
    # Test Arguments
    # if(is.character(data.fun)){
    # }else{
    #     stop("Bro, the data.fun column needs to be a character array")
    # }
    
    # Remove emojis (not final solution, but for now)
    data.fun <- iconv(data.fun, "latin1", "ASCII", sub="")
    
    # Processing
    if(links.out == T){
        data.fun <- gsub("http[A-Za-z0-9.!?\\/-:]*", "", data.fun)  #take links out
        # cat("\n Removed links")
    }
    if(lower.all == T){
        data.fun <- tolower(data.fun)                               #make everything lowercase
        # cat("\n Lowered All")
    }
    if(stop.words.out == T){
        if(length(stop.exceptions) == 1 & stop.exceptions[1] == ""){
            data.fun <- tm::removeWords(data.fun, tm::stopwords("en"))#remove common useless words
        }else{
            stop.words.new <- tm::stopwords("en")[!tm::stopwords("en") %in% stop.exceptions]
            data.fun <- tm::removeWords(data.fun, stop.words.new) #exceptions
        }
        # cat("\n Removed stop words")
    }
    if(stem.words.out == T){
        data.fun <- tm::stemDocument(data.fun)                              #stem words
        # cat("\n Stemmed words")
    }
    if(punct.out == T){
        data.fun <- gsub("[[:punct:]]", "", data.fun)                       #remove punctuation
        # cat("\n Removed punctuation")
    }
    if(numb.out == T){
        data.fun <- gsub("[[:digit:]]", "", data.fun)                       #remove numbers
        # cat("\n Removed numbers")
    }
    if(white.space.out == T){
        data.fun <- tm::stripWhitespace(data.fun)                           #strip out superfluous white space
        # cat("\n Stripped Whitespace")
    }
    if(rt.begin.out == T){
        data.fun <- gsub("^rt |^rts | rt$| rts$| rt | rts ", "", data.fun)                         #strip out rts from twitter
        # cat("\n Removed 'RT's")
    }
    if(singles.out == T){
        data.fun <- gsub("^[A-Za-z] | [A-Za-z] | [A-Za-z]$", "", data.fun)  #remove single letter words 
    }
    if(pictwitter.remove == T){
        data.fun <- gsub("pictwitter.*", "", data.fun)
    }
    if(beg.end.space == T){
        data.fun <- gsub("^ | $", "", data.fun)                             #remove beginning and ending blanks
    }
    
    
    # Return    
    return(data.fun)
}



















