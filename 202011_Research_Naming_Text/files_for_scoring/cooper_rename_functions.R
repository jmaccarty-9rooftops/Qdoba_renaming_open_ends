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


# Breaking the odd data structure into 
break.them.up.fun <- function( data.in ){
    
    for( i in 1:length( q_cols ) ){
        
        out.list[[i]] <- data.in %>%
            select( q_cols[[i]] )
            
    }
    
    return( out.list )
    
}


# clean.element.list.fun <- function( data.in , )
flag.brand.fun.cart <- function( data.in , data_stack_d ){

    # Get data_1 just brand inputs
    data_1.c <- data.in %>%
        select( brand_input ) %>%
        distinct( brand_input ) %>%
        rename( brand_input_data = brand_input )
    
    # Cartesian with data_stack_d
    data_1.c1 <- expand.grid( data_1.c$brand_input_data , data_stack_d$brand_input) 
    
    # remove NA
    data_1.c1 <- data_1.c1 %>%
        na.omit()
}

# 
flag.brand.fun.append <- function( data.in , data_stack_d){
    data_1.c1 <- data.in
    data_1.c1 <- data_1.c1 %>%
        mutate( Var1 = as.character(Var1) ,
                Var2 = as.character(Var2)) %>%
        rename( brand_input_data = Var1 ,
                brand_input_lookup = Var2 )
    
    # Mutate to be if brand_input_data contains string brand_input_lookup, then flag
    tik<-Sys.time()
    data_1.c2 <- data_1.c1 %>%
        rowwise() %>%
        mutate( flag = grepl( brand_input_lookup , brand_input_data ) )
    tok<-Sys.time()
    tok-tik
    
    # Keep it if true
    data_1.c3 <- data_1.c2 %>%
        filter( flag == TRUE ) %>%
        arrange( brand_input_data )
    
    # Now left join the distinct brand and their flag to the brand_input_lookup
    data_1.c4 <- data_1.c3 %>%
        left_join( data_stack_d , by=c("brand_input_lookup"="brand_input") ) %>%
        mutate( char_n = nchar(brand_input_lookup) ) %>%
        filter( char_n > 3 ) %>%
        arrange( brand_input_data )
    
    # Distinct on brand_input_Data, brand_flag
    data_1.c5 <- data_1.c4 %>%
        distinct( brand_input_data , brand_flag ) %>%
        arrange( brand_input_data )
    
    # Remove 31 and 32 when there is another answer #### NOT FINISHED
    data_1.c6 <- data_1.c5 %>%
        group_by( brand_input_data ) %>%
        mutate( row_n = row_number() )
    
    # Remove toyo flag if brand = "toyota"
    data_1.c7 <- data_1.c6 %>%
        filter( ! (grepl("toyota", brand_input_data) == T & brand_flag == 26 ) )
    
    # Cast
    data_1.c7 <- data_1.c6 %>%
        spread( row_n , brand_flag, fill="")

    # 
}




















