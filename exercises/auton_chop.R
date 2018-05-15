auton = local({
  i = 0
  function() {
    i <<- i + 1
    return(paste(i, ".", sep=""))
  }
})
hashline = local({
  function(n=40) {
    return(paste0(rep('\\#',n), collapse=''))
  }
})
hashtitle = local({
  function(x, bold=TRUE, upper=TRUE, num_pre=4, num_post=4, hashborder=c(T,T)) {
    if (upper) x <- toupper(x)
    
    hashborder_str <- paste0(rep('\\#', num_pre + nchar(x) + 2 + num_post), collapse='')
    
    if (hashborder[1]) {
      line01_str <- paste0(hashborder_str, "<br/>")
    } else {
      line01_str <- ""
    }
    
    if (hashborder[2]) {
      line03_str <- hashborder_str
    } else {
      line03_str <- ""
    }
    
    line02_str <- paste0(paste0(rep('\\#', num_pre), collapse=""),
                  " ", ifelse(bold, "__", ""), x, ifelse(bold, "__", ""), " ",
                  paste0(rep('\\#', num_post), collapse=""), "<br/>")
    
    return(paste0(line01_str, line02_str, line03_str, collapse="\n"))

  }
})
chop = local({
  function(x, width=80, indent=0, exdent=0, initial="", prefix="", collapse="  \n\\# &nbsp;", nonprint_omit=TRUE, nonprint_show=FALSE) {
    x_work <- x
    bracket_matches_txt <- character(0)
    link_matches_txt <- character(0)
    braces_matches_txt <- character(0)
    
    if (nonprint_omit) {
      ## Find angle-bracket tags that don't have any spaces
      ## bracket_pattern <- "<(.*?)>"
      ## This works ok but it doesn't remove nested tags, beware
      bracket_pattern <- "<[[:alnum:][:punct:]]*?>"   
      bracket_pos <- gregexpr(bracket_pattern, x_work)
      bracket_at_least_one_match <- (bracket_pos[[1]][1] > 0)
      if (bracket_at_least_one_match) {
        ## Get the text
        bracket_matches_txt <- regmatches(x_work, bracket_pos)
        ## Replace with placeholder
        x_work <- gsub(bracket_pattern, "\1",  x_work)
      }
      
      ## Find a markdown link pattern [The News](http://news.com)
      link_pattern <- "\\([[:alnum:][:punct:]]*?\\)"
      link_pos <- gregexpr(link_pattern, x_work)
      link_at_least_one_match <- (link_pos[[1]][1] > 0)
      
      if (link_at_least_one_match > 0) {
        ## Get the strings(s) to insert back in later
        link_matches_txt <- regmatches(x_work, link_pos)
        
        ## Replace with a non-printing placeholder '\2' that
        ## we'll replace after chopping
        x_work <- gsub(link_pattern, "\2",  x_work)
      }
      
      ## Find curly-braces tags
      braces_pattern <- "\\{target[[:alnum:][:punct:]]*?\\}"
      braces_pos <- gregexpr(braces_pattern, x_work)
      braces_at_least_one_match <- (braces_pos[[1]][1] > 0)
      if (braces_at_least_one_match) {
        ## Get the text(s)
        braces_matches_txt <- regmatches(x_work, braces_pos)
        ## Replace with \3 placeholder
        x_work <- gsub(braces_pattern, "\3",  x_work)
      }
    }
    
    ## Wrap the text
    x_out <- paste(strwrap(x_work, width=width, indent=indent, initial=initial, prefix=prefix, exdent=exdent), collapse=collapse, sep="")

    if (nonprint_show) {
      print(x_out)  
    }
    
    if (nonprint_omit) {
      ## Replace bracket matches
      if (bracket_at_least_one_match) {
        for (i in 1:length(bracket_matches_txt[[1]])) {
          x_out <- sub("\1", bracket_matches_txt[[1]][i],  x_out)
        }
      }
      
      ## Replace link matches
      if (link_at_least_one_match) {
        for (i in 1:length(link_matches_txt[[1]])) {
          x_out <- sub("\2", link_matches_txt[[1]][i],  x_out)
        }
      }
      
      ## Replace link matches
      if (braces_at_least_one_match) {
        for (i in 1:length(braces_matches_txt[[1]])) {
          x_out <- sub("\3", braces_matches_txt[[1]][i],  x_out)
        }
      }
    }
    
    return(x_out)  
  }  

  
  
})


