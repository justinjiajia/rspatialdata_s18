auton = local({
  i = 0
  function() {
    i <<- i + 1
    return(paste(i, ".", sep=""))
  }
})
chop = local({
  function(x, width=100, indent=0, exdent=0, initial="", prefix="", collapse="  \n\\# &nbsp;", nonprint_omit=TRUE) {
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
      
      ## Find a link pattern
      link_pattern <- "\\([[:alnum:][:punct:]]*?\\)"
      link_pos <- gregexpr(link_pattern, x_work)
      link_at_least_one_match <- (link_pos[[1]][1] > 0)
      
      if (link_at_least_one_match > 0) {
        ## Get the text to insert back in later
        link_matches_txt <- regmatches(x_work, link_pos)
        link_matches_txt
        ## Replace with placeholder
        x_work <- gsub(link_pattern, "\2",  x_work)
      }
      
      ## Find curly-braces tags
      braces_pattern <- "\\{target[[:alnum:][:punct:]]*?\\}"
      braces_pos <- gregexpr(braces_pattern, x_work)
      braces_at_least_one_match <- (braces_pos[[1]][1] > 0)
      if (braces_at_least_one_match) {
        ## Get the text
        braces_matches_txt <- regmatches(x_work, braces_pos)
        ## Replace with placeholder
        x_work <- gsub(braces_pattern, "\3",  x_work)
      }
    }
    
    ## Wrap the text
    x_out <- paste(strwrap(x_work, width=width, indent=indent, initial=initial, prefix=prefix, exdent=exdent), collapse=collapse, sep="")
    
    #print("ok"); browser()
    print(x_out)
    
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


