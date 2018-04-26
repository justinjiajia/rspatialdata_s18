auton = local({
  i = 0
  function() {
    i <<- i + 1
    return(paste(i, ".", sep=""))
  }
})
chop = local({
  function(x, width=100, indent=0, exdent=0, initial="", prefix="", collapse="  \n\\# &nbsp;&nbsp;&nbsp;&nbsp;") {
    return(   paste(strwrap(x, width=width, indent=indent, initial=initial, prefix=prefix, exdent=exdent), collapse=collapse, sep="")   )
  }
})
