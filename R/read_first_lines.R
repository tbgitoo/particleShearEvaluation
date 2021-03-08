read_first_lines<-function(path,n=100)
{
    Tfile <- file(path,open="r")
    header=readLines(Tfile,n=n)
    close(Tfile)
    
    return(header)
    
    
}

