find_info_from_file<-function(path,pattern,n=100)
{
    Tfile <- file(path,open="r")
    header=readLines(Tfile,n=n)
    close(Tfile)
    return(find_info_from_text(header,pattern))
    
    
    
    
}