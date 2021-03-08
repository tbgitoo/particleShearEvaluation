find_infos_from_file<-function(path,patterns)
{
    
    header=read_first_lines(path,n=100)
    
    
    
    ret=patterns
    ret[]=""
    for(thePatternInd in 1:length(patterns))
    {
        thePattern=patterns[thePatternInd]
        ret[thePatternInd]=find_info_from_text(header,thePattern)
    }
    
    return (as.data.frame(as.list(ret)))
    
    
    
    
    
}