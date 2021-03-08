find_info_from_text<-function(txt,pattern)
{
    
    found=grep(pattern,txt,value=TRUE,perl=TRUE)
    if(length(found)==0)
    {
        return(FALSE)
    }
    
    found=found[1]
    pos=regexpr(pattern,found,perl=TRUE)
    
    
    return(substr(found,c(attr(pos,"capture.start")),-1+c(attr(pos,"capture.start"))+c(attr(pos,"capture.length"))))
    
    
    
    
}