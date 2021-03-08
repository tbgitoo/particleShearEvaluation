stress_data_start_line<-function(path,data_header_pattern="Detailed[a-zA-Z0-9\\s].*data",n_header=200)
{
    
    
    header=read_first_lines(path,n=n_header)
    
    return(grep(data_header_pattern,header,value=FALSE,perl=TRUE))
    
    
    
    
    
}