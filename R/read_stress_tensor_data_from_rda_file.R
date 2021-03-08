read_stress_tensor_data_from_rda_file<-function(path)
{
    
    load(file=path,envir=sys.frame())
    
    return(stress_data)
        
    
    
}