read_stress_tensor_data_from_rda_file<-function(path)
{
    envir=new.env()
    load(file=path,envir=envir)
    
    return(envir$stress_data)
        
    
    
}
