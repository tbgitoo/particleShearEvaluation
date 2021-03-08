split_tensor_and_overview_data<-function(origin_root,path,file_name,destination_root,overwrite=FALSE)
{
    
    
    destination_file_header = paste(destination_root,path,paste("header",file_name,sep="_"),sep="/")
    
    destination_file_stress_tensor = paste(destination_root,path,paste("stress",
    gsub(".txt",".rda",file_name),sep="_"),sep="/")
    
    # If no overwrite is desired, we should
    if(!overwrite)
    {
        if(file.exists(destination_file_header) & file.exists(destination_file_stress_tensor))
        {
                return(FALSE)
        }
    }
    
    origin_file = paste(origin_root,path,file_name,sep="/")
    origin_file_short =paste(path,file_name,sep="/")
    overview_data = read_general_data_from_file(origin_file)
    
    
    
    stress_data =read_stress_tensor_data_from_file(origin_file)
    
    
    if(overwrite | !file.exists(destination_file_header))
    {
    Tfile <- file(destination_file_header,open="w")
    writeLines(overview_data,Tfile)
    close(Tfile)
    }
        
    origin_file=origin_file_short
    
    if(overwrite | !file.exists(destination_file_stress_tensor))
    {
        
        save(file=destination_file_stress_tensor,list=c("stress_data","origin_file","origin_root"))
    
    }
   
    return(stress_data)
    
}