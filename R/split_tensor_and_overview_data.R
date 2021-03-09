split_tensor_and_overview_data<-function(origin_root,path,file_name,destination_root=getwd(),overwrite=FALSE)
{
    
    # Strip trailing slashes to smooth assembly
    if(nchar(destination_root)>1)
    {
        if(substr(destination_root,nchar(destination_root),nchar(destination_root))=="/")
        {
            destination_root=substr(destination_root,0,nchar(destination_root)-1)
        }
    }
    
    # Strip trailing slashes to smooth assembly
    if(nchar(origin_root)>1)
    {
        if(substr(origin_root,nchar(origin_root),nchar(origin_root))=="/")
        {
            origin_root=substr(origin_root,0,nchar(origin_root)-1)
        }
    }
    # Strip trailing and leading slashes from path
    if(nchar(path)>1)
    {
        if(substr(path,nchar(path),nchar(path))=="/")
        {
            path=substr(path,0,nchar(path)-1)
        }
    }
        
    if(nchar(path)>1)
    {
        if(substr(path,1,1)=="/")
        {
            path=substr(path,2,nchar(path))
        }
    }

    # Strip trailing and leading slashes in path
    if(nchar(origin_root)>1)
    {
        if(substr(origin_root,nchar(origin_root),nchar(origin_root))=="/")
        {
            origin_root=substr(origin_root,0,nchar(origin_root)-1)
        }
    }
    
    destination_file_header = paste(destination_root,path,paste("header",file_name,sep="_"),sep="/")
    
    if(path=="") { # no folder indicated
        destination_file_header = paste(destination_root,paste("header",file_name,sep="_"),sep="/")
    }
    
    
    destination_file_stress_tensor = paste(destination_root,path,paste("stress",
    gsub(".txt",".rda",file_name),sep="_"),sep="/")
    
    if(path=="") { # no folder indicated
        destination_file_stress_tensor = paste(destination_root,paste("stress",
        gsub(".txt",".rda",file_name),sep="_"),sep="/")
    }
    
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
    
    if(path=="") { # no folder indicated
        origin_file = paste(origin_root,file_name,sep="/")
        origin_file_short =file_name
        }
    
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
   
    invisible(stress_data)
    
}
