file_information_table_from_folders<-function(paths,min_file_size=1e5)
{
    
    for(ind in 1:length(paths))
    {
        path=paths[ind]
        file_table=file_information_table_from_folder(path,min_file_size=min_file_size)
        
        if(ind==1)
        {
            all_tables=file_table
        } else
        {
            all_tables=rbind(all_tables,file_table)
        }
        
    }
    
    return(all_tables)
    
}