file_information_table_from_folder<-function(path,min_file_size=1e5)
{
    files = list.files(path,pattern=".txt")
    
    fileinfo=file.info(paste(path,files,sep="/"))
    
    files = files[(!fileinfo$isdir) & fileinfo$size>min_file_size]
    
    patterns=headerSearchPatterns()
    
    numerical_columns=c("N","packing_fraction",
    "Young_modulus_spheres","Friction_coefficient",
    "cut_lines","Strain_amplitude","baseline_pre_periods","baseline_post_periods",
    "periods","relative_transversal_link_strength","relative_viscosity","damping_factor_per_dt",
    "Gprime_Pa","Gprimeprime_Pa" ,"relative_frequency","Frequency","avoid_horizontal_angle_degree","Stress",
    "interface_reenforcement_central","Gprime_Pa_surface_force","Gprimeprime_Pa_surface_force","start_data",
    "remove_link_fraction","actual_remove_link_fraction")
    
    boolean_columns=c("do_permanent_links",
    "keep_viscosity_coefficients_constant","cut_top_bottom","doCutByTriangulation")
    
    # No corresponding files found, we still return a 0-length dataframe of the correct structure
    if(length(files)==0)
    {
        
        
        ret=matrix(nrow=0,ncol=length(patterns)+3)
        
        colnames(ret)=c(names(patterns),"start_data","File","Folder")
        
        ret=as.data.frame(ret)
        
        for(theCol in numerical_columns)
        {
            ret[,theCol]=as.numeric(as.character(ret[,theCol]))
        }
        
        for(theCol in boolean_columns)
        {
            ret[,theCol]=as.logical(as.character(ret[,theCol]))
        }

        return(ret)
        
        
        
    }
    
    fullfiles=paste(path,files,sep="/")
    
    fileinfo=file.info(fullfiles)
    
    
    
    
    
    for(ind in 1:length(fullfiles))
    {
        
        file_data=find_infos_from_file(fullfiles[ind],patterns)
        
        
        if(!is.numeric(file_data["avoid_horizontal_angle_degree"]) & file_data["avoid_horizontal_angle_degree"]==FALSE)
        {
            file_data["avoid_horizontal_angle_degree"]=0
        }
        
        if(!is.numeric(file_data["Gprime_Pa_surface_force"])
        & file_data["Gprime_Pa_surface_force"]==FALSE)
        {
            file_data["Gprime_Pa_surface_force"]="NA"
        }
        
        if(!is.numeric(file_data["Gprimeprime_Pa_surface_force"])
        & file_data["Gprimeprime_Pa_surface_force"]==FALSE)
        {
            file_data["Gprimeprime_Pa_surface_force"]="NA"
        }
        
        if(!is.numeric(file_data["relative_transversal_link_strength"]) & file_data["relative_transversal_link_strength"]==FALSE)
        {
            file_data["relative_transversal_link_strength"]=1
        }
        
        if(!is.numeric(file_data["interface_reenforcement_central"]) & file_data["interface_reenforcement_central"]==FALSE)
        {
            file_data["interface_reenforcement_central"]=1
        }
        
        if(!is.numeric(file_data["interface_reenforcement_tangential"]) & file_data["interface_reenforcement_tangential"]==FALSE)
        {
            file_data["interface_reenforcement_tangential"]=1
        }
        
        if(!is.numeric(file_data["cut_top_bottom"]) & file_data["cut_top_bottom"]==FALSE)
        {
            file_data["cut_top_bottom"]="True"
        }
        
        if(!is.numeric(file_data["doCutByTriangulation"]) & file_data["doCutByTriangulation"]==FALSE)
        {
            file_data["doCutByTriangulation"]="False"
        }
        
        if(!is.numeric(file_data["remove_link_fraction"]) & file_data["remove_link_fraction"]==FALSE)
        {
            file_data["remove_link_fraction"]=0
        }
        
        if(!is.numeric(file_data["actual_remove_link_fraction"]) & file_data["actual_remove_link_fraction"]==FALSE)
        {
            file_data["actual_remove_link_fraction"]=NA
        }
        
        
        # Also record the start line for the stress data in the file
        file_data$start_data=stress_data_start_line(fullfiles[ind])
        
        
        
        
        
        for(theCol in numerical_columns)
        {
            file_data[,theCol]=as.numeric(as.character(file_data[,theCol]))
        }
        
        
        for(theCol in boolean_columns)
        {
            if(as.character(file_data[,theCol])=="False")
            {
                file_data[,theCol]=FALSE
            } else
            {
                file_data[,theCol]=TRUE
            }
        }
        
        
        
        file_data$File=""
        
        file_data$File=files[ind]
        file_data$Folder=path
        
        
        # Find the first line for the actual data
        
        
        if(ind==1)
        {
            all_file_info=file_data
        } else
        {
            all_file_info = rbind(all_file_info,file_data)
        }
        
        
        
        
    }
    
    
    return(all_file_info)
    
    
    
    
    
    
    
    
}
