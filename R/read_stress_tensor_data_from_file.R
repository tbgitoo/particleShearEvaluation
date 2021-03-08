read_stress_tensor_data_from_file<-function(path,data_start_line=-1,time_column="t",strain_column="strain",
strain_rate_column="strain_rate",shear_stress_measured_at_surface_column="stress_measured_at_surface",
shear_stress_from_internal_stress_tensor_column="shear_stress_internal_stress.tensor")
{
    # If not provided, read the data start line from the text file with default arguments
    if(data_start_line<0)
    {
        data_start_line=stress_data_start_line(path)
    }
    
    
    stress_tensor_data=read.table(path,header=TRUE,skip=data_start_line)
    
    
    
    return(stress_tensor_data)
        
    
    
}