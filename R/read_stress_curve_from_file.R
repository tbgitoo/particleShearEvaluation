read_stress_curve_from_file<-function(path,data_start_line=-1,time_column="t",strain_column="strain",
strain_rate_column="strain_rate",shear_stress_measured_at_surface_column="stress_measured_at_surface",
shear_stress_from_internal_stress_tensor_column="shear_stress_internal_stress.tensor")
{
    
    
    stress_tensor_data=read_stress_tensor_data_from_file(path,data_start_line=data_start_line)
    
    
    return(data.frame(
        t=stress_tensor_data[,time_column],
        strain=stress_tensor_data[,strain_column],
        strain_rate=stress_tensor_data[,strain_rate_column],
        shear_stress_measured_at_surface=stress_tensor_data[,shear_stress_measured_at_surface_column],
        shear_stress_from_internal_stress_tensor=stress_tensor_data[,
            shear_stress_from_internal_stress_tensor_column]))
        
    
    
}