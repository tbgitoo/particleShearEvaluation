measurement_periods<-function(shear_stress_data,baseline_pre_periods,periods,Frequency)
{
    N_t = dim(shear_stress_data)[1]
    
    measurement=factor(rep("oscillatory_measurement",N_t),levels=c("pre","initiation","oscillatory_measurement","post"))
    
    measurement[shear_stress_data$t<baseline_pre_periods/Frequency]="pre"
    measurement[shear_stress_data$t>(baseline_pre_periods+periods)/Frequency]="post"
    
    if (periods>1)
    {
        measurement[shear_stress_data$t>=baseline_pre_periods/Frequency &
        shear_stress_data$t<(baseline_pre_periods+1)/Frequency]="initiation"
    }
    
    
    return(measurement)
    
    
}

