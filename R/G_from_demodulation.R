G_from_demodulation<-function(t,Frequency,m,correct_for_jumps=TRUE,Strain_amplitude=1,relative_threshold=0.15,doPlot=FALSE,plot.new=TRUE,max_plot=400,...)
{
    
    
    
    t=t-t[1]
    
    in_phase_signal = sin(t*2*pi*Frequency)
    out_of_phase_signal = cos(t*2*pi*Frequency)
    
    
   
    
    # correction for jumps
    
    if (correct_for_jumps)
    {
        threshold = relative_threshold*(max(m)-min(m))
        
        large_step_indices = which(abs(diff(m))>threshold)
        
        for(l in large_step_indices)
        {
            if(l>1 & l<length(m))
            {
                pre_delta=m[l]-m[l-1]
                
                m[(l+1):length(m)]=m[(l+1):length(m)]-(m[l+1]-m[l])+pre_delta
            }
        }
    }
    
    if(doPlot)
    {
        if(plot.new)
        {
            plot(t,m,...)
        } else {
            args = list(...)
            if(is.null(args$type))
            {
                args$type="l"
            }
            
            
            if(!is.na(max_plot) & max_plot<length(t))
            {
                idx=round(seq(from=1,to=length(t),length.out=max_plot))
                args$x=t[idx]
                args$y=m[idx]
            } else {
                args$x=t
                args$y=m
            }
            
            do.call(lines,args)
        }
        
    }
    
    
    G_prime = sum(in_phase_signal*m)/(sum(in_phase_signal*in_phase_signal))/Strain_amplitude
    
    G_prime_prime = sum(out_of_phase_signal*m)/(sum(out_of_phase_signal*out_of_phase_signal))/Strain_amplitude
    
    
    ret=c(G_prime,G_prime_prime)
    
    names(ret)=c("Gprime","Gprimeprime")
    
    
    return(ret)
    
    
    
    
    
    
}

