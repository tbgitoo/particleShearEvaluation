get_yield_point<-function(strain,Gprime,Gprimeprime)
{
    
    ord=order(strain,decreasing=TRUE)
    
    strain=strain[ord]
    Gprime=Gprime[ord]
    Gprimeprime=Gprimeprime[ord]
    
    
    ind_first_solid = min(which(Gprime>Gprimeprime))
    
    
    if(ind_first_solid==1)
    {
        return(strain[ind_first_solid])
        
    }
    if(ind_first_solid==Inf)
    {
        return(NA)
    }
    if(ind_first_solid>1)
    {
        difference_solid=Gprime[ind_first_solid]-Gprimeprime[ind_first_solid]
        difference_liquid=Gprime[ind_first_solid-1]-Gprimeprime[ind_first_solid-1]
        
        difference_strain = strain[ind_first_solid]-strain[ind_first_solid-1]
        
        # linear interpolation
        
        return(strain[ind_first_solid-1]-difference_liquid/(difference_solid-difference_liquid)*difference_strain)
        
        
        
    }
    
    
    
}