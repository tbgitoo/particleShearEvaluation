evaluate_stress_curve<-function(strain,Gprime,Gprimeprime,guess_softening_strain=0.01,guess_soft_plateau_strain=0.1)
{
    # Initialize the return values
    strain_evaluated = vector(mode="numeric",length=4)
    names(strain_evaluated) = c("low_strain_plateau","softening","soft_plateau","yield")
    Gprime_evaluated = strain_evaluated
    Gprimeprime_evaluated=strain_evaluated
    
    
    strain_evaluated["low_strain_plateau"]=min(strain)
    
    Gprime_evaluated["low_strain_plateau"]=mean(Gprime[strain==strain_evaluated["low_strain_plateau"]])
    Gprimeprime_evaluated["low_strain_plateau"]=mean(Gprimeprime[strain==strain_evaluated["low_strain_plateau"]])
    
    
    # Mid-softening slope: Find the first maximum of G' which should be about where the steepest slope occurs
    
    
    index_softening_from_Gprimeprime=which.max(Gprimeprime*(strain >= guess_softening_strain/5)* ( strain <= guess_softening_strain*5) )
    
    
    
    differential=diff(Gprime)/diff(log(strain))
    
    differential=-(c(differential[1],differential)+c(differential,differential[length(differential)]))/2
    
    region_of_interest=rep(0,length(differential))
    
    region_of_interest[(1:length(region_of_interest))>=(index_softening_from_Gprimeprime-3)&
    (1:length(region_of_interest))<=(index_softening_from_Gprimeprime+3)]=1
    
    index_softening_from_Gprime=which.max(differential*region_of_interest)
    
    
    final_index_softening=(index_softening_from_Gprime+index_softening_from_Gprimeprime)/2
    
    
    strain_evaluated["softening"]=exp(approx(x=1:length(strain),y=log(strain),xout=final_index_softening)$y)
    
    Gprime_evaluated["softening"]=approx(x=1:length(strain),y=Gprime,xout=final_index_softening)$y
   
    Gprimeprime_evaluated["softening"]=approx(x=1:length(strain),y=Gprimeprime,xout=final_index_softening)$y
    
    
    Gprimeprime_for_evaluation=rep(NA,length(Gprime))
    
    selector = (strain >=  strain_evaluated["softening"]*1.5)&( strain <= guess_soft_plateau_strain*3)
    
    if(all(selector==FALSE))
    {
        m=9
        selector = (strain >=  strain_evaluated["softening"]*1.5)&( strain <= guess_soft_plateau_strain*m)
        while(m<1000 & all(selector==FALSE))
        {
            m=m*3
            selector = (strain >=  strain_evaluated["softening"]*1.5)&( strain <= guess_soft_plateau_strain*m)
        }
    }
    
    Gprimeprime_for_evaluation[selector]=Gprimeprime[selector]
    
    index_soft_plateau_from_Gprimeprime=which.min(Gprimeprime_for_evaluation)
    
    region_of_interest=rep(0,length(differential))
    
    region_of_interest[(1:length(region_of_interest))>=(index_soft_plateau_from_Gprimeprime-3)&
    (1:length(region_of_interest))<=(index_soft_plateau_from_Gprimeprime+3)]=1
    
    differential_to_evaluate=rep(NA,length(differential))
    
    differential_to_evaluate[region_of_interest==1]=differential[region_of_interest==1]
    
    index_soft_plateau_from_Gprime=which.min(differential_to_evaluate)
    
    final_index_soft_plateau=(index_soft_plateau_from_Gprime+index_soft_plateau_from_Gprimeprime)/2
    
    
    strain_evaluated["soft_plateau"]=exp(approx(x=1:length(strain),y=log(strain),xout=final_index_soft_plateau)$y)
    
    Gprime_evaluated["soft_plateau"]=approx(x=1:length(strain),y=Gprime,xout=final_index_soft_plateau)$y
    
    Gprimeprime_evaluated["soft_plateau"]=approx(x=1:length(strain),y=Gprimeprime,xout=final_index_soft_plateau)$y
    
    
    strain_evaluated["yield"]=get_yield_point(strain,Gprime,Gprimeprime)
    
    Gprime_evaluated["yield"]=approx(x=strain,y=Gprime,xout=strain_evaluated["yield"])$y
    
    Gprimeprime_evaluated["yield"]=approx(x=strain,y=Gprimeprime,xout=strain_evaluated["yield"])$y
    
    
    ret_matrix=rbind(strain_evaluated,Gprime_evaluated,Gprimeprime_evaluated)
    
    return(ret_matrix)
    
    
}

