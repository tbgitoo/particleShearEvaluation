general_linear_regression_p_bootstrap<-function(x,y,n_agg=5,family=gaussian(link = "identity"),...)
{
    
    N=floor((dim(y)[2])/n_agg)
    t=vector(mode="numeric",length=N)
    df=vector(mode="numeric",length=N)
    r2=vector(mode="numeric",length=N)
    p_shapiro=vector(mode="numeric",length=N)
    
    estimate=vector(mode="numeric",length=N)
    
    for(ind_group in 1:N)
    {
        theData = data.frame(x=vector(mode="numeric",length=0),y=vector(mode="numeric",length=0))
        for(ind_condition in 1:length(x))
        {
            current_y = y[ind_condition,(ind_group-1)*n_agg+(1:n_agg)]
            theData=rbind(theData,data.frame(x=rep(x[ind_condition],length(current_y)),y=current_y))
        }
        
        linmod=glm(y~x,theData,family=family,...)
        residuals=resid(linmod)
        if(length(unique(residuals[!is.na(residuals)]))==1) # Shapiro gives an error when all values are identical. Report 1 in this case
        {
            p_shapiro[ind_group]=1
        } else {
            p_shapiro[ind_group]=shapiro.test(resid(linmod))[["p.value"]]
        }
        
        s=summary(linmod)
        t[ind_group]=coefficients(s)["x","t value"]
        df[ind_group]=s[["df"]][2]
        
        estimate[ind_group] = coefficients(linmod)[["x"]]
        
        if(!family(linmod)$family=="quasi")
        {
            r2[ind_group]=rsq(linmod,adj=TRUE)
        } else
        {
            # rsq doesn't do it so come back to the original definition =======
            # If we don't include x in the regression, we get total variability, this is
            # reported in the summary of the glm
            deviance0 = s$null.deviance
            # If we do the complete regression, the deviance becomes smaller
            deviance_residual = s$deviance
            
            # Rsquared is basically 1 - the relative remaining deviance. There is a correction for degrees
            # of freedom, so overall
            
            r2[ind_group] <- 1 - (deviance_residual/deviance0) * s$df.null/s$df.residual

            
            # =============================================
        }
    }
    
    
    return_val = pf(mean(t^2),1,mean(df),lower.tail=FALSE)
    
    # Idea here: convert p-values to z values which can be averaged. If the shapiro values
    # are uniformly distributed we can get p=0.5 on average; if there is a systematic contribution,
    # then this will lead to a true averaging around the bias.
    
    z_shapiro = mean(qnorm(p_shapiro))
    
    
    attr(return_val,"F")=mean(t^2)
    attr(return_val,"DF1")=1
    attr(return_val,"DF2")=mean(df)
    attr(return_val,"adj.r.squared")=mean(r2)
    attr(return_val,"p_shapiro")=pnorm(z_shapiro)
    attr(return_val,"confint")=c("2.5 %"=quantile(estimate,probs=c(0.025))[["2.5%"]]
    ,"97.5 %"=quantile(estimate,probs=c(0.975))[["97.5%"]])
    
    
    return (return_val)
    
    
    
    
    
}
