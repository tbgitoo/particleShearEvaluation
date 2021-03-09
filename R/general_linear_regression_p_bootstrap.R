general_linear_regression_p_bootstrap<-function(x,y,n_agg=5,family=gaussian(link="identity"),na.rm=FALSE,...)
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
        
        linmod=glm(y~x,data=theData,family=family,...)
        
        
        
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
        
        if(!(linmod$family$family=="quasi"))
        {   # There is a calling environment issue in the package rsq, if these functions are called from within other functions,
            # the family argument is not always resolved. This is why we copied the rsq.v function of interest, but added a "virtual"
            # family argument that technically doesn't get used directly but in evaluation of the calls.
             r2[ind_group]=rsq.v.family(linmod,adj=TRUE,family=family,theData=theData)
            #
            
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
    
    
    return_val = pf(mean(t^2,na.rm=na.rm),1,mean(df,na.rm=na.rm),lower.tail=FALSE)
    
    # Idea here: convert p-values to z values which can be averaged. If the shapiro values
    # are uniformly distributed we can get p=0.5 on average; if there is a systematic contribution,
    # then this will lead to a true averaging around the bias.
    
    z_shapiro = mean(qnorm(p_shapiro),na.rm=na.rm)
    
    
    attr(return_val,"F")=mean(t^2,na.rm=na.rm)
    attr(return_val,"DF1")=1
    attr(return_val,"DF2")=mean(df,na.rm=na.rm)
    attr(return_val,"adj.r.squared")=mean(r2,na.rm=na.rm)
    attr(return_val,"p_shapiro")=pnorm(z_shapiro)
    attr(return_val,"confint")=c("2.5 %"=quantile(estimate,probs=c(0.025))[["2.5%"]]
    ,"97.5 %"=quantile(estimate,probs=c(0.975))[["97.5%"]])
    
    
    return (return_val)
    
    
    
    
    
}



rsq.v.family=function (fitObj, adj = FALSE,family=gaussian(link="identity"),theData=NULL)
{
    if (is(fitObj, "glmerMod")) {
        rsq <- rsq.glmm(fitObj, adj = adj)
    }
    else if (is(fitObj, "glm")) {
        y <- fitObj$y
        wt <- weights(fitObj)
        if (is.null(wt))
            wt <- y * 0 + 1
        n <- sum(wt)
        if (is.null(y))
            stop("The glm object does not include response values!")
        yfit <- fitObj$fitted.values
        if (pmatch("Negative Binomial", family(fitObj)$family,
            nomatch = F)) {
            theta <- ifelse(is.null(fitObj$theta), as.numeric(gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.",
                "", family(fitObj)$family, perl = T)), fitObj$theta)
            sse1 <- sum(wt * vresidual(y, yfit, family = negative.binomial(theta))^2)
            f0 <- glm(y ~ 1, family = negative.binomial(theta))
            yf0 <- f0$fitted.values
            sse0 <- sum(wt * vresidual(y, yf0, family = negative.binomial(theta))^2)
        }
        else if (family(fitObj)$family == "binomial") {
            nSuc <- wt * y
            nFai <- wt - nSuc
            tone <- rep(1, length(nSuc))
            sse1 <- sum(nSuc * vresidual(tone, yfit, family = family(fitObj))^2) +
                sum(nFai * vresidual(1 - tone, yfit, family = family(fitObj))^2)
            f0 <- update(fitObj, . ~ 1)
            yf0 <- f0$fitted.values
            sse0 <- sum(nSuc * vresidual(tone, yf0, family = family(f0))^2) +
                sum(nFai * vresidual(1 - tone, yf0, family = family(f0))^2)
        }
        else {
            sse1 <- sum(wt * vresidual(y, yfit, family = family(fitObj))^2)
            f0 <- update(fitObj, . ~ 1)
            sse0 <- sum(wt * vresidual(y, f0$fitted.values, family = family(f0))^2)
        }
        pM <- fitObj$df.null - fitObj$df.residual + 1
        rsq <- 1 - (sse1/sse0) * ifelse(adj, (n - 1)/(n - pM),
            1)
    }
    rsq
}

