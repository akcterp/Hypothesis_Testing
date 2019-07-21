#' Hypothesis Testing Automation
#' @param values
#' @export

hypothesis<-function(data,target)
{
  if(!is.data.frame(data))
    stop("Not a data frame")
  if (length(unique(target))==2 & is.numeric(target))
  {
    p_vals=c()
    for (i in 1:ncol(data))
    {
      if (is.factor(data[,i]))
      {
        chi_pval = chisq.test(table(data[,i], target))$p.value
        p_vals[i] = chi_pval
      }
      else if(is.numeric(data[,i]))
      {
        if(length(unique(data[,i]))==2)
        {
          ttest_pval = t.test(target~ data[,i])$p.value
          p_vals[i]=ttest_pval
        }
        else
        {
          aovR = aov(target ~ data[,i])
          fv = summary(aovR)[[1]][[1,"F value"]]
          pvalue =summary(aovR)[[1]][[1,"Pr(>F)"]]
          p_vals[i] = pvalue
        }
      }
    }
    Vars = names(data)
    print(data.frame(Vars,p_vals))
  }
  else
  {
    print("Target variable not binary")
  }
}
