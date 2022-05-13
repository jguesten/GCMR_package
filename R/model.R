#' Run Model Function
#'
#' @param data
#' @param stanmod
#' @keywords model
#' @export
#' @examples
#' run_model()
  ## run stanmodel
  run_model = function(data, stanmod) {
    options(mc.cores = 1)
    require(rstan)
    require(tidyverse)
    # generate and run model
    mod <- stan_model(model_code = eval(parse(text = stanmod)))

    fit = rstan::sampling(mod,
                          data = data,
                          chains = 2,
                          iter = 2000)
    fit
  }

  ## extract stanmodel pars
  extract_pars=function(mod){
    parnames=c("theta","beta","gamma")
    parlist=sapply(parnames,function(x)if(!length(grep(x,names(mod)))<1){rstan::extract(mod,pars=x)[[x]]})
    parlist
  }

  ## simulate replicated y
  get_yrep=function(X,pars,mod){
    ## get y_rep ##X
    if(mod=="bc"){
        temp=(pars$theta[X$jj]-pars$beta[X$ii])
        p=exp(temp)/(exp(temp)+1)
        g=abs(X$z-pars$gamma[X$jj])
        prob=p+(1-p)*g
        y_rep=prob%>%rbernoulli(1, p = .)%>%as.numeric()
    }else if(mod=="rasch"){
        temp=(pars$theta[X$jj]-pars$beta[X$ii])
        prob=exp(temp)/(exp(temp)+1)
        y_rep=prob%>%map_lgl(~rbernoulli(1, p = .x))%>%as.numeric()
    }
    y_rep
  }

  ## calculate INFIT
  get_INFIT=function(X,y,pars,mod){
    require(tidyverse)
  if(mod == "rasch"){
    temp=(pars$theta[X$jj]-pars$beta[X$ii])
    prob=exp(temp)/(exp(temp)+1)
    tab=tibble(prob=prob,prob_comp=1-prob)
    }else if(mod == "bc"){
      temp=(pars$theta[X$jj]-pars$beta[X$ii])
      p=exp(temp)/(exp(temp)+1)
      g=abs(X$z-pars$gamma[X$jj])
      prob=p+(1-p)*g
      tab=tibble(prob=prob,prob_comp=1-prob)
    }


  pred_dat=cbind(y=y,tab) %>%
    mutate(SE=(y-prob)^2,prod=prob*prob_comp)%>%
    group_by(X$ii)%>%
    summarize(INFIT=sum(SE)/sum(prod))
  pred_dat
  }

  get_W=function(X,y,pars,mod){
    if(mod == "rasch"){
      temp=(pars$theta[X$jj]-pars$beta[X$ii])
      prob=exp(temp)/(exp(temp)+1)
      tab=tibble(prob=prob,prob_comp=1-prob)
    }else if(mod == "bc"){
      temp=(pars$theta[X$jj]-pars$beta[X$ii])
      p=exp(temp)/(exp(temp)+1)
      g=abs(X$z-pars$gamma[X$jj])
      prob=p+(1-p)*g
      tab=tibble(prob=prob,prob_comp=1-prob)
    }

    pred_dat=cbind(y=y,tab) %>%
      mutate(SE=(y-prob)^2,prod=prob*prob_comp)%>%
      group_by(X$jj)%>%
      summarize(W=sum(SE)/sum(prod))
    pred_dat
  }


