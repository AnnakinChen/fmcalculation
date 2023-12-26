FVIF=function(i,n){
  return((1+i)^n)
}
PVIF=function(i,n){
  return(1/(1+i)^n)
}
#复利终值
FV=function(pv,i,n){
  return(pv*(1+i)^n)
}

#复利现值
PV=function(fv,i,n){
  return(fv/(1+i)^n)
}

FVIFA=function(i,n){
 res=((1+i)^n-1)/i
 return(res)
}

#后付终值
FVA=function(A,i,n){
  return(A*FVIFA(i,n))
}

PVIFA=function(i,n){
  res=((1+i)^n-1)/(i*(1+i)^n)
  return(res)
}

#后付现值
PVA=function(A,i,n){
  return(A*PVIFA(i,n))
}

#先付终值
XFVA=function(A,i,n){
  res=FVA(A,i,n)*(1+i)
  return(res)
}

#先付现值
XPVA=function(A,i,n){
  res=PVA(A,i,n)*(1+i)
  return(res)
}

#延期
DPV=function(A,i,m,n){
  res=A*PVIFA(A,i,n)*(1/(1+i)^m)
  return(res)
}

#永续
PAV=function(A,i){
  return(A/i)
}

#二叉树
phi = function(N,q,t,r){
  phi = diag(x = q,nrow = N+1)
  for(i in 1:N){
    phi[i,i+1] = 1-q
  }
  phi = exp(-r*t)*phi
  return(phi)
}
V_N_1 = function(N,u,d,s0,k,type){
  if(type=='call'){
    S = matrix(0,nrow = N+1,ncol = 1)
    for(i in 1:(N+1)){
      S[i,1] = u^(N-i+1)*d^(i-1)*s0
      S[i,1] = max(S[i,1]-k,0)
    }
  }
  else{
    S = matrix(0,nrow = N+1,ncol = 1)
    for(i in 1:(N+1)){
      S[i,1] = u^(N-i+1)*d^(i-1)*s0
      S[i,1] = max(k-S[i,1],0)
    }
  }
  
  return(S)
}
option0 = function(s0,k,r,sigma,N,maturity,type){
  
  t = maturity/N
  u = exp(sigma*sqrt(t))
  d = 1/u
  q = (exp(r*t)-d)/(u-d)
  phi = phi(N,q,t,r)
  res = diag(1,nrow = N+1)
  for(i in 1:N){
    res = res%*%phi
  }
  V_N_1 = V_N_1(N,u,d,s0,k,type)
  V1 = res%*%V_N_1
  return(round(V1[1,1],5))
  
}

fytm = function(n,pv,fv,pmt){
  fytm1 = function(r){
    res = 0
    for(i in 1:n){
      res = res+pmt/(1+r)^i
    }
    res = res+fv/(1+r)^n
    return(res-pv)
  }
  return(nleqslv(0,fytm1,control=list(ftol = 1e-25, xtol = 1e-25))$x)
}

p_fytm = function(n,pv,fv,pmt){
  if(fv==0 & pv!=0){
    p_fytm1 = function(r){
      res = 0
      for(i in 1:n){
        res = res+pmt/(1+r)^(i-1)
      }
      res = res+fv/(1+r)^(n-1)
      return(res-pv)
    }
    return(nleqslv(0,p_fytm1,control=list(ftol = 1e-25, xtol = 1e-25))$x)
  }
  else if(fv<=0 & pv==0){
    p_fytm1 = function(r){
      res = 0
      for(i in 1:n){
        res = res+pmt/(1+r)^(i-1)
      }
      res = res+fv/(1+r)^(n)
      return(res-pv)
    }
    return(nleqslv(0,p_fytm1,control=list(ftol = 1e-25, xtol = 1e-25))$x)
  }
  else{
    return('Unsupported Calculation!')
  }
  
}

ffv = function(n,pv,pmt,ytm){
  ffv1 = function(fv){
    res = 0
    for(i in 1:n){
      res = res+pmt/(1+ytm)^i
    }
    res = res+fv/(1+ytm)^n
    return(res-pv)
  }
  return(nleqslv(0,ffv1,control=list(ftol = 1e-25, xtol = 1e-25))$x)
}

p_ffv = function(n,pv,pmt,ytm){
  p_ffv1 = function(fv){
    res = 0
    for(i in 1:n){
      res = res+pmt/(1+ytm)^(i-1)
    }
    res = res+fv/(1+ytm)^(n-1)
    return(res-pv)
  }
  a = nleqslv(0,p_ffv1,control=list(ftol = 1e-25, xtol = 1e-25))$x*(1+ytm)
  return(a)
}

fpv = function(n,fv,pmt,ytm){
  res = 0
  for(i in 1:n){
    res = res+pmt/(1+ytm)^i
  }
  res = res+fv/(1+ytm)^n
  return(res)
}

p_fpv = function(n,fv,pmt,ytm){
  res = 0
  for(i in 1:n){
    res = res+pmt/(1+ytm)^(i-1)
  }
  res = res+fv/(1+ytm)^(n-1)
  return(res)
}

fpmt = function(n,pv,fv,ytm){
  fpmt1 = function(pmt){
    res = 0
    for(i in 1:n){
      res = res+pmt/(1+ytm)^i
    }
    res = res+fv/(1+ytm)^n
    return(res-pv)
  }
  return(nleqslv(0,fpmt1,control=list(ftol = 1e-25, xtol = 1e-25))$x)
}

p_fpmt = function(n,pv,fv,ytm){
  if(fv==0 & pv!=0){
    p_fpmt1 = function(pmt){
      res = 0
      for(i in 1:n){
        res = res+pmt/(1+ytm)^(i-1)
      }
      res = res+fv/(1+ytm)^(n-1)
      return(res-pv)
    }
    return(nleqslv(0,p_fpmt1,control=list(ftol = 1e-25, xtol = 1e-25))$x)
  }
  else if(fv<=0 & pv==0){
    fv = fv/(1+ytm)
    p_fpmt1 = function(pmt){
      res = 0
      for(i in 1:n){
        res = res+pmt/(1+ytm)^(i-1)
      }
      res = res+fv/(1+ytm)^(n-1)
      return(res-pv)
    }
    return(nleqslv(0,p_fpmt1,control=list(ftol = 1e-25, xtol = 1e-25))$x)
  }
  else{
    return('Unsupported Calculation!')
  }
  
}




