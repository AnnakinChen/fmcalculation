
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



