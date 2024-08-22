bt_matching <- function(d, p){
  if ((d == "AB") & (p == "AB")){
    return(TRUE)
  }
  if ((d == "A") & ((p == "A") | (p == "AB"))){
    return(TRUE)
  }
  if ((d == "B") & ((p == "B") | (p == "AB"))){
    return(TRUE)
  }
  if (d == "O"){
    return(TRUE)
  }
  return(FALSE)
}

hla_matching <- function(d, p){
  require(transplantr)
  fun <- function(x1, x2, y1, y2){
    if (y2 == 98){
      return(1-as.numeric(y1 %in% c(x1,x2)))
    }
    return(length(which(!(unique(c(y1,y2)) %in% c(x1,x2)))))
  }
  return(hla_mm_level(fun(d[1], d[2], p[1], p[2]),
                      fun(d[3], d[4], p[3], p[4]),
                      fun(d[5], d[6], p[5], p[6])) <= 3)
  
}