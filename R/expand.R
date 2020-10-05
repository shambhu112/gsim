
expand_fct_with_splits <- function(source_fct , splits , n){

  if(sum(unlist(splits)) != 1){
    msg_error(" splits need to add to 1 in expand_fct_with_splits ")
    stop(" Refer error log")
  }

  vec <- sapply(1:length(source_fct) , function(x){
    count <- round(n * as.numeric(splits[x]))
    rep(x =as.character(source_fct[x]) , times = count)
  })
  vec <- unlist(vec)

  z <- length(vec)
  if(z != n){
    msg_warning("Length mismatch vec length = {z} and n = {n}")
  }
  rearranged <- sample(vec , n)
  return(rearranged)
}


expand_fct_with_joint_prob <- function(source_fct){

}
