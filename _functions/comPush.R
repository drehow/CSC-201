#_
comPush <- function(m='NA'){
  system('git add .')
  system(paste0('git commit -m "',m,'"'))
  system('git push -u origin master')
}
