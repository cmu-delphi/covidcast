Start = function(x) head(x, 1)
End = function(x) tail(x, 1)
Min = function(x) min(x, na.rm = TRUE)
Max = function(x) max(x, na.rm = TRUE)
Sum = function(x) sum(x, na.rm = TRUE)
Mean = function(x) mean(x, na.rm = TRUE)
Median = function(x) median(x, na.rm = TRUE)

##########

quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

##########
