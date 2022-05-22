random_sine_wave = function(x, num_sine_waves, frequency){

  if(missing(frequency)){frequency = 10}

  y = rep(0, length(x))

  for(i in 1:num_sine_waves){
    a = runif(1, -10, 10)
    b = runif(1, -frequency, frequency)
    y = y + a*sin(b*x)
  }

  y = (y - min(y)) / (max(y) - min(y))

  return(y)

}
