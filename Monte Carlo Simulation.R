# Roll d dice; find P(total = k)
probtotk = function(d,k,nreps) {
  count = 0
  # do the experiment nreps times -- like doing nreps notebook lines
  for(rep in 1:nreps){
    sum = 0
    # roll ddice and find their sum
    for (j in 1:d) sum = sum + roll()
    if (sum == k) count = count + 1
  }
  return(count/nreps)
}
# simulate roll of one die; the possible return 
# values are 1,2,3,4,5,6 all equally likely
roll = function() return(sample(1:6,1))
# example
probtotk(3,8,1000)
# Bus Ridership
nreps = 10000
nstops = 10
count = 0
for(i in 1:nreps){
  passengers = 0
  for (j in 1:nstops){
    if (passengers > 0) # any alight?
      for (k in 1:passengers)
        if (runif(1) < 0.2)
          passengers = passengers - 1
    newpass = sample(0:2,1,prob = c(0.5,0.4,0.1))
    passengers = passengers + newpass
  }
  if (passengers == 0) count = count + 1
}
print(count/nreps)
