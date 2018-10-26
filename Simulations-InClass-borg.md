Simulations In-Class Project
================
Due October 13, 2017 at 11:59pm

<style type="text/css">
.table {

    width: 80%;
    margin-left:10%; 
    margin-right:10%;
}
</style>

\#Project Goals:

With this project we will simulate a famous probability problem. This
will not require knowledge of probability or statistics but only the
logic to follow the steps in order to simulate this problem. This is one
way to solve problems by using the computer.

Since you all have completed problem 1, yaou first step will be to work
through each of your groupmates code for problem \#1 and comment on what
is happening. Then as a team move forward in on direction as you move on
to the rest of the problems 2-5

1.  **Gambler’s Ruin**: Suppose you have a bankroll of $1000 and make
    bets of $100 on a fair game. By simulating the outcome directly for
    at most 5000 iterations of the game (or hands), estimate:
    1.  the probability that you have “busted” (lost all your money) by
        the time you have placed your one hundredth bet.

<!-- end list -->

``` r
#set parameters values
set.seed(0)
bankroll<-1000
bet<- 100
trials<-5000
perc<-0.5

gambler<- function(B, b, N, p) {
  game<-rep(NA,N) #this creates a vector of NAs
  game[1]<-B
  for (i in 2:N) {
    hand <- game[i-1] + 2*b*rbinom(1, 1, prob = p) - b
    game[i]<-hand
    if (game[i]==0) {
        return(game[1:i])
        break
    }
  }
  return(game)
}

simul.result<-replicate(trials, gambler(bankroll, bet, trials, perc)) # replicate the whole game and store results 

p.bust.100<- sum(sapply(simul.result, function(x) length(x)<100))/trials #compute probability of being busted before 100th hand
```

2.  the probability that you have busted by the time you have placed
    your five hundredth bet by simulating the outcome
directly.

<!-- end list -->

``` r
p.bust.500<- sum(sapply(simul.result, function(x) length(x)<500))/trials #compute probability of being busted before 500th hand
```

3.  the mean time you go bust, given that you go bust within the first
    5000
hands.

<!-- end list -->

``` r
mean.bust<-sapply(simul.result, function(x) length(x)) #copmute the length of each game
mean.bust<-mean(mean.bust[mean.bust<5000]) #compute the average number of hands before going busted, conditional of not having reached the upper limit of 5000
```

4.  the mean and variance of your bankroll after 100 hands (including
    busts).

<!-- end list -->

``` r
hand.100<-sapply(simul.result, "[", 100) #excract the 100th hand from each game
hand.100[is.na(hand.100)]<-0 #replace NAs with zeros to include those in the following computations

mean.100<-mean(hand.100, na.rm = TRUE)
var.100<-var(hand.100, na.rm = TRUE)
mean.100
```

    ## [1] 1006

``` r
var.100
```

    ## [1] 830500

5.  the mean and variance of your bankroll after 500 hands (including
    busts).

<!-- end list -->

``` r
hand.500<-sapply(simul.result, "[", 500)
hand.500[is.na(hand.500)]<-0
mean.500<-mean(hand.500, na.rm = TRUE)
var.500<-var(hand.500, na.rm = TRUE)
mean.500
```

    ## [1] 1001

``` r
var.500
```

    ## [1] 2656370

Note: you *must* stop playing if your player has gone bust. How will you
handle this in the `for` loop?

2.  Repeat the previous problem with betting on black in American
    roulette, where the probability of winning on any spin is 18/38 for
    an even payout.

<!-- end list -->

``` r
perc.rou<-18/38 #set the new probability of winning

simul.roulette<-replicate(trials, gambler(bankroll, bet, trials, perc.rou))


p.bust.100.roulette<- sum(sapply(simul.roulette, function(x) length(x)<100))/trials

p.bust.100.roulette
```

    ## [1] 0.499

``` r
p.bust.500.roulette<- sum(sapply(simul.roulette, function(x) length(x)<500))/trials

p.bust.500.roulette
```

    ## [1] 0.921

``` r
mean.bust.roulette<-sapply(simul.roulette, function(x) length(x))
mean.bust.roulette<-mean(mean.bust.roulette[mean.bust.roulette<5000])

hand.100.roulette<-sapply(simul.roulette, "[", 100)
hand.100.roulette[is.na(hand.100.roulette)]<-0

mean.100.roulette<-mean(hand.100.roulette, na.rm = TRUE)
var.100.roulette<-var(hand.100.roulette, na.rm = TRUE)
mean.100.roulette
```

    ## [1] 596

``` r
var.100.roulette
```

    ## [1] 567833

3.  **Markov Chains**. Suppose you have a game where the probability of
    winning on your first hand is 48%; each time you win, that
    probability goes up by one percentage point for the next game (to a
    maximum of 100%, where it must stay), and each time you lose, it
    goes back down to 48%. Assume you cannot go bust and that the size
    of your wager is a constant $100.

<!-- end list -->

1.  Is this a fair game? Simulate one hundred thousand sequential hands
    to determine the size of your return. Then repeat this simulation 99
    more times to get a range of values to calculate the expectation.

<!-- end list -->

``` r
markov.trials<-100000
markov.p<-0.48
gambler.markov<- function(b, N, p) {
  game<-rep(NA,N) #this creates a vector of NAs
  z<-p #set initial probability
  
  for (i in 1:N) {
    win<-rbinom(1, 1, prob = z)
    if (i ==1) { # specific "if" for the first hand as it does not have an antecendent
      hand<-2*b*win - b
    } else {
        hand <- game[i-1] + 2*b*win - b
    }
    game[i]<-hand
    if (win==1) { #"if/else" statements to update probability
      z<-z + 0.01
      if (z>1) {
        z<-1
      }
    } else {
      z<- p
    }
  }
  return(game)
}

simul.markov<-replicate(100, gambler.markov(bet, markov.trials, markov.p)) #replicate the game 100 times

markov.ret<-simul.markov[dim(simul.markov)[1],] # markov returns: take the last row of markov simulations containing the final amount of money earned/lost

hist(markov.ret)
```

![](Simulations-InClass_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
range(markov.ret)
```

    ## [1] -285000 -120400

``` r
mean(markov.ret)
```

    ## [1] -208500

2.  Repeat this process but change the starting probability to a new
    value within 2% either way. Get the expected return after 100
    repetitions. Keep exploring until you have a return value that is as
    fair as you can make it. Can you do this automatically?

<!-- end list -->

``` r
rng<- seq(from = 0.46, to = 0.50, by = 0.001)
markov.rng<-matrix(data = NA, nrow = length(rng), ncol = 2) #create empty matrix to store expected returns
for (t in 1:length(rng)) {
  try.prob<-rng[t]
  markov.rng[t,1]<-rng[t]
  simul.markov<-replicate(100, gambler.markov(bet, markov.trials, try.prob))
  markov.ret<-simul.markov[dim(simul.markov)[1],] # markov returns
  markov.rng[t,2]<-mean(markov.ret)
}
markov.rng
```

    ##        [,1]    [,2]
    ##  [1,] 0.460 -617836
    ##  [2,] 0.461 -596538
    ##  [3,] 0.462 -578248
    ##  [4,] 0.463 -550430
    ##  [5,] 0.464 -534956
    ##  [6,] 0.465 -510458
    ##  [7,] 0.466 -489796
    ##  [8,] 0.467 -475858
    ##  [9,] 0.468 -449504
    ## [10,] 0.469 -427196
    ## [11,] 0.470 -410410
    ## [12,] 0.471 -380964
    ## [13,] 0.472 -371040
    ## [14,] 0.473 -347174
    ## [15,] 0.474 -323024
    ## [16,] 0.475 -308434
    ## [17,] 0.476 -188926
    ## [18,] 0.477 -254910
    ## [19,] 0.478 -240628
    ## [20,] 0.479 -220118
    ## [21,] 0.480 -196324
    ## [22,] 0.481 -174040
    ## [23,] 0.482 -155152
    ## [24,] 0.483 -137048
    ## [25,] 0.484 -115212
    ## [26,] 0.485  -96452
    ## [27,] 0.486  -59660
    ## [28,] 0.487   42556
    ## [29,] 0.488  -34970
    ## [30,] 0.489   -7982
    ## [31,] 0.490    8428
    ## [32,] 0.491   99940
    ## [33,] 0.492   48216
    ## [34,] 0.493   71692
    ## [35,] 0.494   95900
    ## [36,] 0.495  199344
    ## [37,] 0.496  273354
    ## [38,] 0.497  153780
    ## [39,] 0.498  227382
    ## [40,] 0.499  200886
    ## [41,] 0.500  229634

``` r
markov.rng[markov.rng[,2]==min(abs(markov.rng[,2]))] #report the probability that gets closer to a fair return
```

    ## numeric(0)

3.  Repeat again, keeping the initial probability at 48%, but this time
    change the probability increment to a value different from 1%. Get
    the expected return after 100 repetitions. Keep changing this value
    until you have a return value that is as fair as you can make it.

<!-- end list -->

4.  Creating a Bootstrap function. There is a particular concept called
    \[bootstrapping\]
    (<https://en.wikipedia.org/wiki/Bootstrapping_(statistics)>) where
    we can easily create 95% confidence intervals, even for complex
    estimators.

The steps of this process are:

1.  Draw a sample, with replacement, from your data which is the same
    length of your data.
2.  Calculate the statistic of interest on this boostrap sample (ie
    mean, variance, regression,…)
3.  Peform steps 1:2 at least 1000 times over until you have a vector of
    your statistics.
4.  The lower bound of a 95% CI will be the 0.025 percentile
5.  The upper bound of a 95% CI will be the 0.975 percentile

Make a function called `boot_ci` which calculates the 95% confidence
interval in this manner.

``` r
boot_ci = function(x, iter) {
  #x: data to sample from
  #iter: number of times you will draw a sample
  boot_statistic = numeric()
  for (i in 1:iter){
    boot_sample = sample(x, size = length(x), replace=TRUE)
    boot_statistic[i] <- mean(boot_sample)
  }
  #boot_se <- sd(boot_statistic) #why you take sd? shouldn't we just take the 0.025 and the 0.975 percentiles? 
  boot_ci <- quantile(boot_statistic, probs = c(0.025, 0.975))
  #return(boot_se)
  return(boot_ci)
}

# Assumptions
size = 10000
mean = 0
sd = 1
mydata <- rnorm(size, mean, sd)
mydata_mean = mean(mydata) #compute mean
mydata_sd = sd(mydata) #compute sd
hist(mydata)
```

![](Simulations-InClass_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#(gabri's edit) compute "real" CI (actually the same as Pablo's but I don't understand the steps he takes.)
mydata_lower <- mydata_mean - 1.96*(mydata_sd/sqrt(size))
mydata_upper <- mydata_mean + 1.96*(mydata_sd/sqrt(size))
mydata_ci <- c(mydata_lower, mydata_upper)
mydata_ci
```

    ## [1] -0.0257  0.0133

``` r
mydata_boot_ci <- boot_ci(mydata, size)
mydata_boot_ci
```

    ##    2.5%   97.5% 
    ## -0.0254  0.0131

``` r
#mydata_se=boot_ci(mydata, 10000) 

#mydata_mean_ci <- mydata_mean + qt( c(0.025, 0.975), length(mydata) - 1) * mydata_se # why using the function here? shouldn't we get the CI from the bootstrap and compare it with the real one?
```

5.  For problems 3b and 3c, you calculated a mean value. Because you
    saved these final results in a vector, use the bootstrap to estimate
    the variance of the return in each case for your final answer. Once
    you have these results, which game has the smaller variance in
    returns?
