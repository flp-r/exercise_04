ReturnCoins = function(M){
  num50 <- floor(M / 50)
  reminder50 <- M %% 50
  
  
  num20 <- floor(reminder50/ 20)
  reminder20 <- reminder50 %% 20
  
  num10 <- floor(reminder20/ 10)
  reminder10 <- reminder20 %% 10
  
  num5 <- floor(reminder10/ 5)
  reminder5 <- reminder10 %% 5
  
  num2 <- floor(reminder5/ 2)
  reminder2 <- reminder5 %% 2
  
  num2 <- floor(reminder5/ 2)
  reminder2 <- reminder5 %% 2
  
  num1 <- floor(reminder2/ 1)
  reminder1 <- reminder2 %% 1
  
  return(c(num50, num20, num10, num5, num2, num1))
}


print(ReturnCoins(72))
print(c(50, 20, 10, 5, 2, 1))




UniversalReturnCoins <- function(M, coins){
  t = c()
  for (i in 1:length(coins)){
    t[i] <- 0
    while (M - coins[i] >= 0){
      t[i] <- t[i] + 1;
      M <- M - coins[i]
    }
    
  } 
  return(t)
}


UniversalReturnCoins2 <- function(M, coins){
  t = c()
  for (i in 1:length(coins)){
    t[i] <- floor(M/ coins[i])
    M <- M %% coins[i] 
  } 
  return(t)
}



coins <- c(50, 20, 10, 5, 2, 1)
kk <- UniversalReturnCoins2(785, coins)
print(kk)






Chocolate <- function(M, r,c){
  if (r >= nrow(M)){
    return (M[r, c])
  }
  else{
    bars <- M[r, c]
    down <- Chocolate(M, r+1, c)
    diagonal <- Chocolate(M, r+1, c+1)
    return (max(down, diagonal) + bars)
  }
  

}

M <- matrix(c(2,2,6,4,7,6,4,1,0,2,5,6,7,1,2,3), nrow = 4, ncol = 4)
r <- 1
c <- 1
k <- Chocolate(M, r, c)
print(k)


