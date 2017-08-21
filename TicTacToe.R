triples <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)
# Display Function
display<-function(state){
  p<-cat( state[1], " |", state[2],"|", state[3],"\n",
          "---+----+--- \n",
          state[4], " |", state[5], " |", state[6],"\n",
          "---+----+--- \n",
          state[7], "|", state[8], "|", state[9])
  return(p)
}

# Update Function
update<- function(state, who, pos){
  state[pos]<-who
  display(state)
  return(state)
} 

# Check Function - to see if the spot is taken
check<-function(state, pos){
  if (any(is.na(state[pos]))==TRUE){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Check Winner Function
check_winner<-function(state) {
  winner <- NA
  for (i in 1:length(triples)){
    if (sum(state[triples[[i]]]==c("x","x","x"), na.rm = TRUE) ==3 ) {
      winner <- TRUE
      break
    } else {
      if (sum(state[triples[[i]]]==c("o","o","o"), na.rm = TRUE)==3 ) {
        winner <- TRUE
      } else {
        winner<-FALSE
      }
    }
  }
  return(winner)
}

triples <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)

# Computer Turn Function
# 1) determine whether it shd play x or o
# 2) block if there two in a row - what position is this?
# 3) win if there are two in a row - what position is this?
# 4) play randomly otherwise as long as the spot is empty 

computer_turn<-function(state){
  turn<-sample(which(is.na(state) == TRUE), 1)
  for (i in 1:length(triples)) {
    if (sum(state[triples[[i]]] == c("x", "x", "x"), na.rm = TRUE) == 2 && check(state, is.na(state[triples[[i]]])) == TRUE){
      turn<-triples[[i]][is.na(state[triples[[i]]])]
      break
    break
    } else {
      if (sum(state[triples[[i]]] == c("o", "o", "o"), na.rm = TRUE) == 2 && check(state, is.na(state[triples[[i]]])) == TRUE){
        turn<-triples[[i]][is.na(state[triples[[i]]])]
        break
      break
      }
    }
  }
  return(turn)
}
# returns the position that the computer should play in


# this has ruined the sanctity of tic tac toe
#when it checks where not equal to "o" or "x" but it is actually not bc it is an NA
  

# Play Function - have to fix diagonals, make it print the winner
play<-function(){
  players<- readline("How many human players are there? ")   #number of players
  if (players == 1){
    first<- readline("Do you want to go first? Type 'Yes' or 'No' ")  #does human go first?
  }
  p<-1:9
  display(p)
  readline("The numbers 1-9 represent the positions on the board that you can place a letter. Press enter to continue")
  if (players == 2){
    readline("Decide who will be Player 1 and who will be Player 2! Player 1 gets x, and Player 2 gets o ")
  }
  state<<-rep(NA,9)
  result<-NA
  while(check_winner(state) == FALSE){
    if (sum(is.na(state)) == 0){
      result<-"Nobody has won!"
      break
      # Two Player Game
    } else {
      if (players == 2) {
        move1<-as.integer(readline("PLayer 1. Where do you want to play?"))
        if (check(state, move1) == FALSE){
          move1<-as.integer(readline("Oops that spot is filled! Try again!"))
          state <- update(state, "x", move1)
        } else {
          state<-update(state, "x", move1)
        }
        if (check_winner(state) == TRUE){
          result<-"Player 1 is the winner"
          break
        }
        if (sum(is.na(state)) == 0){
          result<-"Nobody has won!"
          break
          # Two Player Game
        }
        move2<-as.integer(readline("Player 2. Where do you want to play? "))
        if(check(state,move2) == FALSE){
          move2<-as.integer(readline("Oops that spot is filled! Try again! "))
          state<- update(state,"o",move2)
        } else {
          state<-update(state,"o", move2)
        }
        if (check_winner(state) == TRUE){
          result<-"Player 2 is the winner"
          break
        }
        if (sum(is.na(state)) == 0){
          result<-"Nobody has won!"
          break
          # Two Player Game
        }
      }  #the game if there are 2 human players
      if (players == 1) {
        if (first == "Yes") {
          cpu <- "o"
          move1<-as.integer(readline("PLayer 1. Where do you want to play?"))
          if (check(state, move1) == FALSE){
            move1<-as.integer(readline("Oops that spot is filled! Try again!"))
            state<-update(state, "x", move1)
          } else {
            state<-update(state, "x", move1)
          }
          if (check_winner(state) == TRUE){
            result<-"Player 1 is the winner"
            break
          } 
          if (sum(is.na(state)) == 0){
            result<-"Nobody has won!"
            break
          }
          readline("Press enter for the computer's turn")
          state<-update(state, cpu, computer_turn(state))
          if (check_winner(state) == TRUE){
            result<- "CPU has won"
            break
          }
          if (sum(is.na(state)) == 0){
            result<-"Nobody has won!"
            break
          }
        } else {
          cpu <- "x"
          readline("Press enter for the computer's move")
          state<-update(state, cpu, computer_turn(state))
          if (check_winner(state) == TRUE){
            result<- "CPU has won"
            break
          } 
          move1<-as.integer(readline("Player 2. Where do you want to play?")) 
          if (check(state, move1) == FALSE){
            move1<-as.integer(readline("Oops that spot is filled! Try again!"))
            state<-update(state, "o", move1)
          } else {
            state<-update(state, "o", move1)
          } 
          if (check_winner(state) == TRUE){
            result<-"Player 1 is the winner"
            break
          }
          if (sum(is.na(state)) == 0){
            result<-"Nobody has won!"
            break
          }
          if (sum(is.na(state)) == 0){
            result<-"Nobody has won!"
            break
          }
        }
      }
    }
  }
  return(result)
}





    
     


















