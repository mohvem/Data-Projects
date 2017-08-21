gameboard <- data.frame(space = 1:40, title = c("Go" , "Mediterranean Avenue" , "Community Chest" , "Baltic Avenue" , "Income Tax" , "Reading Railroad" , "Oriental Avenue" , "Chance" , "Vermont Avenue" , "Connecticut Avenue" , "Jail" , "St. Charles Place" , "Electric Company" , "States Avenue" , "Virginia Avenue" , "Pennsylvania Railroad" , "St. James Place" , "Community Chest" , "Tennessee Avenue" , "New York Avenue" , "Free Parking" , "Kentucky Avenue" , "Chance" , "Indiana Avenue" , "Illinois Avenue" , "B & O Railroad" , "Atlantic Avenue" , "Ventnor Avenue" , "Water Works" , "Marvin Gardens" , "Go to jail" , "Pacific Avenue" , "North Carolina Avenue" , "Community Chest" , "Pennsylvania Avenue" , "Short Line Railroad" , "Chance" , "Park Place" , "Luxury Tax" , "Boardwalk"))

chancedeck <- data.frame(index = 1:15, card = c("Advance to Go" , "Advance to Illinois Ave." , "Advance to St. Charles Place" , "Advance token to nearest Utility" , "Advance token to the nearest Railroad" , "Take a ride on the Reading Railroad" , "Take a walk on the Boardwalk" , "Go to Jail" , "Go Back 3 Spaces" , "Bank pays you dividend of $50" , "Get out of Jail Free" , "Make general repairs on all your property" , "Pay poor tax of $15" , "You have been elected Chairman of the Board" , "Your building loan matures"))

communitydeck <- data.frame(index = 1:16, card = c("Advance to Go" , "Go to Jail" , "Bank error in your favor. Collect $200" , "Doctor's fees Pay $50" , "From sale of stock you get $45" , "Get Out of Jail Free" , "Grand Opera Night Opening" , "Xmas Fund matures" , "Income tax refund" , "Life insurance matures. Collect $100" , "Pay hospital fees of $100" , "Pay school tax of $150" , "Receive for services $25" , "You are assessed for street repairs" , "You have won second prize in a beauty contest" , "You inherit $100"))


# Dice --------------------------------------------------------------------
dice <- function(verbose=FALSE){
  faces <- sample(1:6, 2, replace=TRUE)
  if(faces[1] == faces[2]) doubles = TRUE
  else doubles = FALSE
  movement = sum(faces)
  if(verbose) cat("Rolled:", faces[1], faces[2], "\n")
  return(list(faces=faces, doubles=doubles, movement=movement))
}

player<-setRefClass("player",
                    fields = list(position = "numeric", 
                                  verbose = "logical",
                                  dubs = "numeric",
                                  jail_turn = "numeric",
                                  jail = "logical"
                    ),
                    methods = list(
                      move_n = function(n) { 
                        if(verbose) cat("Player at:", position)
                        if(verbose) cat(" Player moves:", n)
                        position <<- position + n
                        if(position > 40){
                          position <<- position - 40
                        }
                        if(verbose) cat(" Player now at:", position,"\n")
                      },
                      go_2_space_n = function(n){
                        if(verbose) cat("Player at:", position,".")
                        position <<- n
                        if(verbose) cat(" Player now at:", position,".\n")
                      }, 
                      doubles = function(player, tracking) { #goes through another turn if roll doubles and sends to jail if you hace 3 in a row
                        dubs <<- 0
                        while (dubs <= 3){
                          if (dubs < 3){
                            dubs<<-dubs + 1
                            if (verbose) cat("Doubles count is", dubs, "\n")
                            jail <<- FALSE
                            take_turn(player,tracking)
                            break
                          } else {
                            if (dubs == 3){
                              go_2_space_n(11) #go to jail
                              jail_status()
                              if (verbose) cat("Jail", "\n")
                              break
                            }
                          }
                        }
                      },
                      jail_func = function(player, tracking){ #executes jail turn
                         jail_turn <<- jail_turn + 1
                          while (jail == TRUE){
                            tracking$increase_count(11)
                            jail_roll<-dice() 
                            if (verbose) cat("Jail turn is", jail_turn, "\n") #rolls the dice as long as you are in jail
                            if (jail_roll$doubles == TRUE){
                              move_n(jail_roll$movement)
                              tracking$increase_count(player$position)
                              jail <<- FALSE
                              jail_turn<<-0
                              if (verbose) cat("Out of jail!", "\n")
                              break
                              # if any of the first 2 rolls are doubles, you move and end the loop
                            } else {
                              if (jail_turn == 3){
                                move_n(jail_roll$movement)
                                tracking$increase_count(player$position)
                                jail <<- FALSE
                                jail_turn <<-0
                                if (verbose) cat("Out of jail!", "\n")
                                break # on the third roll, you move, regardless of if you get doubles
                              } 
                            }
                            break
                          }
                          if (position == 31){ #sends you back to jail if you land on 31
                            jail_status()
                            jail_func(player, tracking)
                          }
                        },
                      jail_status = function(){
                        jail <<-TRUE
                      }
                    )
)

# Space Tracking Reference Class ------------------------------------------
# creates a new vector with a spot for each place on the baord, adds 1 each time the increase_count method is called on
tracking<-setRefClass("tracking",
                      fields = list(tally = "numeric"),
                      methods = list(
                        increase_count = function(n){
                          tally[n] <<- tally[n] + 1
                        }
                      )
)


# Take a turn -------------------------------------------------------------
take_turn <- function(player, tracking){
  if (player$jail == TRUE){
    player$jail_func(player, tracking)  #executes jail turn if in jail
  } else {
    while (player$jail == FALSE){  #turn possibilities when not in jail
      roll <- dice() 
      # roll <- dice(verbose = TRUE)  # this will only work if you are not using the manual dice
      player$move_n(roll$movement) #move to spot you roll to
      if (player$position == 31){ #if that spot is 31, sends you to jail and updates status
        player$go_2_space_n(11)
        player$jail_status()
        break   #quits while loop
      } else { #if you land on chance spot, tally a spot there and tally again only if it moves you
        if (player$position == 8 || player$position == 23 || player$position == 37){
          tracking$increase_count(player$position)
          chance_card(player, tracking)
          #need to fix chance and cc functions so that it will record a slot if you draw a card that moves you
          if (player$jail == TRUE){
            break
          }
        } else { #if you land on cc spot, tally a spot there and tally again only if it moves you
          if (player$position == 3 || player$position == 18 || player$position == 34){
            tracking$increase_count(player$position)
            cc_card(player,tracking) 
            if (player$jail == TRUE){
              break
            }
          } else {
            tracking$increase_count(player$position) #adds tallies for normal rolls
          } 
        } 
      } 
      #accounts for all special positions + normal rolls
      if (roll$doubles == TRUE && player$position != 31){ #if you roll doubles and it doesnt take you to 31
        player$doubles(player = player, tracking = tracking) #executes another turn
        break
      }
      break
    }
  }
}

# Drawing Chance Card -----------------------------------------------------
cc_card<-function(player,tracking){
  if (player$verbose) set.seed(10) #sets the seed when doing 20 turns
  carddrawn<-sample(1:16, 1) #randomly chooses the card
  if (player$verbose) cat ("Community Chest! Card drawn is", carddrawn, "\n")
  # based on what card is drawn between 1 and 2, will move the player, otherwise will do nothing
   #player advances to Go
  if (carddrawn == 1){
    player$go_2_space_n(1)
    tracking$increase_count(player$position)
  } else {
    # player advances to Jail
    if (carddrawn == 2){
      player$go_2_space_n(11)
      tracking$increase_count(player$position)
      player$jail_status()
    }
  }
}

# Chance Card --------------------------------------------------------------
chance_card<-function(player, tracking){ #same as cc card function
  if (player$verbose) set.seed(10)
  carddrawn<-sample(1:15,1)
  if (player$verbose) cat ("Community Chest! Card drawn is", carddrawn, "\n")
  #position moves to 1 aka Go
  if (carddrawn == 1) {
    player$go_2_space_n(1)
    tracking$increase_count(player$position)
  }  else {
    #position moves to 25 aka Illinois Avenue
    if (carddrawn == 2) {
      player$go_2_space_n(25)
      tracking$increase_count(player$position)
    } else {
      # position moves to 12 aka St. Charles Place
      if (carddrawn == 3) {
        player$go_2_space_n(12)
        tracking$increase_count(player$position)
      } else {
        #utility one
        if (carddrawn == 4) {
          if (player$position > 13 && player$position <= 29){
            player$go_2_space_n(29)
            tracking$increase_count(player$position)
          } else {
            if ((player$position >29 && player$position <=40) || (player$position >= 1 && player$position <=13)){
              player$go_2_space_n(13)
              tracking$increase_count(player$position)
            }
          }
        }  else {
          #moves to nearest railroad in the forward direction --> what do you do if you're on a railroad spot?
          if (carddrawn == 5) {
            if (6 - player$position < 10){
              player$go_2_space_n(6)
              tracking$increase_count(player$position)
            } else {
              if (16 - player$position < 10){
                player$go_2_space_n(16)
                tracking$increase_count(player$position)
              } else {
                if (26 - player$position < 10){
                  player$go_2_space_n(26)
                  tracking$increase_count(player$position)
                } else {
                  if (36 - player$position < 10){
                    player$go_2_space_n(26)
                    tracking$increase_count(player$position)
                  }
                }
              } 
            }
          } else {
            # moves to Reading Railroad aka position 6
            if (carddrawn == 6) {
              player$go_2_space_n(6)
              tracking$increase_count(player$position)
            } else {
              # moves to Boardwalk
              if (carddrawn == 7) {
                player$go_2_space_n(40)
                tracking$increase_count(player$position)
              } else {
                # Go to Jail
                if (carddrawn == 8) {
                  player$go_2_space_n(11)
                  tracking$increase_count(player$position)
                  player$jail_status()
                }
              }
            } 
          }
        }
      } 
    }
  } 
}


# Running the simulation --------------------------------------------------
## goes through the for loop without any of the rules implemented
set.seed(10)

space_tracking <- tracking$new(tally = rep(0,40))
for(i in 1:1000){ # simulate 1000 games
  player1 <- player$new(position = 1, verbose = FALSE, dubs = 0, jail_turn = 0, jail = FALSE)  # new players for each game
  player2 <- player$new(position = 1, verbose = FALSE, dubs = 0, jail_turn = 0, jail = FALSE)
  for(i in 1:150){ # 150 turns for each game
    if(player1$verbose) cat("Player 1 turn\n")
    take_turn(player1, space_tracking)  
    if(player2$verbose) cat("Player 2 turn\n")
    take_turn(player2, space_tracking)  
  }
}
require(dplyr)


results <- cbind(gameboard, tally = space_tracking$tally)
results <- cbind(results, rel = results$tally/sum(results$tally))
results1<-results[order(-results$tally),]
print(results)

print((cbind(order = 1:40, results1))) #descwnding order table

sum(results$tally)

# Manual Dice (Useful for testing) --------------------------
Dice = setRefClass("Dice", 
                   fields = list(
                     rolls = "numeric",
                     pos = "numeric",
                     verbose = "logical"
                   ), 
                   methods = list(
                     roll = function() {
                       faces = rolls[pos + seq_len(2)]
                       pos <<- pos + 2
                       if(faces[1] == faces[2]) doubles = TRUE
                       else doubles = FALSE
                       movement = sum(faces)
                       if(verbose) cat("Rolled:", faces[1], faces[2], "\n")
                       return(list(faces=faces, doubles=doubles, movement=movement))
                     }
                   )
)
set.seed(10)
setdice <- Dice$new(rolls = c(6, 4, 5, 3, 3, 5, 6, 2, 5, 4, 4, 1, 2, 6, 4, 4, 4, 4, 2, 2, 
                              4, 3, 4, 4, 1, 4, 3, 4, 1, 2, 3, 6, 5, 4, 5, 5, 1, 2, 5, 4, 
                              3, 3, 1, 1, 2, 1, 1, 3),
                    pos = 0, verbose = TRUE)
dice <- function() setdice$roll()
space_tracking <- tracking$new(tally = rep(0,40))
player1 <- player$new(position = 1, verbose = TRUE, dubs = numeric(0), jail_turn = 0, jail = FALSE)  # new players for each game
for(i in 1:20){ # 100 turns for each game
  cat("\n## Turn", i,"\n")
  take_turn(player1, space_tracking) 
  if (player1$jail == TRUE){
    next
  }
}
