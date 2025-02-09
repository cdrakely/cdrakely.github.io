# 1. hand_size is an integer of the draw size
#    - Ex. You draw 7 cards for Uno.
# 2. available is a vector of the # of items in the total pool
#    - Ex. There are 26 red and 26 black cards in a standard deck - c(26,26).
#    The sum of available should be the total deck size. Non-differentiated
#    items should be grouped into one entity at the end.
# 3. criteria is the number of items from available needed
#    for a beginning hand size.
#    - Ex. If I need 2 red cards and 1 black in my starting draw: c(2,1).
#      In this example, we don't have a third 0 entry, since the deck is only
#      red or black.
#    - Ex. If I need 2 reds, 1 blue, and 1 green for a starting Uno hand
#      I would use a criteria of c(2,1,1,0), where 0 declares I don't need
#      any yellow cards.
#
# Output -
# successes: Number of possible successes
# total_possibilities: Number of total outcomes
# percentage_success: Not rounded or multiplied by 100
# calculation_terms: Number of terms in the sum in algorithm
# terms_details: Details of those calculation_terms, represents a
#   vector of variations of draws from the sets in available
#   that represent a success and a full hand drawn.
# Execution time: Time in seconds to execute.
deck_comb_tot <- function(hand_size, available, criteria) {
  # First iteration is a for loop, remaining are recursed.
  start_time <- Sys.time()
  calculation_terms <- 0
  loop_track <- c(rep(0, length(available)))
  terms_detail <- list()
  terms_success_number <- c()
  recurse_ret <- comb_recurse_tot(
    1, 0, loop_track,
    terms_detail,
    terms_success_number,
    hand_size, available, criteria, calculation_terms
  )

  poss <- choose(sum(available), hand_size)
  return(list(
    successes = recurse_ret[[1]],
    total_possibilities = poss,
    percentage_success = recurse_ret[[1]] / poss,
    calculation_terms = recurse_ret[[2]],
    terms_detail = recurse_ret[3],
    terms_success_number = recurse_ret[[4]],
    execution_time = Sys.time() - start_time
  ))
}

comb_recurse_tot <- function(
    depth, cur_hand, loop_track, terms_detail,
    terms_success_number,
    hand_size, avail, crit, calc_terms) {
  # Recursive while loop
  ret <- 0
  iter <- crit[depth]
  # Hand size stipulates how many elements
  # sum(crit) reminds how many required elements we
  #   need in the hand for success
  # crit[depth] rolls back in the current items requirements
  #   which makes max_iter the maximum number of elements
  #   of this set that can be present and still render
  #   a success.
  max_iter <- hand_size - sum(crit) + crit[depth]
  while (iter <= min(
    avail[depth], # Can't have more elements that the set contains
    hand_size - sum(cur_hand), # Can't have more elements than the hand size
    max_iter # Can't break success criteria of the remaining sets
  )) {
    loop_track[depth] <- iter
    if (depth != length(avail)) {
      marg_ret <- comb_recurse_tot(
        depth + 1, cur_hand + iter,
        loop_track, terms_detail,
        terms_success_number,
        hand_size, avail, crit, calc_terms
      )
      ret <- ret + marg_ret[[1]]
      calc_terms <- marg_ret[[2]]
      terms_detail <- marg_ret[[3]]
      terms_success_number <- marg_ret[[4]]
    } else {
      layer_return <- comb_calc(
        avail, loop_track, hand_size,
        terms_detail, terms_success_number
      )
      ret <- ret + layer_return[[1]]
      calc_terms <- calc_terms + 1
      terms_detail <- layer_return[[2]]
      terms_success_number <- layer_return[[3]]
      break
    }
    iter <- iter + 1
  }

  return(list(ret, calc_terms, terms_detail, terms_success_number))
}

comb_calc <- function(
    avail, loop_track, hand_size,
    terms_detail, terms_success_number) {
  terms <- length(avail)
  product <- 1
  # Test for single term use case, otherwise for loop behaves unexpectedly.
  if (terms > 1) {
    # For the first n-1 terms we want to calculate using criterion cards.
    for (t in 1:(terms - 1)) {
      product <- product * choose(avail[t], loop_track[t])
    }
  }
  # The nth term will fill the rest of the hand with remaining cards.
  product <- product * choose(avail[terms], hand_size - sum(loop_track))
  full_hand <- loop_track
  full_hand[length(avail)] <- hand_size - sum(loop_track)
  terms_detail <- append(terms_detail, list(full_hand))
  terms_success_number <- append(terms_success_number, product)
  return(list(product, terms_detail, terms_success_number))
}

test <- deck_comb_tot(7, c(4, 4, 4, 48), c(1, 1, 1, 0))
test2 <- deck_comb_tot(2, c(26, 26), c(1, 0))
test3 <- deck_comb_tot(1, c(26, 26), c(1, 1))
