abs_loop <- function(vec){
  for (i in 1:length(vec)) {
    if (vec[i] < 0) {
      vec[i] <- -vec[i]
    }
  }
  vec
}


abs_set <- function(vec){
  negs <- vec < 0
  vec[negs] <- vec[negs] * -1
  vec
}


score_many <- function(symbols) {
  
  # Step 1: Assign base prize based on cherries and diamonds ---------
  ## Count the number of cherries and diamonds in each combination
  cherries <- rowSums(symbols == "C")
  diamonds <- rowSums(symbols == "DD")
  
  ## Wild diamonds count as cherries
  prize <- c(0, 2, 5)[cherries + diamonds + 1]
  
  ## ...but not if there are zero real cherries
  ### (cherries is coerced to FALSE where cherries == 0)
  prize[!cherries] <- 0
  
  
  # Step 2:Change prize for combinations that contain three of a kind 
  same <- symbols[, 1] == symbols[, 2] & symbols[, 2] == symbols[, 3]
  
  payoffs <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, "B" = 10, "C" =10, "0" = 0)
  
  prize[same] <- payoffs[symbols[same, 1]]
  
  # Step 3: Change prize for combinations that contain all bars -------
  bars <- symbols == "B" | symbols == "BB" | symbols == "BBB"
  all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same
  prize[all_bars] <- 5
  
  # Step 4: Handle wilds -----------------------
  ## combos with two diamonds
  two_wilds <- diamonds == 2
  
  ### Identify the nonwild symbol
  one <- two_wilds & symbols[, 1] != symbols[, 2] & symbols[, 2] == symbols[, 3]
  two <- two_wilds & symbols[, 1] != symbols[, 2] & symbols[, 1] == symbols[, 3]
  three <- two_wilds & symbols[, 1] == symbols[, 2] & symbols[, 1] != symbols[, 3]
  
  ### Treat as three of a kind
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]  
  prize[three] <- payoffs[symbols[three, 3]]
  
  ## combos with one wild
  one_wild <- diamonds == 1
  
  ### Treat as all bars (if appropriate)
  wild_bars <- one_wild & (rowSums(bars) == 2)
  prize[wild_bars] <- 5
  
  ### Treat as three of a kind (if appropriate)
  one <- one_wild & symbols[, 1] == symbols[, 2]
  two <- one_wild & symbols[, 2] == symbols[, 3]
  three <- one_wild & symbols[, 3] == symbols[, 1]
  
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  # Step 5: Double prize for every diamond in combo ------------------
  unname(prize * 2^diamonds)
  
  
}

#Y值IV测试代码
iosdefault <- read_csv("~/R/Linzi/IOSBehavior/rawData/IOSdefault0223.csv")
colnames(iosdefault)[2] <- "default"

iostest <- iosdefault %>% 
  inner_join(riskDB_df, by = "user_id") %>% 
  inner_join(behavior_df, by = "user_id")

iostest <- as.data.frame(iostest)

row.names(iostest) <- 1:nrow(iostest)
IVtest <- iv.mult(iostest,"default", TRUE)


test <-  mutate(step1_1,
                   pass_rate = pass_num / apply_num,
                   use_rate = loan_num / pass_num,
                   JK_FIRST = as.numeric(difftime(ymd(as.Date(step1_1$first_applytime)), ymd(as.Date(step1_1$first_loan_time)), units = "days")),
                   JK_TME =as.numeric(difftime(ymd(as.Date(step1_1$last_loan_time)), ymd(as.Date(step1_1$first_loan_time)), units = "days")),
                   DTI = loan_amount / base,
                   refuse_rate = refuse_product / confirm_product
)


