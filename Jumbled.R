# 23/04/2022
# Lucy Vanes
# Code to create jumbled choir seating
# with full voice part labels

# Define seating
#====================

# Equal number of seats per row? - needs bespoke alternative
n_rows <- 8
equal_seats <- TRUE


# Define voice numbers
#======================
B1_r <- 7
B1_y <- 6
B1_w <- 5

B2_r <- 8
B2_y <- 6
B2_w <- 5

T1_r <- 7
T1_y <- 5
T1_w <- 6

T2_r <- 6
T2_y <- 7
T2_w <- 8

A1_r <- 9
A1_y <- 5
A1_w <- 6

A2_r <- 7
A2_y <- 6
A2_w <- 8

S1_r <- 5
S1_y <- 7
S1_w <- 6

S2_r <- 9 
S2_y <- 7
S2_w <- 6

choir <- c(rep("B1_r",B1_r), rep("B1_y", B1_y), rep(rep("B1_w", B1_w)),
           rep("B2_r",B2_r), rep("B2_y", B2_y), rep(rep("B2_w", B2_w)),
           rep("T1_r",T1_r), rep("T1_y", T1_y), rep(rep("T1_w", T1_w)),
           rep("T2_r",T2_r), rep("T2_y", T2_y), rep(rep("T2_w", T2_w)),
           rep("A1_r",A1_r), rep("A1_y", A1_y), rep(rep("A1_w", A1_w)),
           rep("A2_r",A2_r), rep("A2_y", A2_y), rep(rep("A2_w", A2_w)),
           rep("S1_r",S1_r), rep("S1_y", S1_y), rep(rep("S1_w", S1_w)),
           rep("S2_r",S2_r), rep("S2_y", S2_y), rep(rep("S2_w", S2_w)))


n_singers <- length(choir)
n_singers

if (equal_seats==TRUE){
  seats_per_row <- ceiling(n_singers / n_rows)
} 
seats_per_row

superfluous_seats <- seats_per_row*n_rows - n_singers

#==============================================================================

dat <- data.frame(row = rep(1:n_rows, each=seats_per_row), seat = rep(1:seats_per_row, n_rows), edge=0, label = NA, voice = NA, height= NA, colour = NA, clash=NA)

# remove superfluous seats from last row (imperfect)
dat <- dat[-((dim(dat)[1]-superfluous_seats+1) : dim(dat)[1]),]

# define edge seats
dat$edge[dat$seat==1] <- 1

for (r in 1:n_rows){
  s <- length(which(dat$row==r))
  dat$edge[dat$row==r & dat$seat==s] <- 1
}


# Fill in edges first (whites)
#==============================

edges <- which(dat$edge==1)
for (l in edges){
  pool <- choir
  whites <- which(pool=="B1_w" | pool=="B2_w" | pool=="T1_w" | pool=="T2_w" | pool=="A1_w" | pool=="A2_w" | pool=="A1_w" | pool=="A2_w")
  pool <- pool[whites]
  
  if (length(pool)==0){
    print("CAUTION - MAY CLASH")
    dat$clash[l] <- "YES"
    pool <- choir
  } 
  
  label <- sample(pool, 1)
  dat$label[l] <- label
  dat$voice[l] <- substr(label, 1, 1)
  dat$height[l] <- substr(label, 2, 2)
  dat$colour[l] <- substr(label, 4, 4)
  
  indx <- which(choir==label)[1]
  choir <- choir[-indx]
}

# Fill in front row with yellows and whites
#==========================================

front_row <- which(dat$row==1)

for (l in front_row){
  print(paste("ROW", dat$row[l], "SEAT", dat$seat[l], sep=" "))
  
  if (!is.na(dat$label[l])){
    print("Edge seat- already labelled")
    
  } else {
    
    pool <- choir
    yellows <- which(pool=="B1_y" | pool=="B2_y" | pool=="T1_y" | pool=="T2_y" | pool=="A1_y" | pool=="A2_y" | pool=="S1_y" | pool=="S2_y")
    whites <- which(pool=="B1_w" | pool=="B2_w" | pool=="T1_w" | pool=="T2_w" | pool=="A1_w" | pool=="A2_w" | pool=="S1_w" | pool=="S2_w")
  
    pool <- pool[c(yellows, whites)]
    
    if (length(pool)==0){
      print("CAUTION - MAY CLASH")
      dat$clash[l] <- "YES"
      pool <- choir
    } 
    
    label <- sample(pool, 1)
    dat$label[l] <- label
    dat$voice[l] <- substr(label, 1, 1)
    dat$height[l] <- substr(label, 2, 2)
    dat$colour[l] <- substr(label, 4, 4)
    
    indx <- which(choir==label)[1]
    choir <- choir[-indx]
  }
  
}


# Fill in rest
#===========================

for (l in sample(1:length(dat$label))){
  
  print("===========================", quote=F)
  print(paste("ROW", dat$row[l], "SEAT", dat$seat[l], sep=" "), quote=F)
  print("===========================", quote=F)
 # print(choir)
  
  if (!is.na(dat$label[l])){
    print("Seat already labelled")
    
  } else {
    
    pool <- choir
    print(paste(length(choir), "singers left in choir:", sep=" "), quote=F)
    print("=======", quote=F)
    print(choir, quote=F)
    print(" ", quote=F)
    print(paste("Left seat is", dat$label[l-1], sep=" "), quote=F)
    print(paste("Right seat is", dat$label[l+1], sep=" "), quote=F)
    
    print(" ", quote=F)
    
    
    # don't sit reds next to whites
    #================================

    
    # LEFT SEAT
    #============
    
    if (!is.na(dat$label[l-1])){
      reds <- which(pool=="B1_r" | pool=="B2_r" | pool=="T1_r" | pool=="T2_r" | pool=="A1_r" | pool=="A2_r" | pool=="S1_r" | pool=="S2_r")
      yellows <- which(pool=="B1_y" | pool=="B2_y" | pool=="T1_y" | pool=="T2_y" | pool=="A1_y" | pool=="A2_y" | pool=="S1_y" | pool=="S2_y")
      whites <- which(pool=="B1_w" | pool=="B2_w" | pool=="T1_w" | pool=="T2_w" | pool=="A1_w" | pool=="A2_w" | pool=="S1_w" | pool=="S2_w")
      
      if (dat$colour[l-1]=="w"){
        pool <- pool[c(whites, yellows)]
        } else if (dat$colour[l-1]=="r"){
          pool <- pool[c(yellows, reds)]
          } else if (dat$colour[l-1]=="y"){
            pool <- pool[c(whites, yellows, reds)]
          }
    }
    
    
    # RIGHT SEAT
    if (!is.na(dat$label[l+1])){
      reds <- which(pool=="B1_r" | pool=="B2_r" | pool=="T1_r" | pool=="T2_r" | pool=="A1_r" | pool=="A2_r" | pool=="S1_r" | pool=="S2_r")
      yellows <- which(pool=="B1_y" | pool=="B2_y" | pool=="T1_y" | pool=="T2_y" | pool=="A1_y" | pool=="A2_y" | pool=="S1_y" | pool=="S2_y")
      whites <- which(pool=="B1_w" | pool=="B2_w" | pool=="T1_w" | pool=="T2_w" | pool=="A1_w" | pool=="A2_w" | pool=="S1_w" | pool=="S2_w")
      
      if (dat$colour[l+1]=="w"){
        pool <- pool[c(whites, yellows)]
        } else if (dat$colour[l+1]=="r"){
          pool <- pool[c(yellows, reds)]
          } else if (dat$colour[l+1]=="y"){
            pool <- pool[c(whites, yellows, reds)]
          }
    }
    
    
    
    
    if (length(pool)==0){
      print("MAY HAVE TO SEAT WHITE NEXT TO RED")
    } 
    print("left in pool after white/red exclusions:", quote=F)
    print("=======")
    print(pool)
    
    
    # don't seat same voice parts next to each other
    #=================================================

    # LEFT SEAT
    #============
    
    if (!is.na(dat$label[l-1])){
      basses <- which(pool=="B1_r" | pool=="B1_y" | pool=="B1_w" | pool=="B2_r" | pool=="B2_y" | pool=="B2_w")
      tenors <- which(pool=="T1_r" | pool=="T1_y" | pool=="T1_w" | pool=="T2_r" | pool=="T2_y" | pool=="T2_w")
      altos <- which(pool=="A1_r" | pool=="A1_y" | pool=="A1_w" | pool=="A2_r" | pool=="A2_y" | pool=="A2_w")
      sopranos <- which(pool=="S1_r" | pool=="S1_y" | pool=="S1_w" | pool=="S2_r" | pool=="S2_y" | pool=="S2_w")
    
      if (dat$voice[l-1]=="B"){
        pool <- pool[c(tenors, altos, sopranos)]
      } else if (dat$voice[l-1]=="T"){
        pool <- pool[c(basses, altos, sopranos)]
      } else if (dat$voice[l-1]=="A"){
        pool <- pool[c(basses, tenors, sopranos)]
      } else if (dat$voice[l-1]=="S"){
        pool <- pool[c(basses, tenors, altos)]
      }
    }
    
    # RIGHT SEAT
    #=============
    if (!is.na(dat$label[l+1])){
      basses <- which(pool=="B1_r" | pool=="B1_y" | pool=="B1_w" | pool=="B2_r" | pool=="B2_y" | pool=="B2_w")
      tenors <- which(pool=="T1_r" | pool=="T1_y" | pool=="T1_w" | pool=="T2_r" | pool=="T2_y" | pool=="T2_w")
      altos <- which(pool=="A1_r" | pool=="A1_y" | pool=="A1_w" | pool=="A2_r" | pool=="A2_y" | pool=="A2_w")
      sopranos <- which(pool=="S1_r" | pool=="S1_y" | pool=="S1_w" | pool=="S2_r" | pool=="S2_y" | pool=="S2_w")
    
      if (dat$voice[l+1]=="B"){
        pool <- pool[c(tenors, altos, sopranos)]
      } else if (dat$voice[l+1]=="T"){
        pool <- pool[c(basses, altos, sopranos)]
      } else if (dat$voice[l+1]=="A"){
        pool <- pool[c(basses, tenors, sopranos)]
      } else if (dat$voice[l+1]=="S"){
        pool <- pool[c(basses, tenors, altos)]
      }
    }
    
    print("left in pool after voice part exclusions:", quote=F)
    print("=======")
    print(pool)
    
    if (length(pool)==0){
      print("MAY HAVE TO SEAT SAME VOICE PARTS NEXT TO EACH OTHER")
    } 
    
    #========================================================
    if (length(pool)==0){
      print("CAUTION - MAY CLASH")
      dat$clash[l] <- "YES"
      pool <- choir
    } else if (length(pool)>0){
     # print(paste("Sampling from pool of", length(pool), sep=" "))
    }
    
 #   print(pool)
    
    label <- sample(pool, 1)
    print(label)
    dat$label[l] <- label
    dat$voice[l] <- substr(label, 1, 1)
    dat$height[l] <- substr(label, 2, 2)
    dat$colour[l] <- substr(label, 4, 4)
    
    indx <- which(choir==label)[1]
  
  #  print(paste("index is" ,indx, sep=" "))
  #  print(paste("choir is", choir, sep=" "))
  #  print(paste("pool is", pool, sep=" "))
    
    choir <- choir[-indx]
    
  }
  print("   ", quote=F)
  print("   ", quote=F)
}

length(which(dat$clash=="YES"))


#===========================================================
# corrections




#==========================================================

# create visualisation





