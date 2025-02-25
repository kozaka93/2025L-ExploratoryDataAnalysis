install.packages("proton")
library(proton)
proton()

?plot
x <- c(1, 2, 3)

rep(x, times = 2)
rep(x, each = 2)

cars[,1]
cars["speed car"]
cars$`speed car`


l <- list(name = "Jan", surname = "Wspaniały", no.children = 2, names.children = c("Asia", "Kuba"))
l

l$name
l$no.children
l[[1]]
l[1]


person <- c("Ania", "Hubert")
car_model <- c("Maluch", "Ferrari")
status <- c("Msc", "Bsc")
happy <- c(TRUE, FALSE)
df <- data.frame(person = person, car_model = car_model, status = status, happy = happy)




# 
# Pietraszko uses a password which is very difficult to guess.
# At first, try to hack an account of a person which is not as cautious as Pietraszko.
# 
# But who is the weakest point? Initial investigation suggests that John Insecure doesn't care about security and has an account on the Proton server. He may use a password which is easy to crack.
# Let's attack his account first!
#   
#   Problem 1: Find the login of John Insecure.
# 
# Bit has scrapped 'employees' data (names and logins) from the www web page of Technical University of Warsaw. The data is in the data.frame `employees`. 
# Now, your task is to find John Insecure's login.
# When you finally find out what John's login is, use `proton(action = "login", login="XYZ")` command, where XYZ is Insecure's login.