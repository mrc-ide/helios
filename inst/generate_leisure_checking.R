# generating the leisure visit list
parameters <- get_parameters()
leisure_visit_list <- generate_initial_leisure(parameters)

## checking every element is 7 in length
x <- unlist(lapply(leisure_visit_list, function(x) length(x)))
table(x)

## filling a matrix with the output so we can examine it further
y <- matrix(data = NA, nrow = parameters$human_population, ncol = 7)
for (i in 1:nrow(y)) {
  y[i, ] <- leisure_visit_list[[i]]
}

## seeing how many people are out on a given day
sum(y[, 1] > 0) / parameters$human_population
sum(y[, 2] > 0) / parameters$human_population
sum(y[, 3] > 0) / parameters$human_population
sum(y[, 4] > 0) / parameters$human_population
sum(y[, 5] > 0) / parameters$human_population
sum(y[, 6] > 0) / parameters$human_population
sum(y[, 7] > 0) / parameters$human_population

## looking at the distribution of where people congregate
hist(table(y[, 1][y[, 1] > 0]))
hist(table(y[, 2][y[, 2] > 0]))
hist(table(y[, 3][y[, 3] > 0]))
hist(table(y[, 4][y[, 4] > 0]))
hist(table(y[, 5][y[, 5] > 0]))
hist(table(y[, 6][y[, 6] > 0]))
hist(table(y[, 7][y[, 7] > 0]))

## checking the length of the vector of where people visit (most places occupied on any given night)
length(table(y[, 1][y[, 1] > 0]))
length(table(y[, 2][y[, 2] > 0]))
length(table(y[, 3][y[, 3] > 0]))
length(table(y[, 4][y[, 4] > 0]))
length(table(y[, 5][y[, 5] > 0]))
length(table(y[, 6][y[, 6] > 0]))
length(table(y[, 7][y[, 7] > 0]))

x <- create_variables(parameters)
x$leisure
