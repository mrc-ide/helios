library(tictoc); library(dqrng)

tic()
leisure_visit <- vector(mode = "numeric", length = parameters_list$human_population)
for (i in seq(parameters_list$human_population)) {
  leisure_visit[i] <- sample(x = leisure_indvidual_possible_visits_list[[i]], size = 1)
}
toc()

tic()
leisure_visit <- vector(mode = "numeric", length = parameters_list$human_population)
for (i in seq(parameters_list$human_population)) {
  leisure_visit[i] <- leisure_indvidual_possible_visits_list[[i]][sample.int(n = 7, size = 1)]
}
toc()

tic()
leisure_visit <- vapply(leisure_indvidual_possible_visits_list, function(x) sample(x, size = 1), numeric(1))
toc()

bench::mark(leisure_indvidual_possible_visits_list[[i]][sample.int(n = 7, size = 1)],
            sample(x = leisure_indvidual_possible_visits_list[[i]], size = 1),
            leisure_indvidual_possible_visits_list[[i]][dqsample.int(n = 7, size = 1)],
            dqsample(x = leisure_indvidual_possible_visits_list[[i]], size = 1),
            check=FALSE, relative=TRUE,
            max_iterations = 10^6)


                  sample.int(1e4*m, n, replace = TRUE),
                  dqsample.int(m, n, replace = TRUE),
                  dqsample.int(1e4*m, n, replace = TRUE),
                  check = FALSE)
