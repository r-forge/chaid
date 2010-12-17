attach(asNamespace("CHAID"))

x <- factor(c(rep(1, 5), rep(2, 400), rep(3, 2595)))
response <- rep(0, 3000)
response[c(1:4, 10:50, 1000:1400)] <- 1
response <- factor(response)
weights <- rep(1, 3000)
ctrl <- chaid_control(minbucket=20)
index <- 1:3

# Here it shouldn't return NULL, though the two most similar levels are big
# enough and not similar enough, because it would result in a small bucket.
levindx <- step2(response, x, weights, index, ctrl)
stopifnot(!is.null(levindx))


# Here it should return 0, not allowing us to select this variable for
# a split, because it would result in a small bucket.
logpval <- step4internal(response, x, weights, index)
stopifnot(logpval == 0)

