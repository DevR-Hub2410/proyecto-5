xi <- c(1, 0, 3, 1, 0, 0, 3, 3, 3, 3, 0, 1, 1, 1, 1, 2, 2, 0, 1)

abs_ni <- table(xi)

rel_ni <- abs_ni / length(xi)

abs_acum <- cumsum(abs_ni)

rel_acum <- cumsum(rel_ni)

data.frame(
  abs_ni,
  rel_ni,
  abs_acum,
  rel_acum
)