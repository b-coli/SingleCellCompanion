rotate_3dxy <- function(mat, angle) {
  rot_mat_x <- rbind(c(1, 0, 0),
                     c(0, cos(angle), -sin(angle)),
                     c(0, sin(angle), cos(angle)))
  rot_mat_y <- rbind(c(cos(angle), 0, sin(angle) ),
                     c(0, 1, 0),
                     c(-sin(angle), 0, cos(angle)))
  return(mat %*% rot_mat_x %*% rot_mat_y)
}
