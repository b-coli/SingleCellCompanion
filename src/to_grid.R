to_grid <- function(mat, angle) {
  mat <- rotate_3dxy(mat, angle * pi/180)
  tr_mat <- plot3D::persp3D(z = mat, theta = 0, 
                            phi = 0, xlim = c(-20, 20), 
                            ylim = c(-20,20), zlim = c(-20,20),
                            plot = FALSE)
  trans <- trans3d(mat[, 1], mat[, 2], mat[, 3], tr_mat) %>%
    as.data.frame() %>%
    tibble::as_tibble(rownames = "Cell") %>%
    dplyr::mutate(depth = -mat[,2]) %>%
    dplyr::mutate(angle = angle)
}
