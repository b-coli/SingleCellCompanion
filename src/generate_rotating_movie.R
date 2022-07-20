generate_rotating_movie <- function(sobj, color_scheme, filename = "movie.gif") {
  metadata <- sobj@meta.data %>% tibble::as_tibble(rownames = "Cell")
  umap <- sobj@reductions$umap_3D@cell.embeddings %>% tibble::as_tibble(rownames = "Cell")
  com <- c(mean(umap$UMAP_1), mean(umap$UMAP_2), mean(umap$UMAP_3))
  cell_data <- dplyr::full_join(umap, metadata) %>%
    dplyr::select(Cell, UMAP3D_1 = UMAP_1, 
                  UMAP3D_2 = UMAP_2, 
                  UMAP3D_3 = UMAP_3, 
                  Cell_Type = celltype.anno) %>% 
    dplyr::mutate(UMAP3D_1 = UMAP3D_1 - com[1],
                  UMAP3D_2 = UMAP3D_2 - com[2],
                  UMAP3D_3 = UMAP3D_3 - com[3])

  cell_mat <- cell_data %>% 
    dplyr::select(Cell, UMAP3D_1, UMAP3D_2, UMAP3D_3) %>% 
    tibble::column_to_rownames("Cell") %>% 
    as.matrix()
  movie_data <- purrr::map_dfr(1:360, .f = function(angle) {
    print(angle)
    frame <- to_grid(cell_mat, angle)
    return(frame)
  })

  movie_data <- dplyr::left_join(movie_data, cell_data)

  movie <- animate_movie(movie_data, color_scheme, filename = filename)
  
  return(movie)
}
