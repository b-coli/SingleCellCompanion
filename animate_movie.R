
animate_movie <- function(movie_data, color_scheme, filename = "movie.gif") {
  anim <- ggplot2::ggplot(movie_data %>% 
			  dplyr::mutate(Cell_Type = if_else(Cell_Type == "Putative Quiescent Center", 
							    "Quiescent Center", 
							    as.character(Cell_Type))) %>% 
			  dplyr::arrange(depth), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(ggplot2::aes(size = depth, color = Cell_Type)) +
    ggplot2::coord_fixed() + 
    ggplot2::scale_color_manual(values = color_scheme) +
    ggplot2::scale_size_continuous(range = c(0.02, 0.1), guide = FALSE) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 2))) +
    gganimate::transition_states(angle, transition_length = 1, state_length = 0) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "black"),
      panel.background = ggplot2::element_rect(fill = 'black'),
      legend.background = ggplot2::element_rect(fill = "black", color = NA),
      legend.key = ggplot2::element_rect(color = "black", fill = "black"),
      legend.title = ggplot2::element_text(color = "white"),
      legend.text = ggplot2::element_text(color = "white", size = 16)
    )
  nframes <- movie_data %>% pull(angle) %>% unique() %>% length()
  movie = gganimate::animate(anim, nframes = nframes, fps = 25, width = 1200, height = 800, 
                             renderer = gganimate::magick_renderer())
  movie <- gganimate::anim_save(filename = filename, animation = movie, path = ".")
  return(movie)
}
