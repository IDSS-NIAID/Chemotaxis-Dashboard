# convolution_illustration.R
# generation of convolution illustrations for visual_tour.key

library(caTools)
library(dplyr)
library(stringr)
library(purrr)

root <- system('git rev-parse --show-toplevel', intern = TRUE)


##############
# raw images #
##############

# fetch tracks from Biowulf
# data: /data/IDSS_projects/liuy5/raw/20180215
# (going with Channel 6 - convolve frame 0053.jpg)
channel <- 'CH6_RT_C5a'
frame <- '0053.jpg'

data_dir <- file.path(root, 'docs', 'img', '20180215', channel)

f <- paste('ls', data_dir, '| grep jpg') %>%
  system(intern = TRUE)

#  (pause at `frame`)
pause_here <- which(f == frame)

# create data_dir/CH6.gif: convert -resize 50% -delay 10 *.jpg CH6.gif


############
# convolve #
############

# not sure why 'grey' doesn't work, but let's make our own...
grey.scale <- rgb(0:255, 0:255, 0:255, maxColorValue = 255)

#' convolve
#' Take an image and compute convolutions using `mask`, taking `n_frames` snapshots as it moves through `mat`
#' 
#' @param mat A 2D matrix of pixel intensities for convolution (assuming pixel intensities are in 0:255)
#' @param mask A 2D matrix to use in convolutions of `mat`. Default is to run a vertical convolution. Transpose `mat` to do a horizontal convolution, or supply an alternate mask.
#' @param trim Numeric value, number of pixels to trim around the outside of `retval` prior to returning
#' @param n_frames Numeric value, number of frames to include in the animated gif
#' @param correction Numeric value, expected maximum value of the convoluted matrix - need to normalize to `[0:255]`, with the `correction` being mapped to 255. Leaving at 255 (default) will result in no correction.
#' @param transpose logical value, when TRUE the convolution will run by column, rather than by row
convolve <- function(mat, mask = rbind(rep(-1, 3), rep(0, 3), rep(1, 3)), 
                     trim = 1, n_frames = 300, correction = 255,
                     transpose = FALSE)
{
  retval <- array(0, dim = c(nrow(mat), ncol(mat), n_frames))
  retval[,,1] <- mat
  
  # row = x, col = y, frame = z
  if(transpose)
  {
    frames <- tibble(x = rep(1:dim(retval)[1], dim(retval)[2]),
                     y = rep(1:dim(retval)[2], each = dim(retval)[1]),
                     snapshot = 1:length(x) %in% round(seq(from = 1, to = length(x), length.out = dim(retval)[3])),
                     z = c(1, (cumsum(snapshot) + 1)[-length(x)]))
    
    mask <- t(mask)
  }else{
    frames <- tibble(x = rep(1:dim(retval)[1], each = dim(retval)[2]),
                     y = rep(1:dim(retval)[2], dim(retval)[1]),
                     snapshot = 1:length(x) %in% round(seq(from = 1, to = length(x), length.out = dim(retval)[3])),
                     z = c(1, (cumsum(snapshot) + 1)[-length(x)]))
  }

  # add padding
  mat <- rbind(0, cbind(0, mat, 0), 0)

  # scale function
  scale_fun <- function(x)
  {
    # straight correction
    x <- x / correction * 255
  }
  
  for(i in 2:dim(frames)[1])
  {
    if(frames$snapshot[i-1])
    {
      # move progress from the previous frame up to this one
      retval[,,frames$z[i]] <- retval[,,frames$z[i - 1]]
    }
      
    retval[frames$x[i],
           frames$y[i],
           frames$z[i]] <- c(mat[frames$x[i] + 0:2,
                                 frames$y[i] + 0:2] * mask) %>%
      sum() %>%
      abs() %>%
      scale_fun()
  }

  # trim
  retval <- retval[(1+trim):(dim(retval)[1]-trim),
                   (1+trim):(dim(retval)[2]-trim),]
  
  # return
  retval
}


##################
# Generate movie #
##################

ori <- readJPEG(file.path(data_dir, f[pause_here])) %>%
  apply(1:2, mean)

# vertical convolution
mov <- convolve(ori*255)

write.gif(mov, file.path(root, 'docs', 'img', 'conv_vertical.gif'), col = grey.scale)
write.gif(mov[,,dim(mov)[3]], file.path(root, 'docs', 'img', 'conv_v_stop.gif'), col = grey.scale)

# horizontal convolution
mov <- convolve(ori*255, transpose = TRUE)

write.gif(mov, file.path(root, 'docs', 'img', 'conv_horizontal.gif'), col = grey.scale)
write.gif(mov[,,dim(mov)[3]], file.path(root, 'docs', 'img', 'conv_h_stop.gif'), col = grey.scale)






