tickfilter_b <- function (u) 
{
    c(floor(median(u)) - 3, floor(median(u)) - 2, floor(median(u)) - 
        1, max(u) - 2, max(u) - 1, max(u))
}
