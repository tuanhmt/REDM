logadd <- function( la, lb )
{
	if( is.infinite(la) )
		return(lb)
	else if( is.infinite(lb) )
		return(la)
	if( la > lb )
		return( la + log(1+exp(lb-la)) )
	lb + log(1+exp(la-lb))
}

