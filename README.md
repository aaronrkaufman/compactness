# compactness
Implements the model from Aaron Kaufman, Gary King, and Mayya Komisarchik. Forthcoming. “How to Measure Legislative District Compactness If You Only Know it When You See It.” American Journal of Political Science. Copy at http://j.mp/2u9OWrG

Install this package with the following:

devtools::install_github("aaronrkaufman/compactness")

Important: our state district identification scheme is structured as state FIPS then chamber then district ID then year, so for example 1_U_007_2010 is Alabama's upper chamber's 7th district for the district cycle that begins in 2010. The chamber part of the ID is either U, L, or C: upper chamber of the state legislature, lower chamber of the state legislature, or federal Congressional district. 
