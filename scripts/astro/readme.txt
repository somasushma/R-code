* copy script and all data files into the same directory
* setwd() to above directory
* Run the entire script to initialized
* then run sky2map() with any of the below options
* coords= "galc" for galactic; default is RA-DE for Julian 2000
* projection= "ham.ait" default; or give one of the following options:  "lam" (Lambert), "equirect" (equirectangular), 
"eckert4" (Eckert IV),"winkel3" (Winkel Tripel), "gott.m" (Gott-Mugnolo)
* object= one or more of the following "ecli": ecliptic "mwo": Milky Way Objects; 
messier: Messier; "constf": constellation figures; "bstars": bright stars

* Messier includes Messier+ bright NGC

* e.g. sky2map(projection = "winkel3")
