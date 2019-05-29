##  makedoc.g for the package groupoids,
##  This builds the documentation of the groupoids package. 
##  Needs: GAPDoc & AutoDoc packages, latex, pdflatex, mkindex
##  call this with GAP from within the package root directory 

LoadPackage( "groupoids" );
LoadPackage( "AutoDoc" ); 

AutoDoc(rec( 
    gapdoc := rec( 
        LaTeXOptions := rec( EarlyExtraPreamble := """
            \usepackage[all]{xy} 
            \newcommand{\Aut} {\mathrm{Aut}}
            \newcommand{\AUT} {\mathrm{AUT}}
            \newcommand{\Inn} {\mathrm{Inn}}
        """ )),  
    scaffold := rec(
        includes := [ "intro.xml",    "mwo.xml",     "mwohom.xml", 
                      "gpd.xml",      "gpdhom.xml",  "ggraph.xml", 
                      "tecnotes.xml", "history.xml" 
                    ],
        bib := "bib.xml", 
        entities := rec( 
            AutoDoc := "<Package>AutoDoc</Package>",
            XMod := "<Package>XMod</Package>"
        )
    )
));
