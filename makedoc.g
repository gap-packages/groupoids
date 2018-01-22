##  makedoc.g for the package groupoids,
##  This builds the documentation of the groupoids package. 
##  Needs: GAPDoc & AutoDoc packages, latex, pdflatex, mkindex
##  call this with GAP from within the package root directory 

LoadPackage( "groupoids" );
LoadPackage( "AutoDoc" ); 

AutoDoc(rec( 
    scaffold := rec(
        includes := [ "intro.xml",    "mwo.xml",     "mwohom.xml", 
                      "gpd.xml",      "gpdhom.xml",  "ggraph.xml", 
                      "tecnotes.xml", "history.xml" 
                    ],
        bib := "bib.xml", 
        gapdoc_latex_options := rec( EarlyExtraPreamble := """
            \usepackage[all]{xy} 
        """ ),  
        entities := rec( 
            AutoDoc := "<Package>AutoDoc</Package>",
            XMod := "<Package>XMod</Package>"
        )
    )
));

QUIT;
