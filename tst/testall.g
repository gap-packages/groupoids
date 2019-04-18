##############################################################################
##
#W  testall.g               groupoids Package                    Chris Wensley
##
#Y  Copyright (C) 2000-2019, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

LoadPackage( "groupoids" ); 
TestDirectory( 
    [ DirectoriesPackageLibrary( "groupoids", "tst/manual" ), 
      DirectoriesPackageLibrary( "groupoids", "tst/extra" ) ], 
    rec( exitGAP := true,
         testOptions := rec(compareFunction := "uptowhitespace") ) 
    );
FORCE_QUIT_GAP(1);
