############################################################################
##
#W  testall.g               groupoids Package                  Chris Wensley
##

LoadPackage( "groupoids" ); 
TestDirectory( 
    [ DirectoriesPackageLibrary( "groupoids", "tst/manual" ), 
      DirectoriesPackageLibrary( "groupoids", "tst/extra" ) ], 
    rec( exitGAP := true,
         testOptions := rec(compareFunction := "uptowhitespace") ) 
    );
FORCE_QUIT_GAP(1);
