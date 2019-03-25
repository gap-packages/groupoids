##############################################################################
##
#W  testall.g               groupoids Package                    Chris Wensley
##
#Y  Copyright (C) 2000-2019, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

LoadPackage( "groupoids" ); 
dir := DirectoriesPackageLibrary("groupoids","tst");
TestDirectory(dir, rec(exitGAP := true,
    testOptions:=rec(compareFunction := "uptowhitespace")));
FORCE_QUIT_GAP(1);
