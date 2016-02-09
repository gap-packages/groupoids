##############################################################################
##
#W  testall.g                     Gpd Package                    Chris Wensley
##
##  version 1.42, 09/02/2016   
##
#Y  Copyright (C) 2000-2016, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

LoadPackage( "gpd" ); 

TestMyPackage := function( pkgname )
    local  pkgdir, testfiles, testresult, ff, fn;
    LoadPackage( pkgname );
    pkgdir := DirectoriesPackageLibrary( pkgname, "tst" );
    # Arrange chapters as required
    testfiles := 
        [ "mwo.tst", "mwohom.tst", "gpd.tst", "gpdhom.tst", "ggraph.tst" ];
    testresult:=true;
    for ff in testfiles do
        fn := Filename( pkgdir, ff );
        Print( "#I  Testing ", fn, "\n" );
        if not Test( fn, rec(compareFunction := "uptowhitespace") ) then
            testresult:=false;
        fi;
    od;
    if testresult then
        Print("#I  No errors detected while testing package ", pkgname, "\n");
    else
        Print("#I  Errors detected while testing package ", pkgname, "\n");
    fi;
end;

##  Set the name of the package here
TestMyPackage( "gpd" );
