##############################################################################
##
#W  testing.g               groupoids Package                    Chris Wensley
##
#Y  Copyright (C) 2000-2018, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

LoadPackage( "groupoids" );

pkgname := "groupoids"; 
pkgdir := DirectoriesPackageLibrary( pkgname, "tst" );
testfiles := 
    [ "mwo.tst", "mwohom.tst", "gpd.tst", "gpdhom.tst", "gpdaut.tst", 
      "ggraph.tst" ];
testresult := true;
for ff in testfiles do
    fn := Filename( pkgdir, ff );
    Print( "#I  Testing ", fn, "\n" );
    if not Test( fn, rec(compareFunction := "uptowhitespace", 
                            showProgress := true ) ) then
        testresult := false;
    fi;
od;
if testresult then
    Print("#I  No errors detected while testing package ", pkgname, "\n");
else
    Print("#I  Errors detected while testing package ", pkgname, "\n");
fi;
