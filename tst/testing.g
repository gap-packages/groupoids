############################################################################
##
#W  testing.g               groupoids Package                  Chris Wensley
##

LoadPackage( "groupoids" );

pkgname := "groupoids"; 
pkgdir := DirectoriesPackageLibrary( pkgname, "tst/manual" ); 
## testing manual examples 
testmanual := 
    [ "mwo.tst",  "mwohom.tst",  "gpd.tst",  "gpdhom.tst",  "gpdaut.tst", 
      "ggraph.tst",  "double.tst" ];
testresult := true;
for ff in testmanual do
    fn := Filename( pkgdir, ff );
    Print( "#I  Testing ", fn, "\n" );
    if not Test( fn, rec(compareFunction := "uptowhitespace", 
                            showProgress := true ) ) then
        testresult := false;
    fi;
od;
if testresult then
    Print( "#I  No errors detected while testing manual examples in package ", 
           pkgname, "\n" );
else
    Print( "#I  Errors detected while testing manual examples in package ", 
           pkgname, "\n" );
fi; 
## testing extra examples
pkgdir := DirectoriesPackageLibrary( pkgname, "tst/extra" );
testextra := [ "autos.tst",    "autos2.tst",   "cosets.tst",  "homs.tst", 
               "isos.tst",     "nicemap.tst",  "rt-act.tst",  "subgpds.tst", 
               "gpd-isos.tst", "auto-gpd.tst" ];
testresult := true;
for ff in testextra do
    fn := Filename( pkgdir, ff );
    Print( "#I  Testing ", fn, "\n" );
    if not Test( fn, rec(compareFunction := "uptowhitespace", 
                            showProgress := true ) ) then
        testresult := false;
    fi;
od;
if testresult then
    Print( "#I  No errors detected while testing extra examples in package ", 
           pkgname, "\n" );
else
    Print( "#I  Errors detected while testing extra examples in package ", 
           pkgname, "\n" );
fi; 
