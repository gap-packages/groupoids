##############################################################################
##
#W  double.tst                 Groupoids Package                 Chris Wensley
##
#Y  Copyright (C) 2000-2022, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
gap> START_TEST( "groupoids package: double.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## SubSection 7.1.1 
gap> Dd8 := DoubleGroupoid( Gd8 ); 



gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "double.tst", 10000 );
