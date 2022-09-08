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

## make independant of gpd.tst 
gap> s4 := Group( (1,2,3,4), (3,4) );; 
gap> d8 := Subgroup( s4, [ (1,2,3,4), (1,3) ] );;
gap> SetName( s4, "s4" );  SetName( d8, "d8" ); 
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );;

## SubSection 7.1.1 
gap> Dd8 := SinglePieceDoubleGroupoid( Gd8, d8 );; 
gap> Dd8!.groupoid;
single piece groupoid: < d8, [ -9, -8, -7 ] >
gap> Dd8!.group;
d8



gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "double.tst", 10000 );
