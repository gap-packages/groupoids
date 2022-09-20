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
gap> SetName( s4, "s4" ); 
gap> Gs4 := SinglePieceGroupoid( s4, [-15 .. -11] );;  
gap> d8 := Subgroup( s4, [ (1,2,3,4), (1,3) ] );;
gap> SetName( d8, "d8" ); 
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );;
gap> c6 := Group( (5,6,7)(8,9) );;
gap> SetName( c6, "c6" );
gap> Gc6 := DomainWithSingleObject( c6, -6 );;
gap> SetName( Gs4, "Gs4" ); 
gap> SetName( Gd8, "Gd8" ); 
gap> SetName( Gc6, "Gc6" );  
gap> U3 := UnionOfPieces( [ Gc6, Gd8, Gs4 ] );;

## SubSection 7.1.1 
gap> Dd8 := SinglePieceDoubleGroupoid( Gd8, s4 );; 
gap> Dd8!.groupoid;
Gd8
gap> Dd8!.group;
s4
gap> a := Arrow( Gd8, (2,4), -9, -8 );; 
gap> b := Arrow( Gd8, (1,3), -8, -7 );;
gap> c := Arrow( Gd8, (1,2,3,4), -9, -8 );; 
gap> d := Arrow( Gd8, (1,4,3,2), -8, -7 );; 
gap> e1 := (1,2,3);; 
gap> s1 := MultiplicativeSquareWithObjects( Dd8, e1, b, c, a, d );; 
gap> ElementOfSquare( s1 ); 
(1,2,3)
gap> UpArrow( s1 );
[(2,4) : -9 -> -8]
gap> LeftArrow( s1 );
[(1,2,3,4) : -9 -> -8]
gap> RightArrow( s1 );
[(1,4,3,2) : -8 -> -7]
gap> DownArrow( s1 );
[(1,3) : -8 -> -7]
gap> ad := a*d;
[(1,4)(2,3) : -9 -> -7]
gap> cb := c*b;
[(1,2)(3,4) : -9 -> -7]
gap> e2 := (2,3,4);; 
gap> s2 := MultiplicativeSquareWithObjects( Dd8, e2, ad^-1, d, b, cb^-1 );;
gap> RightArrow( s2 ); 
[(1,2)(3,4) : -7 -> -9]
gap> DownArrow( s2 ); 
[(1,4)(2,3) : -7 -> -9]
gap> e3 := (1,3,4);; 
gap> s3 := MultiplicativeSquareWithObjects( Dd8, e3, cb^-1, d, b, ad^-1 );;
gap> RightArrow( s3 ); 
[(1,4)(2,3) : -7 -> -9]
gap> DownArrow( s3 );
[(1,2)(3,4) : -7 -> -9]

gap> s12 := LeftRightProduct( Dd8, s1, s2 );
[() : [(1,2,3,4) : -8 -> -9] -> [(1,2,3,4) : -9 -> -8]]
gap> ElementOfSquare( s12 );
()
gap> UpArrow( s12 );
[(1,3)(2,4) : -9 -> -7]
gap> LeftArrow( s12 );
[(1,2,3,4) : -9 -> -8]
gap> DownArrow( s12 );
[(1,2,3,4) : -8 -> -9]
gap> RightArrow( s12 );
[(1,2)(3,4) : -7 -> -9]
gap> s13 := UpDownProduct( Dd8, s1, s3 );;
gap> ElementOfSquare( s13 );              
(1,2,4)
gap> UpArrow( s13 );                      
[(2,4) : -9 -> -8]
gap> LeftArrow( s13 );                    
[() : -9 -> -7]
gap> DownArrow( s13 );                    
[(1,2)(3,4) : -7 -> -9]
gap> RightArrow( s13 );                   
[(2,4) : -8 -> -9]

gap> U3;
groupoid with 3 pieces:
[ Gs4, Gd8, Gc6 ]
gap> Ds4 := SinglePieceDoubleGroupoid( Gs4, s4 );; 
gap> Dc6 := SinglePieceDoubleGroupoid( Gc6, s4 );; 
gap> DU3 := UnionOfPieces( [ Ds4, Dd8, Dc6 ] ); 
magma with objects having 3 pieces :-
1:  groupoid = Gs4
    group = s4
  objects = [ -15 .. -11 ]
2:  groupoid = Gd8
    group = s4
  objects = [ -9, -8, -7 ]
3:  groupoid = Gc6
    group = s4
  objects = [ -6 ]

gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "double.tst", 10000 );
