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
gap> Gs4 := SinglePieceGroupoid( s4, [-19 .. -15] );;  
gap> d8 := Subgroup( s4, [ (1,2,3,4), (1,3) ] );;
gap> SetName( d8, "d8" ); 
gap> Gd8 := Groupoid( d8, [-12 .. -7] );;
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
gap> a := Arrow( Gd8, (1,2,3,4), -12, -11 );; 
gap> b := Arrow( Gd8, (1,3), -11, -10 );;
gap> c := Arrow( Gd8, (1,4)(2,3), -10, -9 );; 
gap> d := Arrow( Gd8, (1,2,3,4), -12, -8 );; 
gap> e := Arrow( Gd8, (2,4) , -8, -7 );; 
gap> f := Arrow( Gd8, (1,2)(3,4), -7, -9 );; 

gap> g1 := (1,2,3);; 
gap> sq1 := MultiplicativeSquareWithObjects( Dd8, g1, a*b, d*e, c, f );; 
gap> ElementOfSquare( sq1 ); 
(1,2,3)
gap> UpArrow( sq1 );
[(1,2)(3,4) : -12 -> -10]
gap> LeftArrow( sq1 );
[(1,4)(2,3) : -12 -> -7]
gap> RightArrow( sq1 );
[(1,4)(2,3) : -10 -> -9]
gap> DownArrow( sq1 );
[(1,2)(3,4) : -7 -> -9]

gap> g2 := (2,3,4);; 
gap> sq2 := MultiplicativeSquareWithObjects( Dd8, g2, b^-1, c, a^-1*d, f^-1*e^-1 );;
gap> UpArrow( sq2 ); 
[(1,3) : -10 -> -11]
gap> LeftArrow( sq2 ) = RightArrow( sq1 ); 
true
gap> RightArrow( sq2 ); 
[() : -11 -> -8]
gap> DownArrow( sq2 ); 
[(1,4,3,2) : -9 -> -8]

gap> g3 := (1,3,4);; 
gap> sq3 := MultiplicativeSquareWithObjects( Dd8, g3, f, e^-1, c^-1*b^-1, d^-1*a );;
gap> UpArrow( sq3 ) = DownArrow( sq1 ); 
true
gap> LeftArrow( sq3 ); 
[(2,4) : -7 -> -8]
gap> RightArrow( sq3 ); 
[(1,4,3,2) : -9 -> -11]
gap> DownArrow( sq3 );
[() : -8 -> -11]

gap> g4 := (1,2,4);; 
gap> sq4 := MultiplicativeSquareWithObjects( Dd8, g4, f^-1*e^-1, c^-1*b^-1, d^-1, a^-1 );;
gap> UpArrow( sq4 ) = DownArrow( sq2 ); 
true
gap> LeftArrow( sq4 ) = RightArrow( sq3 ); 
true
gap> RightArrow( sq4 ); 
[(1,4,3,2) : -8 -> -12]
gap> DownArrow( sq4 );
[(1,4,3,2) : -11 -> -12]

gap> sq12 := LeftRightProduct( Dd8, sq1, sq2 );
[-12] ---- (1,2,3,4) ---> [-11]
  |                         |
(1,4)(2,3)    (1,3,4)    ()
  V                         V
[-7] ---- (2,4) ---> [-8]

gap> sq34 := LeftRightProduct( Dd8, sq3, sq4 );
[-7] ---- (2,4) ---> [-8]
  |                         |
(2,4)    (1,2,3)    (1,4,3,2)
  V                         V
[-8] ---- (1,4,3,2) ---> [-12]

gap> sq13 := UpDownProduct( Dd8, sq1, sq3 ); 
[-12] ---- (1,2)(3,4) ---> [-10]
  |                         |
(1,2,3,4)    (1,3)(2,4)    (1,3)
  V                         V
[-8] ---- () ---> [-11]

gap> sq24 := UpDownProduct( Dd8, sq2, sq4 ); 
[-10] ---- (1,3) ---> [-11]
  |                         |
(1,3)    (1,3)(2,4)    (1,4,3,2)
  V                         V
[-11] ---- (1,4,3,2) ---> [-12]

gap> sq1324 := LeftRightProduct( Dd8, sq13, sq24 );
[-12] ---- (1,2,3,4) ---> [-11]
  |                         |
(1,2,3,4)    ()    (1,4,3,2)
  V                         V
[-8] ---- (1,4,3,2) ---> [-12]

gap> sq1234 := UpDownProduct( Dd8, sq12, sq34 ); 
[-12] ---- (1,2,3,4) ---> [-11]
  |                         |
(1,2,3,4)    (1,3)(2,4)    (1,4,3,2)
  V                         V
[-8] ---- (1,4,3,2) ---> [-12]

gap> sq1324 = sq1234;
false 

gap> U3;
groupoid with 3 pieces:
[ Gs4, Gd8, Gc6 ]
gap> Ds4 := SinglePieceDoubleGroupoid( Gs4, s4 );; 
gap> Dc6 := SinglePieceDoubleGroupoid( Gc6, s4 );; 
gap> DU3 := UnionOfPieces( [ Ds4, Dd8, Dc6 ] ); 
magma with objects having 3 pieces :-
1:  groupoid = Gs4
    group = s4
  objects = [ -19 .. -15 ]
2:  groupoid = Gd8
    group = s4
  objects = [ -12 .. -7 ]
3:  groupoid = Gc6
    group = s4
  objects = [ -6 ]

gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "double.tst", 10000 );
