##############################################################################
##
#W  double.tst               Groupoids Package                 Chris Wensley
##
#Y  Copyright (C) 2023, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
gap> START_TEST( "groupoids package: double.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## SubSection 11.1.1 
gap> g := (1,2,3,4);;  h := (1,3);;
gap> gend8 := [ g, h ];;
gap> d8 := Group( gend8 );;
gap> SetName( d8, "d8" ); 
gap> Gd8 := Groupoid( d8, [-9..-7] );;
gap> SetName( Gd8, "Gd8" ); 
gap> D1 := SinglePieceBasicDoubleGroupoid( Gd8 );; 
gap> D1!.groupoid;
Gd8
gap> D1!.objects;
[ -9 .. -7 ]
gap> a1 := Arrow(Gd8,(),-9,-7);;         a2 := Arrow(Gd8,(2,4),-7,-7);;
gap> b1 := Arrow(Gd8,(2,4),-9,-8);;      b2 := Arrow(Gd8,(),-8,-9);; 
gap> c1 := Arrow(Gd8,(1,2)(3,4),-8,-8);; c2 := Arrow(Gd8,(1,4)(2,3),-8,-7);; 
gap> d1 := Arrow(Gd8,g,-9,-9);;          d2 := Arrow(Gd8,(1,3),-9,-8);; 
gap> e1 := Arrow(Gd8,(1,3),-7,-8);;      e2 := Arrow(Gd8,g,-8,-8);; 
gap> f1 := Arrow(Gd8,g^-1,-7,-9);;       f2 := Arrow(Gd8,(1,3),-9,-7);; 

gap> bdy1 := b1![1]^-1 * d1![1]^-1 * a1![1] * e1![1];; 
gap> sq1 := SquareOfArrows( D1, bdy1, a1, d1, e1, b1 ); 
[-9] ---- () ---> [-7]
  |                         |
(1,2,3,4)    (1,4,3,2)    (1,3)
  V                         V
[-9] ---- (2,4) ---> [-8]
gap> UpArrow( sq1 );
[() : -9 -> -7]
gap> LeftArrow( sq1 );
[(1,2,3,4) : -9 -> -9]
gap> RightArrow( sq1 );
[(1,3) : -7 -> -8]
gap> DownArrow( sq1 );
[(2,4) : -9 -> -8]

gap> bdy2 := b2![1]^-1 * e1![1]^-1 * a2![1] * f1![1];; 
gap> sq2 := SquareOfArrows( D1, bdy2, a2, e1, f1, b2 );;  
gap> LeftArrow( sq2 ) = RightArrow( sq1 ); 
true
gap> bdy3 := c1![1]^-1 * d2![1]^-1 * b1![1] * e2![1];; 
gap> sq3 := SquareOfArrows( D1, bdy3, b1, d2, e2, c1 );;
gap> UpArrow( sq3 ) = DownArrow( sq1 ); 
true
gap> bdy4 := c2![1]^-1 * e2![1]^-1 * b2![1] * f2![1];; 
gap> sq4 := SquareOfArrows( D1, bdy4, b2, e2, f2, c2 );;
gap> UpArrow(sq4)=DownArrow(sq2) and LeftArrow(sq4)=RightArrow(sq3); 
true

gap> sq12 := LeftRightProduct( D1, sq1, sq2 );
[-9] ---- (2,4) ---> [-7]
  |                         |
(1,2,3,4)    (1,3)(2,4)    (1,4,3,2)
  V                         V
[-9] ---- (2,4) ---> [-9]

gap> sq34 := LeftRightProduct( D1, sq3, sq4 );
[-9] ---- (2,4) ---> [-9]
  |                         |
(1,3)    (1,3)(2,4)    (1,3)
  V                         V
[-8] ---- (1,3)(2,4) ---> [-7]

gap> sq13 := UpDownProduct( D1, sq1, sq3 ); 
[-9] ---- () ---> [-7]
  |                         |
(1,2)(3,4)    (1,4)(2,3)    (1,4)(2,3)
  V                         V
[-8] ---- (1,2)(3,4) ---> [-8]

gap> sq24 := UpDownProduct( D1, sq2, sq4 ); 
[-7] ---- (2,4) ---> [-7]
  |                         |
(1,4)(2,3)    (1,4,3,2)    (1,4)(2,3)
  V                         V
[-8] ---- (1,4)(2,3) ---> [-7]

gap> sq1324 := LeftRightProduct( D1, sq13, sq24 );
[-9] ---- (2,4) ---> [-7]
  |                         |
(1,2)(3,4)    ()    (1,4)(2,3)
  V                         V
[-8] ---- (1,3)(2,4) ---> [-7]

gap> sq1234 := UpDownProduct( D1, sq12, sq34 );;
gap> sq1324 = sq1234;
true

gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "double.tst", 10000 );
