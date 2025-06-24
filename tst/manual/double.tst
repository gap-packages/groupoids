############################################################################
##
#W  double.tst               Groupoids Package                 Chris Wensley
##

gap> START_TEST( "groupoids package: double.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## make double.tst independent of gpd.tst 
gap> g := (1,2,3,4);;  h := (1,3);;
gap> gend8 := [ g, h ];;
gap> d8 := Group( gend8 );;
gap> Gd8 := Groupoid( d8, [-9..-7] );;
gap> SetName( d8, "d8" );  SetName( Gd8, "Gd8" ); 
gap> c6 := Group( (5,6,7)(8,9) );;
gap> Gc6 := MagmaWithSingleObject( c6, -6 );;
gap> SetName( c6, "c6" );  SetName( Gc6, "Gc6" );
gap> s4 := Group( (1,2,3,4), (3,4) );; 
gap> Gs4 := SinglePieceGroupoid( s4, [-15 .. -11] );;
gap> SetName( s4, "s4" );  SetName( Gs4, "Gs4" );
gap> q8 := QuaternionGroup( 8 );;
gap> genq8 := GeneratorsOfGroup( q8 );;
gap> x := genq8[1];;  y := genq8[2];;
gap> Gq8 := Groupoid( q8, [ -18, -17 ] );;
gap> SetName( q8, "q8" );  SetName( Gq8, "Gq8" );
gap> V4 := UnionOfPieces( [ Gs3, Gq8, Gd8, Gc6 ] );;

## SubSection 7.1.1 
gap> Dd8 := SinglePieceBasicDoubleGroupoid( Gd8 );; 
gap> Dd8!.groupoid;
Gd8
gap> Dd8!.objects;
[ -9 .. -7 ]
gap> [ Size(Dd8), (3*8)^4 ]; 
[ 331776, 331776 ]
gap> SetName( Dd8, "Dd8" );
gap> a1 := Arrow( Gd8, (1,3), -7, -8 );;
gap> b1 := Arrow( Gd8, (2,4), -7, -7 );;
gap> c1 := Arrow( Gd8, (1,2)(3,4), -8, -9 );;
gap> d1 := Arrow( Gd8, g, -7, -9 );;
gap> bdy1 := d1^-1 * b1^-1 * a1 * c1;
[(2,4) : -9 -> -9]
gap> sq1 := SquareOfArrows( Dd8, a1, b1, c1, d1 ); 
[-7] ------- (1,3) ------> [-8]
  |                          |
(2,4)        (2,4)        (1,2)(3,4)
  V                          V
[-7] ----- (1,2,3,4) ----> [-9]
gap> sq1 in Dd8;
true
gap> UpArrow( sq1 );
[(1,3) : -7 -> -8]
gap> LeftArrow( sq1 );
[(2,4) : -7 -> -7]
gap> RightArrow( sq1 );
[(1,2)(3,4) : -8 -> -9]
gap> DownArrow( sq1 );
[(1,2,3,4) : -7 -> -9]
gap> BoundaryOfSquare( sq1 );
[(2,4) : -9 -> -9]
gap> DoubleGroupoidOfSquare( sq1 );
Dd8

## SubSection 7.1.2
gap> a2 := Arrow( Gd8, (2,4), -8, -9 );;
gap> c2 := Arrow( Gd8, (1,3)(2,4), -9, -8);;
gap> d2 := Arrow( Gd8, g, -9, -8 );; 
gap> sq2 := SquareOfArrows( Dd8, a2, c1, c2, d2 );
[-8] -------- (2,4) -------> [-9]
  |                            |
(1,2)(3,4)        ()        (1,3)(2,4)
  V                            V
[-9] ------ (1,2,3,4) -----> [-8]
gap> bdy2 := BoundaryOfSquare( sq2 );
[() : -8 -> -8]
gap> [ IsCommutingSquare(sq1), IsCommutingSquare(sq2) ]; 
[ false, true ]

## SubSection 7.1.3
gap> tsq1 := TransposedSquare( sq1 );
[-7] ------- (2,4) ------> [-7]
  |                         |
(1,3)        (2,4)        (1,2,3,4)
  V                         V
[-8] ---- (1,2)(3,4) ---> [-9]
gap> IsClosedUnderTransposition( sq1 );  
false

## SubSection 7.1.4
gap> LeftArrow( sq2 ) = RightArrow( sq1 ); 
true
gap> sq12 := HorizontalProduct( sq1, sq2 );
[-7] ----- (1,3)(2,4) ----> [-9]
  |                          |
(2,4)        (1,3)        (1,3)(2,4)
  V                          V
[-7] ----- (1,3)(2,4) ----> [-8]
gap> bdy12 := BoundaryOfSquare( sq12 );
[(1,3) : -8 -> -8]
gap> (bdy1^d2) * bdy2 = bdy12;
true

## SubSection 7.1.5
gap> b3 := Arrow( Gd8, (1,3), -7, -9 );;
gap> c3 := Arrow( Gd8, (2,4), -9, -8);;
gap> d3 := Arrow( Gd8, (1,4)(2,3), -9, -8 );; 
gap> sq3 := SquareOfArrows( Dd8, d1, b3, c3, d3 );
[-7] ---- (1,2,3,4) ---> [-9]
  |                         |
(1,3)      (2,4)      (2,4)
  V                         V
[-9] ---- (1,4)(2,3) ---> [-8]
gap> bdy3 := BoundaryOfSquare( sq3 );
[(2,4) : -8 -> -8]
gap> UpArrow( sq3 ) = DownArrow( sq1 ); 
true
gap> sq13 := VerticalProduct( sq1, sq3 );
[-7] -------- (1,3) -------> [-8]
  |                           |
(1,3)(2,4)        ()        (1,4,3,2)
  V                           V
[-9] ----- (1,4)(2,3) ----> [-8]

gap> c4 := Arrow( Gd8, (1,2,3,4), -8, -7);;
gap> d4 := Arrow( Gd8, (1,2)(3,4), -8, -7 );; 
gap> sq4 := SquareOfArrows( Dd8, d2, c3, c4, d4 );
[-9] ------- (1,2,3,4) ------> [-8]
  |                             |
(2,4)        (1,2,3,4)        (1,2,3,4)
  V                             V
[-8] ------ (1,2)(3,4) -----> [-7]
gap> UpArrow( sq4 ) = DownArrow( sq2 );
true
gap> LeftArrow( sq4 ) = RightArrow( sq3 ); 
true

gap> sq34 := HorizontalProduct( sq3, sq4 );
[-7] ------- (1,3)(2,4) ------> [-8]
  |                              |
(1,3)        (1,4)(2,3)        (1,2,3,4)
  V                              V
[-9] ------- (1,3)(2,4) ------> [-7]

gap> sq1234 := VerticalProduct( sq12, sq34 );
[-7] --------- (1,3)(2,4) --------> [-9]
  |                                  |
(1,3)(2,4)        (1,2,3,4)        (1,4,3,2)
  V                                  V
[-9] --------- (1,3)(2,4) --------> [-7]

gap> sq24 := VerticalProduct( sq2, sq4 ); 
[-8] ----------- (2,4) ----------> [-9]
  |                                 |
(1,4,3,2)        (1,2,3,4)        (1,4,3,2)
  V                                 V
[-8] -------- (1,2)(3,4) -------> [-7]

gap> sq1324 := HorizontalProduct( sq13, sq24 );;
gap> sq1324 = sq1234;
true

## SubSection 7.1.6
gap> hid := HorizontalIdentities( sq24 );;
gap> hid[1]; Print("\n"); hid[2];                    
[-8] --------- () --------> [-8]
  |                          |
(1,4,3,2)        ()        (1,4,3,2)
  V                          V
[-8] --------- () --------> [-8]

[-9] --------- () --------> [-9]
  |                          |
(1,4,3,2)        ()        (1,4,3,2)
  V                          V
[-7] --------- () --------> [-7]
gap> HorizontalProduct( hid[1], sq24 ) = sq24;
true
gap> HorizontalProduct( sq24, hid[2] ) = sq24;      
true

gap> vid := VerticalIdentities( sq24 );;  
gap> vid[1]; Print("\n"); vid[2];                    
[-8] ---- (2,4) ---> [-9]
  |                   |
()         ()         ()
  V                   V
[-8] ---- (2,4) ---> [-9]

[-8] ---- (1,2)(3,4) ---> [-7]
  |                        |
()            ()            ()
  V                        V
[-8] ---- (1,2)(3,4) ---> [-7]
gap> VerticalProduct( vid[1], sq24 ) = sq24;
true
gap> VerticalProduct( sq24, vid[2] ) = sq24;
true

gap> hinv := HorizontalInverse( sq24 ); 
[-9] ----------- (2,4) ----------> [-8]
  |                                 |
(1,4,3,2)        (1,2,3,4)        (1,4,3,2)
  V                                 V
[-7] -------- (1,2)(3,4) -------> [-8]
gap> HorizontalProduct( hinv, sq24 ) = hid[2];
true
gap> HorizontalProduct( sq24, hinv ) = hid[1];      
true

gap> vinv := VerticalInverse( sq24 );
[-8] -------- (1,2)(3,4) -------> [-7]
  |                                 |
(1,2,3,4)        (1,4,3,2)        (1,2,3,4)
  V                                 V
[-8] ----------- (2,4) ----------> [-9]
gap> VerticalProduct( vinv, sq24 ) = vid[2];
true
gap> VerticalProduct( sq24, vinv ) = vid[1];   
true


## SubSection 7.2.1
gap> Dc6 := SinglePieceBasicDoubleGroupoid( Gc6 );; 
gap> Ds4 := SinglePieceBasicDoubleGroupoid( Gs4 );; 
gap> Dc6s4 := DoubleGroupoid( [ Dc6, Ds4 ] );
double groupoid having 2 pieces :-
1: single piece double groupoid with:
 groupoid = Gs4
    group = s4
  objects = [ -15 .. -11 ]
2: single piece double groupoid with:
 groupoid = Gc6
    group = c6
  objects = [ -6 ]

gap> Ds4c6 := DoubleGroupoid( [ Gs4, Gc6 ] );;
gap> Pieces( Ds4c6 );
[ single piece double groupoid with:
     groupoid = Gs4
        group = s4
      objects = [ -15 .. -11 ], single piece double groupoid with:
     groupoid = Gc6
        group = c6
      objects = [ -6 ] ]
gap> Dc6s4 = Ds4c6;
true

## SubSection 7.3.1
gap> Dtriv := DoubleGroupoidWithTrivialGroup( [-19..-17] );
single piece double groupoid with:
 groupoid = single piece groupoid: < Group( [ () ] ), [ -19 .. -17 ] >
    group = Group( [ () ] )
  objects = [ -19 .. -17 ]

gap> Size(Dtriv);                                          
81

## Section 7.4
gap> Gd8c6 := DirectProduct( Gd8, Gc6 );
single piece groupoid: < Group( [ (1,2,3,4), (1,3), (5,6,7)(8,9) ] ),
[ [ -9, -6 ], [ -8, -6 ], [ -7, -6 ] ] >
gap> SetName( Gd8c6, "Gd8c6" );
gap> Dd8c6 := SinglePieceBasicDoubleGroupoid( Gd8c6 );
single piece double groupoid with:
 groupoid = Gd8c6
    group = Group( [ (1,2,3,4), (1,3), (5,6,7)(8,9) ] )
  objects = [ [ -9, -6 ], [ -8, -6 ], [ -7, -6 ] ]

gap> emb1 := Embedding( Gd8c6, 1 );;
gap> emb2 := Embedding( Gd8c6, 2 );;
gap> a5 := Arrow( Gd8, (1,3), -9, -7 );;
gap> a6 := ImageElm( emb1, a5 );
[(1,3) : [ -9, -6 ] -> [ -7, -6 ]]
gap> d5 := Arrow( Gd8, (2,4), -9, -8 );;
gap> d6 := ImageElm( emb1, d5 );
[(2,4) : [ -9, -6 ] -> [ -8, -6 ]]
gap> b5 := Arrow( Gc6, (5,6,7), -6, -6 );;
gap> b6 := ImageElm( emb2, b5 );
[(5,6,7) : [ -9, -6 ] -> [ -9, -6 ]]
gap> c6 := Arrow( Gd8c6, (8,9), [-7,-6], [-8,-6] );;
gap> sq := SquareOfArrows( Dd8c6, a6, b6, c6, d6 );
[[ -9, -6 ]] ------ (1,3) -----> [[ -7, -6 ]]
  |                                        |
(5,6,7)        (1,3)(2,4)(5,7,6)(8,9)        (8,9)
  V                                        V
[[ -9, -6 ]] ------ (2,4) -----> [[ -8, -6 ]]

## SubSection 7.5.1
gap> ad8 := GroupHomomorphismByImages( d8, d8,
>               [ (1,2,3,4), (1,3) ], [ (1,4,3,2), (2,4) ] );;
gap> md8 := GroupoidHomomorphism( Gd8, Gd8, ad8, 
>               [-7,-9,-8], [(),(1,3),(2,4)] );;
gap> endDd8 := DoubleGroupoidHomomorphism( Dd8, Dd8, md8 );
double groupoid homomorphism : Dd8 -> Dd8
gap> Display( endDd8 );
double groupoid homomorphism: [ Dd8 ] -> [ Dd8 ]
with underlying groupoid homomorphism:
 groupoid mapping: [ Gd8 ] -> [ Gd8 ]
root homomorphism: [ [ (1,2,3,4), (1,3) ], [ (1,4,3,2), (2,4) ] ]
images of objects: [ -7, -9, -8 ]
   images of rays: [ [() : -7 -> -7], [(1,3) : -7 -> -9], [(2,4) : -7 -> -8] ]
gap> sq1;
[-7] ------- (1,3) ------> [-8]
  |                          |
(2,4)        (2,4)        (1,2)(3,4)
  V                          V
[-7] ----- (1,2,3,4) ----> [-9]
gap> ImageElm( endDd8, sq1 );
[-8] ------- (1,3) ------> [-9]
  |                         |
(1,3)        (1,3)        (1,4,3,2)
  V                         V
[-8] ---- (1,4)(2,3) ---> [-7]


gap> DV4 := DoubleGroupoid( V4 );
double groupoid having 4 pieces :-
1: single piece double groupoid with:
 groupoid = Gs3
    group = s3
  objects = [ -36, -35, -34 ]
2: single piece double groupoid with:
 groupoid = Gq8
    group = q8
  objects = [ -18, -17 ]
3: single piece double groupoid with:
 groupoid = Gd8
    group = d8
  objects = [ -9 .. -7 ]
4: single piece double groupoid with:
 groupoid = Gc6
    group = c6
  objects = [ -6 ]

gap> a4 := Arrow( Gq8, x, -18, -17 );;
gap> b4 := Arrow( Gq8, y, -18, -17 );;
gap> c4 := Arrow( Gq8, y^2, -17, -18 );;
gap> d4 := Arrow( Gq8, x*y, -17, -18 );;
Dq8 := DoubleGroupoid( Gq8 );;
gap> sq4 := SquareOfArrows( Dq8, a4, b4, c4, d4 );
[-18] ----- x ----> [-17]
  |                   |
y          y2          y2
  V                   V
[-17] ---- x*y ---> [-18]




gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "double.tst", 10000 );
