############################################################################
##
#W  double.tst               Groupoids Package                 Chris Wensley
##

gap> START_TEST( "groupoids package: double.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## make double.tst independent of gpd.tst and gpdhom.tst
gap> a4 := Group( (1,2,3), (2,3,4) );; 
gap> d8 := Group( (5,6,7,8), (5,7) );;
gap> SetName( a4, "a4" );  SetName( d8, "d8" ); 
gap> Ga4 := SinglePieceGroupoid( a4, [-15 .. -11] );; 
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );;
gap> c6 := Group( (11,12,13)(14,15) );;
gap> SetName( c6, "c6" );
gap> Gc6 := MagmaWithSingleObject( c6, -10 );;
gap> SetName( Ga4, "Ga4" );  SetName( Gd8, "Gd8" );  SetName( Gc6, "Gc6" );  
gap> q8 := QuaternionGroup( 8 );;
gap> genq8 := GeneratorsOfGroup( q8 );;
gap> x := genq8[1];;  y := genq8[2];;
gap> Gq8 := Groupoid( q8, [ -19, -18, -17 ] );;
gap> SetName( q8, "q8" );  SetName( Gq8, "Gq8" );
gap> U3 := UnionOfPieces( [ Ga4, Gc6, Gd8 ] );;

## SubSection 8.1.1 
gap> DGd8 := SinglePieceBasicDoubleGroupoid( Gd8 );; 
gap> DGd8!.groupoid;
Gd8
gap> DGd8!.objects;
[ -9, -8, -7 ]
gap> SetName( DGd8, "DGd8" );
gap> [ IsDoubleGroupoid( DGd8 ), IsBasicDoubleGroupoid( DGd8 ) ];
[ true, true ]

## SubSection 8.1.2 
gap> [ Size(DGd8), (3*8)^4 ]; 
[ 331776, 331776 ]
gap> a1 := Arrow( Gd8, (5,7), -7, -8 );;
gap> b1 := Arrow( Gd8, (6,8), -7, -7 );;
gap> c1 := Arrow( Gd8, (5,6)(7,8), -8, -9 );;
gap> d1 := Arrow( Gd8, (5,6,7,8), -7, -9 );;
gap> bdy1 := d1^-1 * b1^-1 * a1 * c1;
[(6,8) : -9 -> -9]
gap> sq1 := SquareOfArrows( DGd8, a1, b1, c1, d1 ); 
[-7] ------- (5,7) ------> [-8]
  |                          |
(6,8)        (6,8)        (5,6)(7,8)
  V                          V
[-7] ----- (5,6,7,8) ----> [-9]
gap> sq1 in DGd8;
true
gap> UpArrow( sq1 );
[(5,7) : -7 -> -8]
gap> LeftArrow( sq1 );
[(6,8) : -7 -> -7]
gap> RightArrow( sq1 );
[(5,6)(7,8) : -8 -> -9]
gap> DownArrow( sq1 );
[(5,6,7,8) : -7 -> -9]
gap> BoundaryOfSquare( sq1 );
[(6,8) : -9 -> -9]
gap> DoubleGroupoidOfSquare( sq1 );
DGd8
gap> IsDoubleGroupoidElement( sq1 );
true

## SubSection 8.1.3
gap> a2 := Arrow( Gd8, (6,8), -8, -9 );;
gap> c2 := Arrow( Gd8, (5,7)(6,8), -9, -8);;
gap> d2 := Arrow( Gd8, (5,6,7,8), -9, -8 );; 
gap> sq2 := SquareOfArrows( DGd8, a2, c1, c2, d2 );
[-8] -------- (6,8) -------> [-9]
  |                            |
(5,6)(7,8)        ()        (5,7)(6,8)
  V                            V
[-9] ------ (5,6,7,8) -----> [-8]
gap> bdy2 := BoundaryOfSquare( sq2 );
[() : -8 -> -8]
gap> [ IsCommutingSquare(sq1), IsCommutingSquare(sq2) ]; 
[ false, true ]

## SubSection 8.1.4
gap> tsq1 := TransposedSquare( sq1 );
[-7] ------- (6,8) ------> [-7]
  |                         |
(5,7)        (6,8)        (5,6,7,8)
  V                         V
[-8] ---- (5,6)(7,8) ---> [-9]
gap> IsClosedUnderTransposition( sq1 );  
false

## SubSection 8.1.5
gap> LeftArrow( sq2 ) = RightArrow( sq1 ); 
true
gap> sq12 := HorizontalProduct( sq1, sq2 );
[-7] ----- (5,7)(6,8) ----> [-9]
  |                          |
(6,8)        (5,7)        (5,7)(6,8)
  V                          V
[-7] ----- (5,7)(6,8) ----> [-8]
gap> bdy12 := BoundaryOfSquare( sq12 );
[(5,7) : -8 -> -8]
gap> (bdy1^d2) * bdy2 = bdy12;
true

## SubSection 8.1.6
gap> b3 := Arrow( Gd8, (5,7), -7, -9 );;
gap> c3 := Arrow( Gd8, (6,8), -9, -8);;
gap> d3 := Arrow( Gd8, (5,8)(6,7), -9, -8 );; 
gap> sq3 := SquareOfArrows( DGd8, d1, b3, c3, d3 );
[-7] ---- (5,6,7,8) ---> [-9]
  |                        |
(5,7)       (6,8)       (6,8)
  V                        V
[-9] ---- (5,8)(6,7) ---> [-8]
gap> bdy3 := BoundaryOfSquare( sq3 );
[(6,8) : -8 -> -8]
gap> UpArrow( sq3 ) = DownArrow( sq1 ); 
true
gap> sq13 := VerticalProduct( sq1, sq3 );
[-7] -------- (5,7) -------> [-8]
  |                           |
(5,7)(6,8)        ()        (5,8,7,6)
  V                           V
[-9] ----- (5,8)(6,7) ----> [-8]

gap> c4 := Arrow( Gd8, (5,6,7,8), -8, -7);;
gap> d4 := Arrow( Gd8, (5,6)(7,8), -8, -7 );; 
gap> sq4 := SquareOfArrows( DGd8, d2, c3, c4, d4 );
[-9] ------- (5,6,7,8) ------> [-8]
  |                             |
(6,8)        (5,6,7,8)        (5,6,7,8)
  V                             V
[-8] ------ (5,6)(7,8) -----> [-7]
gap> UpArrow( sq4 ) = DownArrow( sq2 );
true
gap> LeftArrow( sq4 ) = RightArrow( sq3 ); 
true

gap> sq34 := HorizontalProduct( sq3, sq4 );
[-7] ------- (5,7)(6,8) ------> [-8]
  |                              |
(5,7)        (5,8)(6,7)        (5,6,7,8)
  V                              V
[-9] ------- (5,7)(6,8) ------> [-7]

gap> sq1234 := VerticalProduct( sq12, sq34 );
[-7] --------- (5,7)(6,8) --------> [-9]
  |                                  |
(5,7)(6,8)        (5,6,7,8)        (5,8,7,6)
  V                                  V
[-9] --------- (5,7)(6,8) --------> [-7]

gap> sq24 := VerticalProduct( sq2, sq4 ); 
[-8] ----------- (6,8) ----------> [-9]
  |                                 |
(5,8,7,6)        (5,6,7,8)        (5,8,7,6)
  V                                 V
[-8] -------- (5,6)(7,8) -------> [-7]

gap> sq1324 := HorizontalProduct( sq13, sq24 );;
gap> sq1324 = sq1234;
true

## SubSection 8.1.7
gap> hid := HorizontalIdentities( sq24 );;
gap> hid[1]; Print("\n"); hid[2];                    
[-8] --------- () --------> [-8]
  |                          |
(5,8,7,6)        ()        (5,8,7,6)
  V                          V
[-8] --------- () --------> [-8]

[-9] --------- () --------> [-9]
  |                          |
(5,8,7,6)        ()        (5,8,7,6)
  V                          V
[-7] --------- () --------> [-7]
gap> HorizontalProduct( hid[1], sq24 ) = sq24;
true
gap> HorizontalProduct( sq24, hid[2] ) = sq24;      
true

gap> vid := VerticalIdentities( sq24 );;  
gap> vid[1]; Print("\n"); vid[2];                    
[-8] ---- (6,8) ---> [-9]
  |                   |
()         ()         ()
  V                   V
[-8] ---- (6,8) ---> [-9]

[-8] ---- (5,6)(7,8) ---> [-7]
  |                        |
()            ()            ()
  V                        V
[-8] ---- (5,6)(7,8) ---> [-7]
gap> VerticalProduct( vid[1], sq24 ) = sq24;
true
gap> VerticalProduct( sq24, vid[2] ) = sq24;
true

gap> hinv := HorizontalInverse( sq24 ); 
[-9] ----------- (6,8) ----------> [-8]
  |                                 |
(5,8,7,6)        (5,6,7,8)        (5,8,7,6)
  V                                 V
[-7] -------- (5,6)(7,8) -------> [-8]
gap> HorizontalProduct( hinv, sq24 ) = hid[2];
true
gap> HorizontalProduct( sq24, hinv ) = hid[1];      
true

gap> vinv := VerticalInverse( sq24 );
[-8] -------- (5,6)(7,8) -------> [-7]
  |                                 |
(5,6,7,8)        (5,8,7,6)        (5,6,7,8)
  V                                 V
[-8] ----------- (6,8) ----------> [-9]
gap> VerticalProduct( vinv, sq24 ) = vid[2];
true
gap> VerticalProduct( sq24, vinv ) = vid[1];   
true

## Section 8.2
## SubSection 8.2.1
gap> DGc6 := SinglePieceBasicDoubleGroupoid( Gc6 );; 
gap> DGa4 := SinglePieceBasicDoubleGroupoid( Ga4 );; 
gap> DGc6s4 := DoubleGroupoid( [ DGc6, DGa4 ] );
double groupoid having 2 pieces :-
1: single piece double groupoid with:
 groupoid = Ga4
    group = a4
  objects = [ -15 .. -11 ]
2: single piece double groupoid with:
 groupoid = Gc6
    group = c6
  objects = [ -10 ]

gap> DGa4c6 := DoubleGroupoid( [ Ga4, Gc6 ] );;
gap> Pieces( DGa4c6 );
[ single piece double groupoid with:
     groupoid = Ga4
        group = a4
      objects = [ -15 .. -11 ], single piece double groupoid with:
     groupoid = Gc6
        group = c6
      objects = [ -10 ] ]

## Section 8.3
## SubSection 8.3.1
gap> DGtriv := DoubleGroupoidWithTrivialGroup( [-19..-17] );
single piece double groupoid with:
 groupoid = single piece groupoid: < Group( [ () ] ), [ -19 .. -17 ] >
    group = Group( [ () ] )
  objects = [ -19 .. -17 ]

gap> Size(DGtriv);                                          
81

## SubSection 8.3.2
gap> DGc4 := DoubleGroupoidWithSingleObject( Group((1,2,3,4)), 0 );
single piece double groupoid with:
 groupoid = single piece groupoid: < Group( [ (1,2,3,4) ] ), [ 0 ] >
    group = Group( [ (1,2,3,4) ] )
  objects = [ 0 ]

gap> Size( DGc4 );                                                 
256

## Section 8.4
gap> Gd8c6 := DirectProduct( Gd8, Gc6 );
single piece groupoid: < Group( [ (1,2,3,4), (1,3), (5,6,7)(8,9) ] ), 
[ [ -9, -10 ], [ -8, -10 ], [ -7, -10 ] ] >
gap> SetName( Gd8c6, "Gd8c6" );
gap> DGd8c6 := SinglePieceBasicDoubleGroupoid( Gd8c6 );
single piece double groupoid with:
 groupoid = Gd8c6
    group = Group( [ (1,2,3,4), (1,3), (5,6,7)(8,9) ] )
  objects = [ [ -9, -10 ], [ -8, -10 ], [ -7, -10 ] ]

gap> emb1 := Embedding( Gd8c6, 1 );;
gap> emb2 := Embedding( Gd8c6, 2 );;
gap> a5 := Arrow( Gd8, (5,7), -9, -7 );;
gap> a6 := ImageElm( emb1, a5 );
[(1,3) : [ -9, -10 ] -> [ -7, -10 ]]
gap> d5 := Arrow( Gd8, (6,8), -9, -8 );;
gap> d6 := ImageElm( emb1, d5 );
[(2,4) : [ -9, -10 ] -> [ -8, -10 ]]
gap> b5 := Arrow( Gc6, (11,12,13), -10, -10 );;
gap> b6 := ImageElm( emb2, b5 );
[(5,6,7) : [ -9, -10 ] -> [ -9, -10 ]]
gap> c6 := Arrow( Gd8c6, (8,9), [-7,-10], [-8,-10] );;
gap> sq := SquareOfArrows( DGd8c6, a6, b6, c6, d6 );
[[ -9, -10 ]] ----- (1,3) ----> [[ -7, -10 ]]
  |                                        |
(5,6,7)        (1,3)(2,4)(5,7,6)(8,9)        (8,9)
  V                                        V
[[ -9, -10 ]] ----- (2,4) ----> [[ -8, -10 ]]

## SubSection 8.5.1
gap> ad8 := GroupHomomorphismByImages( d8, d8,
>               [ (5,6,7,8), (5,7) ], [ (5,8,7,6), (6,8) ] );;
gap> md8 := GroupoidHomomorphism( Gd8, Gd8, ad8, 
>               [-7,-9,-8], [(),(5,7),(6,8)] );;
gap> endDGd8 := DoubleGroupoidHomomorphism( DGd8, DGd8, md8 );;
gap> Display( endDGd8 );            
double groupoid homomorphism: [ DGd8 ] -> [ DGd8 ]
with underlying groupoid homomorphism:
homomorphism to single piece groupoid: Gd8 -> Gd8
root group homomorphism:
(5,6,7,8) -> (5,8,7,6)
(5,7) -> (6,8)
object map: [ -9, -8, -7 ] -> [ -7, -9, -8 ]
ray images: [ (), (5,7), (6,8) ]
gap> IsDoubleGroupoidHomomorphism( endDGd8 );
true
gap> sq1;
[-7] ------- (5,7) ------> [-8]
  |                          |
(6,8)        (6,8)        (5,6)(7,8)
  V                          V
[-7] ----- (5,6,7,8) ----> [-9]
gap> ImageElm( endDGd8, sq1 );
[-8] ------- (5,7) ------> [-9]
  |                         |
(5,7)        (5,7)        (5,8,7,6)
  V                         V
[-8] ---- (5,8)(6,7) ---> [-7]


gap> DGU3 := DoubleGroupoid( U3 );
double groupoid having 3 pieces :-
1: single piece double groupoid with:
 groupoid = Ga4
    group = a4
  objects = [ -15 .. -11 ]
2: single piece double groupoid with:
 groupoid = Gc6
    group = c6
  objects = [ -10 ]
3: single piece double groupoid with:
 groupoid = Gd8
    group = d8
  objects = [ -9, -8, -7 ]


gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "double.tst", 10000 );
