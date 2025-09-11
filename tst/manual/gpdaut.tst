############################################################################
##
#W  gpdaut.tst              groupoids Package                  Chris Wensley
##

gap> START_TEST( "groupoids package: gpdaut.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## make gpdaut.tst independent of other tests  
gap> a4 := Group( (1,2,3), (2,3,4) );; 
gap> d8 := Group( (5,6,7,8), (5,7) );;
gap> SetName( a4, "a4" );  SetName( d8, "d8" ); 
gap> Ga4 := SinglePieceGroupoid( a4, [-15 .. -11] );; 
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );;
gap> Ha4 := SubgroupoidByObjects( Ga4, [-14,-13,-12] );;  
gap> SetName( Ha4, "Ha4" );
gap> k4 := Subgroup( a4, [ (1,2)(3,4), (1,3)(2,4) ] );;
gap> SetName( k4, "k4" );
gap> Kk4 := SubgroupoidWithRays( Ha4, k4, [ (), (1,3,4), (1,4)(2,3) ] );;
gap> SetName( Kk4, "Kk4" );


## Section 6.1, Automorphisms of single piece groupoids 

## SubSection 6.1.1
gap> perm1 := [-13,-12,-14];;
gap> aut1 := GroupoidAutomorphismByObjectPerm( Ha4, perm1 );; 
gap> Display( aut1 ); 
homomorphism to single piece groupoid: Ha4 -> Ha4
root group homomorphism:
(1,2,3) -> (1,2,3)
(2,3,4) -> (2,3,4)
object map: [ -14, -13, -12 ] -> [ -13, -12, -14 ]
ray images: [ (), (), () ]
gap> d := Arrow( Ha4, (1,3,4), -12, -13 ); 
[(1,3,4) : -12 -> -13]
gap> d1 := ImageElm( aut1, d ); 
[(1,3,4) : -14 -> -12]
gap> gensa4 := GeneratorsOfGroup( a4 );; 
gap> alpha2 := GroupHomomorphismByImages( a4, a4, gensa4, [(2,3,4), (1,3,4)] );; 
gap> aut2 := GroupoidAutomorphismByGroupAuto( Ha4, alpha2 );; 
gap> Display( aut2 ); 
homomorphism to single piece groupoid: Ha4 -> Ha4
root group homomorphism:
(1,2,3) -> (2,3,4)
(2,3,4) -> (1,3,4)
object map: [ -14, -13, -12 ] -> [ -14, -13, -12 ]
ray images: [ (), (), () ]
gap> d2 := ImageElm( aut2, d1 );
[(1,2,4) : -14 -> -12]
gap> L3 := [(1,2)(3,4), (1,3)(2,4), (1,4)(2,3)];; 
gap> aut3 := GroupoidAutomorphismByNtuple( Ha4, L3 );; 
gap> Display( aut3 ); 
homomorphism to single piece groupoid: Ha4 -> Ha4
root group homomorphism:
(1,2,3) -> (1,4,2)
(2,3,4) -> (1,4,3)
object map: [ -14, -13, -12 ] -> [ -14, -13, -12 ]
ray images: [ (), (1,4)(2,3), (1,3)(2,4) ]
gap> d3 := ImageElm( aut3, d2 );
[(2,3,4) : -14 -> -12]
gap> L4 := [(), (1,3,2), (2,4,3)];; 
gap> aut4 := GroupoidAutomorphismByRayShifts( Ha4, L4 );; 
gap> Display( aut4 ); 
homomorphism to single piece groupoid: Ha4 -> Ha4
root group homomorphism:
(1,2,3) -> (1,2,3)
(2,3,4) -> (2,3,4)
object map: [ -14, -13, -12 ] -> [ -14, -13, -12 ]
ray images: [ (), (1,3,2), (2,4,3) ]
gap> d4 := ImageElm( aut4, d3 );
[() : -14 -> -12]
gap> h4 := Arrow( Ha4, (2,3,4), -12, -13 );; 
gap> aut1234 := aut1*aut2*aut3*aut4;; 
gap> Display( aut1234 ); 
homomorphism to single piece groupoid: Ha4 -> Ha4
root group homomorphism:
(1,2,3) -> (1,4,3)
(2,3,4) -> (1,2,3)
object map: [ -14, -13, -12 ] -> [ -13, -12, -14 ]
ray images: [ (), (2,3,4), (1,3,4) ]
gap> d4  = ImageElm( aut1234, d );
true
gap> inv1234 := InverseGeneralMapping( aut1234 );; 
gap> Display( inv1234 ); 
homomorphism to single piece groupoid: Ha4 -> Ha4
root group homomorphism:
(1,2,3) -> (1,4,3)
(2,3,4) -> (2,4,3)
object map: [ -14, -13, -12 ] -> [ -12, -14, -13 ]
ray images: [ (), (1,3,2), (1,3,4) ]

# Subsection 6.1.2 
gap> inn1 := GroupoidInnerAutomorphism( Ha4, h4 );;
gap> Display( inn1 );
homomorphism to single piece groupoid: Ha4 -> Ha4
root group homomorphism:
(1,2,3) -> (1,2,3)
(2,3,4) -> (2,3,4)
object map: [ -14, -13, -12 ] -> [ -14, -12, -13 ]
ray images: [ (), (2,4,3), (2,3,4) ]
gap> d5 := ImageElm( inn1, d4 );
[(2,3,4) : -14 -> -13]

gap> Nk4 := SubgroupoidBySubgroup( Ha4, k4 );;
gap> SetName( Nk4, "Nk4" );
gap> e4 := Arrow( Ha4, (1,2)(3,4), -14, -13 );;
gap> inn2 := GroupoidInnerAutomorphismNormalSubgroupoid( Ha4, Nk4, e4 );;
gap> Display( inn2 );
homomorphism to single piece groupoid: Nk4 -> Nk4
root group homomorphism:
(1,2)(3,4) -> (1,2)(3,4)
(1,3)(2,4) -> (1,3)(2,4)
object map: [ -14, -13, -12 ] -> [ -13, -14, -12 ]
ray images: [ (), (), (1,2)(3,4) ]

gap> Ma4 := MaximalDiscreteSubgroupoid( Ha4 );;
gap> SetName( Ma4, "Ma4" );
gap> inn3 := GroupoidInnerAutomorphismNormalSubgroupoid( Ha4, Ma4, e4 );;
gap> Display( inn3 );
homogeneous discrete groupoid mapping: [ Ma4 ] -> [ Ma4 ]
images of objects: [ -13, -14, -12 ]
object homomorphisms:
GroupHomomorphismByImages( a4, a4, [ (1,2,3), (2,3,4) ], [ (1,4,2), (1,4,3)  ] )
GroupHomomorphismByImages( a4, a4, [ (1,2,3), (2,3,4) ], [ (1,4,2), (1,4,3)  ] )
GroupHomomorphismByImages( a4, a4, [ (1,2,3), (2,3,4) ], [ (1,2,3), (2,3,4)  ] )

# Subsection 6,1,3
gap> ## (1) automorphism by group auto 
gap> a6 := GroupHomomorphismByImages( k4, k4,
>              [ (1,2)(3,4), (1,3)(2,4) ], [ (1,3)(2,4), (1,4)(2,3) ] );;
gap> aut6 := GroupoidAutomorphismByGroupAuto( Kk4, a6 );
groupoid homomorphism : Kk4 -> Kk4
[ [ [(1,2)(3,4) : -14 -> -14], [(1,3)(2,4) : -14 -> -14], 
      [(1,3,4) : -14 -> -13], [(1,4)(2,3) : -14 -> -12] ], 
  [ [(1,3)(2,4) : -14 -> -14], [(1,4)(2,3) : -14 -> -14], 
      [(1,3,4) : -14 -> -13], [(1,4)(2,3) : -14 -> -12] ] ]
gap> a := Arrow( Kk4, (1,3)(2,4), -12, -12 );;
gap> ImageElm( aut6, a );
[(1,4)(2,3) : -12 -> -12]
gap> b := Arrow( Kk4, (1,4,2), -12, -13 );;
gap> ImageElm( aut6, b );
[(1,2,3) : -12 -> -13]
gap> ## (2) automorphism by object perm 
gap> aut7 := GroupoidAutomorphismByObjectPerm( Kk4, [-13,-12,-14] );
groupoid homomorphism : Kk4 -> Kk4
[ [ [(1,2)(3,4) : -14 -> -14], [(1,3)(2,4) : -14 -> -14], 
      [(1,3,4) : -14 -> -13], [(1,4)(2,3) : -14 -> -12] ], 
  [ [(1,4)(2,3) : -13 -> -13], [(1,2)(3,4) : -13 -> -13], 
      [(2,3,4) : -13 -> -12], [(1,4,3) : -13 -> -14] ] ]
gap> ImageElm( aut7, a );                                           
[(1,3)(2,4) : -14 -> -14]
gap> ImageElm( aut7, b );                                           
[(1,3)(2,4) : -14 -> -12]
gap> ## (3) automorphism by ray shifts 
gap> aut8 := GroupoidAutomorphismByRayShifts( Kk4,
>                [ (), (1,4)(2,3), (1,3)(2,4) ] );
groupoid homomorphism : Kk4 -> Kk4
[ [ [(1,2)(3,4) : -14 -> -14], [(1,3)(2,4) : -14 -> -14], 
      [(1,3,4) : -14 -> -13], [(1,4)(2,3) : -14 -> -12] ], 
  [ [(1,2)(3,4) : -14 -> -14], [(1,3)(2,4) : -14 -> -14], 
      [(1,2,3) : -14 -> -13], [(1,2)(3,4) : -14 -> -12] ] ]
gap> ImageElm( aut8, a );
[(1,3)(2,4) : -12 -> -12]
gap> ImageElm( aut8, b );
[(1,2,3) : -12 -> -13]
gap> ## (4) combine these three automorphisms 
gap> aut678 := aut6 * aut7 * aut8;
groupoid homomorphism : Kk4 -> Kk4
[ [ [(1,2)(3,4) : -14 -> -14], [(1,3)(2,4) : -14 -> -14], 
      [(1,3,4) : -14 -> -13], [(1,4)(2,3) : -14 -> -12] ], 
  [ [(1,2)(3,4) : -13 -> -13], [(1,3)(2,4) : -13 -> -13], 
      [(1,4,3) : -13 -> -12], [(1,3,2) : -13 -> -14] ] ]
gap> ImageElm( aut678, a ); 
[(1,4)(2,3) : -14 -> -14]
gap> ImageElm( aut678, b );
[(1,4)(2,3) : -14 -> -12]
gap> ## (5) conjgation by an arrow
gap> e8 := Arrow( Kk4, (1,3)(2,4), -14, -12 );;
gap> aut9 := GroupoidInnerAutomorphism( Kk4, e8 );
groupoid homomorphism : Kk4 -> Kk4
[ [ [(1,2)(3,4) : -14 -> -14], [(1,3)(2,4) : -14 -> -14], 
      [(1,3,4) : -14 -> -13], [(1,4)(2,3) : -14 -> -12] ], 
  [ [(1,2)(3,4) : -12 -> -12], [(1,3)(2,4) : -12 -> -12], 
      [(1,4,2) : -12 -> -13], [(1,4)(2,3) : -12 -> -14] ] ]

# Subsection 6.1.4
gap> AHa4 := AutomorphismGroupOfGroupoid( Ha4 ); 
Aut(Ha4)
gap> Agens := GeneratorsOfGroup( AHa4);; 
gap> Length( Agens );
8
gap> NHa4 := NiceObject( AHa4 );; 
gap> MHa4 := NiceMonomorphism( AHa4 );; 
gap> Size( AHa4 );    ## (3!)x24x(12^2)
20736
gap> SetName( AHa4, "AHa4" ); 
gap> SetName( NHa4, "NHa4" );
gap> ## either of these names may be returned
gap> names := [ "(((A4 x A4 x A4) : C2) : C3) : C2",
>    "(C2 x C2 x C2 x C2 x C2 x C2) : (((C3 x C3 x C3) : C3) : (C2 x C2))" ];;
gap> StructureDescription( NHa4 ) in names;
true
gap> ##  cannot test images of Agens because of random variations 
gap> ##  Now do some tests!
gap> mgi := MappingGeneratorsImages( MHa4 );; 
gap> autgen := mgi[1];;
gap> pcgen := mgi[2];;
gap> ngen := Length( autgen );; 
gap> ForAll( [1..ngen], i -> Order(autgen[i]) = Order(pcgen[i]) ); 
true

## SubSection 6.1.5
gap> AHa40 := Groupoid( AHa4, [0] );
single piece groupoid: < Aut(Ha4), [ 0 ] >
gap> conj := function(a) 
>        return ArrowNC( Ha4, true, GroupoidInnerAutomorphism(Ha4,a), 0, 0 ); 
>    end;; 
gap> inner := MappingWithObjectsByFunction( Ha4, AHa40, conj, [0,0,0] );;
gap> a1 := Arrow( Ha4, (1,2,3), -14, -13 );;
gap> inner1 := ImageElm( inner, a1 );;                        
gap> a2 := Arrow( Ha4, (2,3,4), -13, -12 );;
gap> inner2 := ImageElm( inner, a2 );;       
gap> a3 := a1*a2;                      
[(1,3)(2,4) : -14 -> -12]
gap> inner3 := ImageElm( inner, a3 );  
[groupoid homomorphism : Ha4 -> Ha4
[ [ [(1,2,3) : -14 -> -14], [(2,3,4) : -14 -> -14], [() : -14 -> -13], 
      [() : -14 -> -12] ], 
  [ [(1,3,4) : -12 -> -12], [(1,2,4) : -12 -> -12], [(1,3)(2,4) : -12 -> -13],
      [() : -12 -> -14] ] ] : 0 -> 0]
gap> (inner3 = inner1*inner2*inner1) and (inner3 = inner2*inner1*inner2);
true

## SubSection 6.1.6
gap> Dd8 := HomogeneousDiscreteGroupoid( d8, [ -13..-10] ); 
homogeneous, discrete groupoid: < d8, [ -13 .. -10 ] >
gap> aut10 := GroupoidAutomorphismByObjectPerm( Dd8, [-12,-10,-11,-13] ); 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -12, -10, -11, -13 ]
object homomorphisms:
IdentityMapping( d8 )
IdentityMapping( d8 )
IdentityMapping( d8 )
IdentityMapping( d8 )
gap> gend8 := GeneratorsOfGroup( d8 );; 
gap> g1 := gend8[1];; 
gap> g2 := gend8[2];;
gap> b1 := IdentityMapping( d8 );;
gap> b2 := GroupHomomorphismByImages( d8, d8, gend8, [g1, g2*g1 ] );; 
gap> b3 := GroupHomomorphismByImages( d8, d8, gend8, [g1^g2, g2 ] );; 
gap> b4 := GroupHomomorphismByImages( d8, d8, gend8, [g1^g2, g2^(g1*g2) ] );; 
gap> aut11 := GroupoidAutomorphismByGroupAutos( Dd8, [b1,b2,b3,b4] ); 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -13, -12, -11, -10 ]
object homomorphisms:
IdentityMapping( d8 )
GroupHomomorphismByImages( d8, d8, [ (5,6,7,8), (5,7) ], 
[ (5,6,7,8), (5,8)(6,7) ] )
GroupHomomorphismByImages( d8, d8, [ (5,6,7,8), (5,7) ], [ (5,8,7,6), (5,7) ] )
GroupHomomorphismByImages( d8, d8, [ (5,6,7,8), (5,7) ], [ (5,8,7,6), (6,8) ] )
gap> ADd8 := AutomorphismGroupOfGroupoid( Dd8 ); 
<group with 4 generators>
gap> Size( ADd8 );    ## 4!*8^4
98304
gap> genADd8 := GeneratorsOfGroup( ADd8 );;
gap> Length( genADd8 ); 
4  
gap> w := GroupoidAutomorphismByGroupAutos( Dd8, [b2,b1,b1,b1] );; 
gap> x := GroupoidAutomorphismByGroupAutos( Dd8, [b3,b1,b1,b1] );; 
gap> y := GroupoidAutomorphismByObjectPerm( Dd8, [ -12, -11, -10, -13 ] );; 
gap> z := GroupoidAutomorphismByObjectPerm( Dd8, [ -12, -13, -11, -10 ] );; 
gap> ok := ForAll( genADd8, a -> a in[ w, x, y, z ] ); 
true
gap> NADd8 := NiceObject( ADd8 );; 
gap> MADd8 := NiceMonomorphism( ADd8 );;
gap> w1 := ImageElm( MADd8, w );; 
gap> x1 := ImageElm( MADd8, x );; 
gap> y1 := ImageElm( MADd8, y );; 
gap> z1 := ImageElm( MADd8, z );; 
gap> u := z*w*y*x*z; 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -11, -13, -10, -12 ]
object homomorphisms:
IdentityMapping( d8 )
GroupHomomorphismByImages( d8, d8, [ (5,6,7,8), (5,7) ], 
[ (5,6,7,8), (5,8)(6,7) ] )
IdentityMapping( d8 )
GroupHomomorphismByImages( d8, d8, [ (5,6,7,8), (5,7) ], [ (5,8,7,6), (5,7) ] )
gap> u1 := z1*w1*y1*x1*z1; 
(1,2,4,3)(5,17,23,11,6,18,24,16)(7,19,25,15,9,21,27,13)(8,20,26,14,10,22,28,12)
gap> imu := ImageElm( MADd8, u );; 
gap> u1 = imu;
true

## SubSection 6.1.7
gap> HGd8 := HomogeneousGroupoid( Gd8, 
>                [ [-39,-38,-37], [-36,-35,-34], [-33,-32,-31] ] );;
gap> SetName( HGd8, "HGd8" );
gap> AHGd8 := AutomorphismGroupoidOfGroupoid( HGd8 ); 
Aut(HGd8)
gap> ObjectList( AHGd8 );
[ [ -39, -38, -37 ], [ -36, -35, -34 ], [ -33, -32, -31 ] ]
gap> RaysOfGroupoid( AHGd8 ){[2..3]};
[ groupoid homomorphism : 
    [ [ [(5,6,7,8) : -39 -> -39], [(5,7) : -39 -> -39], [() : -39 -> -38], 
          [() : -39 -> -37] ], 
      [ [(5,6,7,8) : -36 -> -36], [(5,7) : -36 -> -36], [() : -36 -> -35], 
          [() : -36 -> -34] ] ], groupoid homomorphism : 
    [ [ [(5,6,7,8) : -39 -> -39], [(5,7) : -39 -> -39], [() : -39 -> -38], 
          [() : -39 -> -37] ], 
      [ [(5,6,7,8) : -33 -> -33], [(5,7) : -33 -> -33], [() : -33 -> -32], 
          [() : -33 -> -31] ] ] ]
gap> obgp := ObjectGroup( AHGd8, [ -36, -35, -34 ] );; 
gap> Size( obgp );    ## 3!*8^3
3072

## Section 6.2
gap> reps := IrreducibleRepresentations( a4 );; 
gap> rep4 := reps[4]; 
Pcgs([ (2,4,3), (1,3)(2,4), (1,2)(3,4) ]) -> 
[ [ [ 0, 0, 1 ], [ 1, 0, 0 ], [ 0, 1, 0 ] ], 
  [ [ -1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, -1 ] ], 
  [ [ 1, 0, 0 ], [ 0, -1, 0 ], [ 0, 0, -1 ] ] ]
gap> Ra4 := Groupoid( Image( rep4 ), Ga4!.objects );; 
gap> ObjectList( Ra4 ) = [ -15 .. -11 ];
true
gap> gens := GeneratorsOfGroupoid( Ga4 );
[ [(1,2,3) : -15 -> -15], [(2,3,4) : -15 -> -15], [() : -15 -> -14],
  [() : -15 -> -13], [() : -15 -> -12], [() : -15 -> -11] ]
gap> images := List( gens, 
>        g -> Arrow( Ra4, ImageElm(rep4,g![2]), g![3], g![4] ) ); 
[ [[ [ 0, 0, -1 ], [ 1, 0, 0 ], [ 0, -1, 0 ] ] : -15 -> -15], 
  [[ [ 0, 1, 0 ], [ 0, 0, 1 ], [ 1, 0, 0 ] ] : -15 -> -15], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -14], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -13], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -12], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -11] ]
gap> mor := GroupoidHomomorphismFromSinglePiece( Ga4, Ra4, gens, images );
groupoid homomorphism : 
[ [ [(1,2,3) : -15 -> -15], [(2,3,4) : -15 -> -15], [() : -15 -> -14], 
      [() : -15 -> -13], [() : -15 -> -12], [() : -15 -> -11] ], 
  [ [[ [ 0, 0, -1 ], [ 1, 0, 0 ], [ 0, -1, 0 ] ] : -15 -> -15], 
      [[ [ 0, 1, 0 ], [ 0, 0, 1 ], [ 1, 0, 0 ] ] : -15 -> -15], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -14], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -13], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -12], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -11] ] ]
gap> IsMatrixGroupoid( Ra4 ); 
true
gap> a := Arrow( Ha4, (1,4,2), -12, -13 );
[(1,4,2) : -12 -> -13]
gap> ImageElm( mor, a );
[[ [ 0, 0, 1 ], [ -1, 0, 0 ], [ 0, -1, 0 ] ] : -12 -> -13]
gap> rmor := RestrictedMappingGroupoids( mor, Ha4 );
groupoid homomorphism : 
[ [ [(1,2,3) : -14 -> -14], [(2,3,4) : -14 -> -14], [() : -14 -> -13], 
      [() : -14 -> -12] ], 
  [ [[ [ 0, 0, -1 ], [ 1, 0, 0 ], [ 0, -1, 0 ] ] : -14 -> -14], 
      [[ [ 0, 1, 0 ], [ 0, 0, 1 ], [ 1, 0, 0 ] ] : -14 -> -14], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -14 -> -13], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -14 -> -12] ] ]
gap> ParentMappingGroupoids( rmor ) = mor;
true

## Section 6.3 : Groupoid actions

gap> c1 := Arrow( Ha4, (1,2)(3,4), -14, -13);;
gap> innc1 := GroupoidInnerAutomorphism( Ha4, c1 );
groupoid homomorphism : Ha4 -> Ha4
[ [ [(1,2,3) : -14 -> -14], [(2,3,4) : -14 -> -14], [() : -14 -> -13], 
      [() : -14 -> -12] ], 
  [ [(1,4,2) : -13 -> -13], [(1,4,3) : -13 -> -13], [() : -13 -> -14], 
      [(1,2)(3,4) : -13 -> -12] ] ]
gap> c2 := Arrow( Ha4, (1,4,2), -13, -12);;
gap> innc2 := GroupoidInnerAutomorphism( Ha4, c2 );
groupoid homomorphism : Ha4 -> Ha4
[ [ [(1,2,3) : -14 -> -14], [(2,3,4) : -14 -> -14], [() : -14 -> -13], 
      [() : -14 -> -12] ], 
  [ [(1,2,3) : -14 -> -14], [(2,3,4) : -14 -> -14], [(1,4,2) : -14 -> -12], 
      [(1,2,4) : -14 -> -13] ] ]
gap> c12 := c1 * c2;
[(2,4,3) : -14 -> -12]
gap> innc12 := GroupoidInnerAutomorphism( Ha4, c12 );
groupoid homomorphism : Ha4 -> Ha4
[ [ [(1,2,3) : -14 -> -14], [(2,3,4) : -14 -> -14], [() : -14 -> -13], 
      [() : -14 -> -12] ], 
  [ [(1,4,2) : -12 -> -12], [(2,3,4) : -12 -> -12], [(2,3,4) : -12 -> -13], 
      [(2,4,3) : -12 -> -14] ] ]
gap> [ innc1 * innc2 * innc1 = innc12, innc2 * innc1 * innc2 = innc12 ];
[ true, true ]

## Subsection 6.3.1
gap> act1 := GroupoidActionByConjugation( Ha4 );
<general mapping: Ha4 -> Aut(Ha4) >
gap> IsGroupoidAction( act1 );
true
gap> amap1 := ActionMap( act1 );;
gap> amap1( h4 ) = inn1;
true

gap> act2 := GroupoidActionByConjugation( Ha4, Nk4 );
<general mapping: Ha4 -> Aut(Nk4) >
gap> IsGroupoidAction( act2 );
true
gap> amap2 := ActionMap( act2 );;
gap> amap2( e4 ) = inn2;
true

gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "gpdaut.tst", 10000 );
