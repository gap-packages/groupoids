##############################################################################
##
#W  gpdaut.tst              groupoids Package                    Chris Wensley
##
#Y  Copyright (C) 2000-2018, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
gap> START_TEST( "groupoids package: gpdaut.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## make gpdaut.tst independent of other tests  
gap> s4 := Group( (1,2,3,4), (3,4) );; 
gap> d8 := Subgroup( s4, [ (1,2,3,4), (1,3) ] );;
gap> SetName( s4, "s4" );  SetName( d8, "d8" ); 
gap> Gs4 := SinglePieceGroupoid( s4, [-15 .. -11] );; 
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );;
gap> Hs4 := SubgroupoidByObjects( Gs4, [-14,-13,-12] );; 
gap> SetName( Hs4, "Hs4" ); 
gap> Hd8b := SubgroupoidWithRays( Hs4, d8, [(),(1,2,3),(1,2,4)] );;
gap> SetName( Hd8b, "Hd8b" );
gap> gend12 := [ (15,16,17,18,19,20), (15,20)(16,19)(17,18) ];; 
gap> d12 := Group( gend12 );; 
gap> Gd12 := Groupoid( d12, [-37,-36,-35,-34] );;
gap> SetName( d12, "d12" );  
gap> SetName( Gd12, "Gd12" );
gap> s3 := Subgroup( d12, [ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ] );;
gap> Gs3 := SubgroupoidByPieces( Gd12, [ [ s3, [-36,-35,-34] ] ] );;
gap> SetName( s3, "s3" );  
gap> SetName( Gs3, "Gs3" );

## Section 5.6, Groupoid automorphisms 

## SubSection 5.6.1
gap> a4 := Subgroup( s4, [(1,2,3),(2,3,4)] );; 
gap> SetName( a4, "a4" ); 
gap> gensa4 := GeneratorsOfGroup( a4 );; 
gap> Ga4 := SubgroupoidByPieces( Gs4, [ [a4, [-15,-13,-11]] ] ); 
single piece groupoid: < a4, [ -15, -13, -11 ] >
gap> SetName( Ga4, "Ga4" ); 
gap> d := Arrow( Ga4, (1,3,4), -11, -13 ); 
[(1,3,4) : -11 -> -13]
gap> aut1 := GroupoidAutomorphismByObjectPerm( Ga4, [-13,-11,-15] );; 
gap> Display( aut1 ); 
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,2,3), (2,3,4) ] ]
images of objects: [ -13, -11, -15 ]
   images of rays: [ [() : -13 -> -13], [() : -13 -> -11], [() : -13 -> -15] ]
gap> d1 := ImageElm( aut1, d ); 
[(1,3,4) : -15 -> -11]
gap> h2 := GroupHomomorphismByImages( a4, a4, gensa4, [(2,3,4), (1,3,4)] );; 
gap> aut2 := GroupoidAutomorphismByGroupAuto( Ga4, h2 );; 
gap> Display( aut2 ); 
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (2,3,4), (1,3,4) ] ]
images of objects: [ -15, -13, -11 ]
   images of rays: [ [() : -15 -> -15], [() : -15 -> -13], [() : -15 -> -11] ]
gap> d2 := ImageElm( aut2, d1 );
[(1,2,4) : -15 -> -11]
gap> im3 := [(), (1,3,2), (2,4,3)];; 
gap> aut3 := GroupoidAutomorphismByRayShifts( Ga4, im3 );; 
gap> Display( aut3 ); 
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,2,3), (2,3,4) ] ]
images of objects: [ -15, -13, -11 ]
   images of rays: [ [() : -15 -> -15], [(1,3,2) : -15 -> -13], 
  [(2,4,3) : -15 -> -11] ]
gap> d3 := ImageElm( aut3, d2 );
[(1,4)(2,3) : -15 -> -11]
gap> d0 := Arrow( Ga4, (2,3,4), -11, -13 );; 
gap> aut4 := GroupoidInnerAutomorphism( Ga4, d0 );;
gap> Display( aut4 );
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,2,3), (2,3,4) ] ]
images of objects: [ -15, -11, -13 ]
   images of rays: [ [() : -15 -> -15], [(2,4,3) : -15 -> -11], 
  [(2,3,4) : -15 -> -13] ]
gap> d4 := ImageElm( aut4, d3 );
[(1,2,4) : -15 -> -13]
gap> aut1234 := aut1*aut2*aut3*aut4;; 
gap> Display( aut1234 ); 
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,4,3), (2,4,3) ] ]
images of objects: [ -11, -13, -15 ]
   images of rays: [ [() : -11 -> -11], [(1,2)(3,4) : -11 -> -13], 
  [(1,2)(3,4) : -11 -> -15] ]
gap> d4  = ImageElm( aut1234, d );
true
gap> inv1234 := InverseGeneralMapping( aut1234 );; 
gap> Display( inv1234 ); 
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,2,4), (1,2,3) ] ]
images of objects: [ -11, -13, -15 ]
   images of rays: [ [() : -11 -> -11], [() : -11 -> -13], 
  [(1,4)(2,3) : -11 -> -15] ]

# Subsection 5.6.2 
gap> s4c := Group( (1,2,3,4), (3,4) );; 
gap> SetName( s4c, "s4c" );
gap> s3c := Subgroup( s4c, [ (1,2), (2,3) ] );; 
gap> SetName( s3c, "s3c" ); 
gap> Gs4c := SinglePieceGroupoid( s4c, [-9,-8,-7,-6] );; 
gap> SetName( Gs4c, "Gs4c" ); 
gap> Hs3c := SubgroupoidWithRays( Gs4c, s3c, [ (), (1,4), (2,4), (3,4) ] );; 
gap> SetName( Hs3c, "Hs3c" ); 
gap> ## (1) automorphism by group auto 
gap> a1 := GroupHomomorphismByImages( s3c, s3c, [(1,2),(2,3)], [(1,3),(2,3)] );;
gap> aut1 := GroupoidAutomorphismByGroupAuto( Hs3c, a1 );
groupoid homomorphism : Hs3c -> Hs3c
[ [ [(1,2) : -9 -> -9], [(2,3) : -9 -> -9], [(1,4) : -9 -> -8], 
      [(2,4) : -9 -> -7], [(3,4) : -9 -> -6] ], 
  [ [(1,3) : -9 -> -9], [(2,3) : -9 -> -9], [(1,4) : -9 -> -8], 
      [(2,4) : -9 -> -7], [(3,4) : -9 -> -6] ] ]
gap> a := Arrow( Hs3c, (2,3,4), -8, -8 );
[(2,3,4) : -8 -> -8]
gap> ImageElm( aut1, a );
[(2,4,3) : -8 -> -8]
gap> b := Arrow( Hs3c, (1,2,3,4), -7, -6 );
[(1,2,3,4) : -7 -> -6]
gap> ##  b = (2,4)(1,2)(3,4) -> (2,4)(1,3)(3,4)
gap> ImageElm( aut1, b );
[(1,4,2,3) : -7 -> -6]
gap> ## (2) automorphism by object perm 
gap> aut2 := GroupoidAutomorphismByObjectPerm( Hs3c, [-8,-7,-6,-9] );
groupoid homomorphism : Hs3c -> Hs3c
[ [ [(1,2) : -9 -> -9], [(2,3) : -9 -> -9], [(1,4) : -9 -> -8], 
      [(2,4) : -9 -> -7], [(3,4) : -9 -> -6] ], 
  [ [(2,4) : -8 -> -8], [(2,3) : -8 -> -8], [(1,2,4) : -8 -> -7], 
      [(1,3,4) : -8 -> -6], [(1,4) : -8 -> -9] ] ] 
gap> ImageElm( aut2, a );                                           
[(1,4,3) : -7 -> -7]
gap> ImageElm( aut2, b );                                           
[(1,2)(3,4) : -6 -> -9]
gap> ## (3) automorphism by ray shifts 
gap> aut3 := GroupoidAutomorphismByRayShifts( Hs3c, [(),(2,3),(1,3),(1,2)] );   
groupoid homomorphism : Hs3c -> Hs3c
[ [ [(1,2) : -9 -> -9], [(2,3) : -9 -> -9], [(1,4) : -9 -> -8], 
      [(2,4) : -9 -> -7], [(3,4) : -9 -> -6] ], 
  [ [(1,2) : -9 -> -9], [(2,3) : -9 -> -9], [(1,4)(2,3) : -9 -> -8], 
      [(1,3)(2,4) : -9 -> -7], [(1,2)(3,4) : -9 -> -6] ] ]
gap> ImageElm( aut3, a );
[(2,4,3) : -8 -> -8]
gap> ImageElm( aut3, b );
[(1,4,2,3) : -7 -> -6]
gap> ## (4) combine these three automorphisms 
gap> aut := aut1 * aut2 * aut3;
groupoid homomorphism : Hs3c -> Hs3c
[ [ [(1,2) : -9 -> -9], [(2,3) : -9 -> -9], [(1,4) : -9 -> -8], 
      [(2,4) : -9 -> -7], [(3,4) : -9 -> -6] ], 
  [ [(2,4) : -8 -> -8], [(2,3) : -8 -> -8], [(1,2)(3,4) : -8 -> -7], 
      [(1,3)(2,4) : -8 -> -6], [(1,4)(2,3) : -8 -> -9] ] ]
gap> ImageElm( aut, a ); 
[(1,4,3) : -7 -> -7]
gap> ImageElm( aut, b );
[(1,2,3,4) : -6 -> -9]
gap> e86 := Arrow( Hs3c, (1,3,2,4), -8, -6 );;
gap> aut86 := GroupoidInnerAutomorphism( Hs3c, e86 );
groupoid homomorphism : Hs3c -> Hs3c
[ [ [(1,2) : -9 -> -9], [(2,3) : -9 -> -9], [(1,4) : -9 -> -8], 
      [(2,4) : -9 -> -7], [(3,4) : -9 -> -6] ], 
  [ [(1,2) : -9 -> -9], [(2,3) : -9 -> -9], [(2,4,3) : -9 -> -6], 
      [(2,4) : -9 -> -7], [(1,4)(2,3) : -9 -> -8] ] ]

# Subsection 5.6.3 
gap> AGa4 := AutomorphismGroupOfGroupoid( Ga4 ); 
Aut(Ga4)
gap> Length( GeneratorsOfGroup( AGa4 ) );
8
gap> AGgens := GeneratorsOfGroup( AGa4);; 
gap> NGa4 := NiceObject( AGa4 );; 
gap> MGa4 := NiceMonomorphism( AGa4 );; 
gap> Size( AGa4 ); 
20736
gap> SetName( AGa4, "AGa4" ); 
gap> SetName( NGa4, "NGa4" ); 
gap> ##  cannot test images of AGgens because of random variations 
gap> ##  Now do some tests!
gap> mgi := MappingGeneratorsImages( MGa4 );; 
gap> autgen := mgi[1];; 
gap> pcgen := mgi[2];;
gap> ngen := Length( autgen );; 
gap> ForAll( [1..ngen], i -> Order(autgen[i]) = Order(pcgen[i]) ); 
true

## SubSection 5.6.4
gap> AGa40 := Groupoid( AGa4, [0] );
single piece groupoid: < Aut(Ga4), [ 0 ] >
gap> conj := function(a) 
>            return ArrowNC( true, GroupoidInnerAutomorphism(Ga4,a), 0, 0 ); 
>            end;; 
gap> inner := MappingWithObjectsByFunction( Ga4, AGa40, conj, [0,0,0] );;
gap> a1 := Arrow( Ga4, (1,2,3), -15, -13 );;
gap> inn1 := ImageElm( inner, a1 );;                        
gap> a2 := Arrow( Ga4, (2,3,4), -13, -11 );;
gap> inn2 := ImageElm( inner, a2 );;       
gap> a3 := a1*a2;                      
[(1,3)(2,4) : -15 -> -11]
gap> inn3 := ImageElm( inner, a3 );  
[groupoid homomorphism : Ga4 -> Ga4
[ [ [(1,2,3) : -15 -> -15], [(2,3,4) : -15 -> -15], [() : -15 -> -13], 
      [() : -15 -> -11] ], 
  [ [(1,3,4) : -11 -> -11], [(1,2,4) : -11 -> -11], [(1,3)(2,4) : -11 -> -13],
      [() : -11 -> -15] ] ] : 0 -> 0]
gap> (inn3 = inn1*inn2*inn1) and (inn3 = inn2*inn1*inn2);
true

## SubSection 5.6.5
gap> Hs3 := HomogeneousDiscreteGroupoid( s3, [ -13..-10] ); 
homogeneous, discrete groupoid: < s3, [ -13 .. -10 ] >
gap> aut4 := GroupoidAutomorphismByObjectPerm( Hs3, [-12,-10,-11,-13] ); 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -12, -10, -11, -13 ]
object homomorphisms:
IdentityMapping( s3 )
IdentityMapping( s3 )
IdentityMapping( s3 )
IdentityMapping( s3 )

gap> gens3 := GeneratorsOfGroup( s3 );; 
gap> g1 := gens3[1];; 
gap> g2 := gens3[2];; 
gap> b1 := GroupHomomorphismByImages( s3, s3, gens3, [g1, g2^g1 ] );; 
gap> b2 := GroupHomomorphismByImages( s3, s3, gens3, [g1^g2, g2 ] );; 
gap> b3 := GroupHomomorphismByImages( s3, s3, gens3, [g1^g2, g2^(g1*g2) ] );; 
gap> b4 := GroupHomomorphismByImages( s3, s3, gens3, [g1^(g2*g1), g2^g1 ] );; 
gap> aut5 := GroupoidAutomorphismByGroupAutos( Hs3, [b1,b2,b3,b4] ); 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -13, -12, -11, -10 ]
object homomorphisms:
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,17,19)(16,18,20), (15,18)(16,17)(19,20) ] )
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,19,17)(16,20,18), (15,20)(16,19)(17,18) ] )
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,19,17)(16,20,18), (15,16)(17,20)(18,19) ] )
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,19,17)(16,20,18), (15,18)(16,17)(19,20) ] )

gap> AHs3 := AutomorphismGroupOfGroupoid( Hs3 ); 
<group with 4 generators>
gap> Size( AHs3 );
31104
gap> genAHs3 := GeneratorsOfGroup( AHs3 ); 
[ groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -13, -12, -11, -10 ]
    object homomorphisms:
    ConjugatorAutomorphism( s3, (15,19,17)(16,20,18) )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    , groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -13, -12, -11, -10 ]
    object homomorphisms:
    ConjugatorAutomorphism( s3, (15,20)(16,19)(17,18) )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    , groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -12, -11, -10, -13 ]
    object homomorphisms:
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    , groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -12, -13, -11, -10 ]
    object homomorphisms:
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
     ]
gap> nobAHs3 := NiceObject( AHs3 );; 
gap> nmonAHs3 := NiceMonomorphism( AHs3 );;
gap> w := genAHs3[1];; 
gap> w1 := ImageElm( nmonAHs3, w );; 
gap> x := genAHs3[2];; 
gap> x1 := ImageElm( nmonAHs3, x );; 
gap> y := genAHs3[3];; 
gap> y1 := ImageElm( nmonAHs3, y );; 
gap> z := genAHs3[4];; 
gap> z1 := ImageElm( nmonAHs3, z );; 
gap> u := z*w*y*x*z; 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -11, -13, -10, -12 ]
object homomorphisms:
IdentityMapping( s3 )
ConjugatorAutomorphism( s3, (15,19,17)(16,20,18) )
IdentityMapping( s3 )
ConjugatorAutomorphism( s3, (15,20)(16,19)(17,18) )

gap> u1 := z1*w1*y1*x1*z1; 
(1,2,4,3)(5,17,23,16,8,20,26,13)(6,18,24,15,7,19,25,14)(9,21,27,12,10,22,28,
11)
gap> imu := ImageElm( nmonAHs3, u );; 
gap> u1 = imu;
true

## SubSection 5.6.5
gap> Hd8 := HomogeneousGroupoid( Gd8,
>           [ [-20,-19,-18], [-12,-11,-10], [-16,-15,-14] ] );;
gap> SetName( Hd8, "Hd8" );
gap> AHd8 := AutomorphismGroupoidOfGroupoid( Hd8 ); 
Aut(Hd8)
gap> ObjectList( AHd8 );
[ [ -20, -19, -18 ], [ -16, -15, -14 ], [ -12, -11, -10 ] ]
gap> RaysOfGroupoid( AHd8 ){[2..3]};
[ groupoid homomorphism : 
    [ [ [(1,2,3,4) : -20 -> -20], [(1,3) : -20 -> -20], [() : -20 -> -19], 
          [() : -20 -> -18] ], 
      [ [(1,2,3,4) : -16 -> -16], [(1,3) : -16 -> -16], [() : -16 -> -15], 
          [() : -16 -> -14] ] ], groupoid homomorphism : 
    [ [ [(1,2,3,4) : -20 -> -20], [(1,3) : -20 -> -20], [() : -20 -> -19], 
          [() : -20 -> -18] ], 
      [ [(1,2,3,4) : -12 -> -12], [(1,3) : -12 -> -12], [() : -12 -> -11], 
          [() : -12 -> -10] ] ] ]
gap> ObjectGroup( AHd8, [ -12, -11, -10 ] );
<group with 8 generators>

## Section 5.7 
gap> reps := IrreducibleRepresentations( s4 );;
gap> rep4 := reps[4];; 
gap> Rs4 := Groupoid( Image( rep4 ), Gs4!.objects ); 
single piece groupoid: < Group([ [ [ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ], 
  [ [ 0, 0, 1 ], [ 1, 0, 0 ], [ 0, 1, 0 ] ], 
  [ [ -1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, -1 ] ], 
  [ [ 1, 0, 0 ], [ 0, -1, 0 ], [ 0, 0, -1 ] ] ]), [ -15, -14, -13, -12, -11 
 ] >
gap> gens := GeneratorsOfGroupoid( Gs4 );
[ [(1,2,3,4) : -15 -> -15], [(3,4) : -15 -> -15], [() : -15 -> -14], 
  [() : -15 -> -13], [() : -15 -> -12], [() : -15 -> -11] ]
gap> images := List( gens, 
>        g -> Arrow( Rs4, ImageElm(rep4,g![1]), g![2], g![3] ) ); 
[ [[ [ -1, 0, 0 ], [ 0, 0, 1 ], [ 0, -1, 0 ] ] : -15 -> -15], 
  [[ [ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ] : -15 -> -15], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -14], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -13], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -12], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -11] ]
gap> mor := GroupoidHomomorphismFromSinglePiece( Gs4, Rs4, gens, images );
groupoid homomorphism : 
[ [ [(1,2,3,4) : -15 -> -15], [(3,4) : -15 -> -15], [() : -15 -> -14], 
      [() : -15 -> -13], [() : -15 -> -12], [() : -15 -> -11] ], 
  [ [[ [ -1, 0, 0 ], [ 0, 0, 1 ], [ 0, -1, 0 ] ] : -15 -> -15], 
      [[ [ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ] : -15 -> -15], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -14], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -13], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -12], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -11] ] ]
gap> IsMatrixGroupoid( Rs4 ); 
true
gap> a := Arrow( Hs4, (1,4,2), -12, -13 );
[(1,4,2) : -12 -> -13]
gap> ImageElm( mor, a );
[[ [ 0, 0, -1 ], [ -1, 0, 0 ], [ 0, 1, 0 ] ] : -12 -> -13]
gap> rmor := RestrictedMappingGroupoids( mor, Hd8b );
groupoid homomorphism : 
[ [ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14], [(1,2,3) : -14 -> -13], 
      [(1,2,4) : -14 -> -12] ], 
  [ [[ [ -1, 0, 0 ], [ 0, 0, 1 ], [ 0, -1, 0 ] ] : -14 -> -14], 
      [[ [ 1, 0, 0 ], [ 0, 0, -1 ], [ 0, -1, 0 ] ] : -14 -> -14], 
      [[ [ 0, 0, 1 ], [ -1, 0, 0 ], [ 0, -1, 0 ] ] : -14 -> -13], 
      [[ [ 0, -1, 0 ], [ 0, 0, 1 ], [ -1, 0, 0 ] ] : -14 -> -12] ] ]
gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "gpdaut.tst", 10000 );

#############################################################################
##
#E  gpdaut.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
