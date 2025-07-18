############################################################################
##
#W  autos2.tst                  Groupoids Package              Chris Wensley
##

gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

gap> s4 := Group( (1,2,3,4), (3,4) );;
gap> SetName( s4, "s4" );
gap> d8 := Subgroup( s4, [ (1,2,3,4), (2,4) ] );;
gap> SetName( d8, "d8" );
gap> Gs4 := SinglePieceGroupoid( s4, [-9,-8,-7,-6] ); 
single piece groupoid: < s4, [ -9, -8, -7, -6 ] >
gap> SetName( Gs4, "Gs4" ); 
gap> Gd8 := SubgroupoidWithRays( Gs4, d8, [(),(1,2,3),(),(1,2,3)] );
single piece groupoid with rays: < d8, [ -9, -8, -7, -6 ], 
[ (), (1,2,3), (), (1,2,3) ] >
gap> SetName( Gd8, "Gd8" ); 
gap> isosg := IsomorphismStandardGroupoid( Gd8, [-5,-4,-3,-2] ); 
groupoid homomorphism : 
[ [ [(1,2,3,4) : -9 -> -9], [(2,4) : -9 -> -9], [(1,2,3) : -9 -> -8], 
      [() : -9 -> -7], [(1,2,3) : -9 -> -6] ], 
  [ [(1,2,3,4) : -5 -> -5], [(2,4) : -5 -> -5], [() : -5 -> -4], 
      [() : -5 -> -3], [() : -5 -> -2] ] ]
gap> isono := IsomorphismNewObjects( Gs4, [-19,-18,-17,-16] );;
gap> Fs4 := Image( isono ); 
single piece groupoid: < s4, [ -19, -18, -17, -16 ] >
gap> SetName( Fs4, "Fs4" );
gap> isod8 := RestrictedMappingGroupoids( isono, Gd8 ); 
groupoid homomorphism : 
[ [ [(1,2,3,4) : -9 -> -9], [(2,4) : -9 -> -9], [(1,2,3) : -9 -> -8], 
      [() : -9 -> -7], [(1,2,3) : -9 -> -6] ], 
  [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ] ]
gap> Fd8 := ImagesSource( isod8 );
single piece groupoid with rays: < Group( [ (1,2,3,4), (2,4) ] ), 
[ -19, -18, -17, -16 ], [ (), (1,2,3), (), (1,2,3) ] >
gap> SetName( Fd8, "Fd8" ); 

gap> alpha := GroupoidAutomorphismByObjectPerm( Gs4, [-8,-7,-6,-9] ); 
groupoid homomorphism : Gs4 -> Gs4
[ [ [(1,2,3,4) : -9 -> -9], [(3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7], [() : -9 -> -6] ], 
  [ [(1,2,3,4) : -8 -> -8], [(3,4) : -8 -> -8], [() : -8 -> -7], 
      [() : -8 -> -6], [() : -8 -> -9] ] ]
gap> coll := GeneratorsOfGroupoid( Gd8 );
[ [(1,2,3,4) : -9 -> -9], [(2,4) : -9 -> -9], [(1,2,3) : -9 -> -8], 
  [() : -9 -> -7], [(1,2,3) : -9 -> -6] ]
gap> acoll := List( coll, a -> ImageElm( alpha, a ) ); 
[ [(1,2,3,4) : -8 -> -8], [(2,4) : -8 -> -8], [(1,2,3) : -8 -> -7], 
  [() : -8 -> -6], [(1,2,3) : -8 -> -9] ]
gap> Hd8a := SinglePieceSubgroupoidByGenerators( Gs4, acoll );; 
gap> Display( Hd8a ); 
single piece groupoid with rays having: 
supergroupoid: single piece groupoid: < s4, [ -9, -8, -7, -6 ] >
      objects: [ -9, -8, -7, -6 ]
   root group: Group( [ (1,4,2,3), (3,4) ] )
         rays: [ (), (1,3,2), (), (1,3,2) ]
gap> resalpha := RestrictedMappingGroupoids( alpha, Gd8 ); 
groupoid homomorphism : 
[ [ [(1,2,3,4) : -9 -> -9], [(2,4) : -9 -> -9], [(1,2,3) : -9 -> -8], 
      [() : -9 -> -7], [(1,2,3) : -9 -> -6] ], 
  [ [(1,2,3,4) : -8 -> -8], [(2,4) : -8 -> -8], [(1,2,3) : -8 -> -7], 
      [() : -8 -> -6], [(1,2,3) : -8 -> -9] ] ]
gap> IsAutomorphismWithObjects( resalpha ); 
false
gap> Jd8a := ImagesSource( resalpha ); 
single piece groupoid with rays: < Group( [ (1,4,2,3), (3,4) ] ), 
[ -9, -8, -7, -6 ], [ (), (1,3,2), (), (1,3,2) ] >

gap> beta := GroupoidAutomorphismByObjectPerm( Gs4, [-7,-6,-9,-8] ); 
groupoid homomorphism : Gs4 -> Gs4
[ [ [(1,2,3,4) : -9 -> -9], [(3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7], [() : -9 -> -6] ], 
  [ [(1,2,3,4) : -7 -> -7], [(3,4) : -7 -> -7], [() : -7 -> -6], 
      [() : -7 -> -9], [() : -7 -> -8] ] ]
gap> bcoll := List( coll, a -> ImageElm( beta, a ) ); 
[ [(1,2,3,4) : -7 -> -7], [(2,4) : -7 -> -7], [(1,2,3) : -7 -> -6], 
  [() : -7 -> -9], [(1,2,3) : -7 -> -8] ]
gap> Hd8b := SinglePieceSubgroupoidByGenerators( Gs4, bcoll );; 
gap> Display( Hd8b ); 
single piece groupoid with rays having: 
supergroupoid: single piece groupoid: < s4, [ -9, -8, -7, -6 ] >
      objects: [ -9, -8, -7, -6 ]
   root group: Group( [ (1,2,3,4), (2,4) ] )
         rays: [ (), (1,2,3), (), (1,2,3) ]
gap> resbeta := RestrictedMappingGroupoids( beta, Gd8 ); 
groupoid homomorphism : 
[ [ [(1,2,3,4) : -9 -> -9], [(2,4) : -9 -> -9], [(1,2,3) : -9 -> -8], 
      [() : -9 -> -7], [(1,2,3) : -9 -> -6] ], 
  [ [(1,2,3,4) : -7 -> -7], [(2,4) : -7 -> -7], [(1,2,3) : -7 -> -6], 
      [() : -7 -> -9], [(1,2,3) : -7 -> -8] ] ]
gap> IsAutomorphismWithObjects( resbeta );
true
gap> Jd8b := ImagesSource( resbeta ); 
single piece groupoid with rays: < Group( [ (1,2,3,4), (2,4) ] ), 
[ -9, -8, -7, -6 ], [ (), (1,2,3), (), (1,2,3) ] >

gap> aut1 := GroupoidAutomorphismByObjectPerm( Fd8, [-17,-16,-19,-18] ); 
groupoid homomorphism : Fd8 -> Fd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -17 -> -17], [(2,4) : -17 -> -17], [(1,2,3) : -17 -> -16], 
      [() : -17 -> -19], [(1,2,3) : -17 -> -18] ] ]
gap> IsAutomorphismWithObjects( aut1 ); 
true
gap> iso1 := isod8 * aut1;; 
gap> Display( iso1 ); 
homomorphism to single piece groupoid: Gd8 -> Fd8
root group homomorphism:
(1,2,3,4) -> (1,2,3,4)
(2,4) -> (2,4)
object map: [ -9, -8, -7, -6 ] -> [ -17, -16, -19, -18 ]
ray images: [ (), (1,2,3), (), (1,2,3) ]
gap> a := Arrow( Gd8, (1,2,3), -9, -8 );; 
gap> a1 := ImageElm( iso1, a ); 
[(1,2,3) : -17 -> -16]
gap> b := Arrow( Gd8, (), -9, -7 );; 
gap> b1 := ImageElm( iso1, b ); 
[() : -17 -> -19]
gap> c := Arrow( Gd8, (1,2,3), -9, -6 );; 
gap> c1 := ImageElm( iso1, c ); 
[(1,2,3) : -17 -> -18]
gap> e := Arrow( Gd8, (1,3,4,2), -8, -7 );; 
gap> e1 := ImageElm( iso1, e ); 
[(1,3,4,2) : -16 -> -19]

gap> hom := GroupHomomorphismByImages( d8, d8, 
>               [ (1,2,3,4), (2,4) ], [ (1,2,3,4), (1,3) ] );;
gap> aut2 := GroupoidAutomorphismByGroupAuto( Fd8, hom );
groupoid homomorphism : Fd8 -> Fd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -19 -> -19], [(1,3) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ] ]
gap> IsAutomorphismWithObjects( aut2 ); 
true
gap> iso2 := isod8 * aut2;;
gap> Display( iso2 ); 
homomorphism to single piece groupoid: Gd8 -> Fd8
root group homomorphism:
(1,2,3,4) -> (1,2,3,4)
(2,4) -> (1,3)
object map: [ -9, -8, -7, -6 ] -> [ -19, -18, -17, -16 ]
ray images: [ (), (1,2,3), (), (1,2,3) ]
gap> a2 := ImageElm( iso2, a ); 
[(1,2,3) : -19 -> -18]
gap> b2 := ImageElm( iso2, b ); 
[() : -19 -> -17]
gap> c2 := ImageElm( iso2, c ); 
[(1,2,3) : -19 -> -16]
gap> e2 := ImageElm( iso2, e ); 
[(2,3) : -18 -> -17]
gap> a12 := ImageElm( aut2, a1 ); 
[(1,2,3) : -17 -> -16]
gap> b12 := ImageElm( aut2, b1 ); 
[() : -17 -> -19]
gap> c12 := ImageElm( aut2, c1 ); 
[(1,2,3) : -17 -> -18]
gap> e12 := ImageElm( aut2, e1 ); 
[(2,3) : -16 -> -19]

gap> shifts := [ (), (1,2)(3,4), (1,4,3,2), (1,4)(2,3) ];; 
gap> aut3 := GroupoidAutomorphismByRayShifts( Fd8, shifts ); 
groupoid homomorphism : Fd8 -> Fd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(2,4,3) : -19 -> -18], 
      [(1,4,3,2) : -19 -> -17], [(1,3,4) : -19 -> -16] ] ]
gap> IsAutomorphismWithObjects( aut3 ); 
true
gap> iso3 := isod8 * aut3;;
gap> Display( iso3 ); 
homomorphism to single piece groupoid: Gd8 -> Fd8
root group homomorphism:
(1,2,3,4) -> (1,2,3,4)
(2,4) -> (2,4)
object map: [ -9, -8, -7, -6 ] -> [ -19, -18, -17, -16 ]
ray images: [ (), (2,4,3), (1,4,3,2), (1,3,4) ]
gap> a3 := ImageElm( iso3, a ); 
[(2,4,3) : -19 -> -18]
gap> b3 := ImageElm( iso3, b ); 
[(1,4,3,2) : -19 -> -17]
gap> c3 := ImageElm( iso3, c ); 
[(1,3,4) : -19 -> -16]
gap> e3 := ImageElm( iso3, e ); 
[(1,4,3) : -18 -> -17]
gap> a123 := ImageElm( aut3, a12 ); 
[(1,2,4,3) : -17 -> -16]
gap> b123 := ImageElm( aut3, b12 ); 
[(1,2,3,4) : -17 -> -19]
gap> c123 := ImageElm( aut3, c12 ); 
[(1,4) : -17 -> -18]
gap> e123 := ImageElm( aut3, e12 ); 
[(1,4) : -16 -> -19]

gap> inv3 := InverseGeneralMapping( iso3 ); 
groupoid homomorphism : Fd8 -> Gd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -9 -> -9], [(2,4) : -9 -> -9], [(2,4,3) : -9 -> -8], 
      [(1,2,3,4) : -9 -> -7], [(1,3,4) : -9 -> -6] ] ]
gap> inv2 := InverseGeneralMapping( iso2 ); 
groupoid homomorphism : Fd8 -> Gd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -9 -> -9], [(1,3) : -9 -> -9], [(1,2,3) : -9 -> -8], 
      [() : -9 -> -7], [(1,2,3) : -9 -> -6] ] ]
gap> inv1 := InverseGeneralMapping( iso1 );
groupoid homomorphism : Fd8 -> Gd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -7 -> -7], [(2,4) : -7 -> -7], [(1,2,3) : -7 -> -6], 
      [() : -7 -> -9], [(1,2,3) : -7 -> -8] ] ]
gap> ainv3 := InverseGeneralMapping( aut3 ); 
groupoid homomorphism : Fd8 -> Fd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(2,4,3) : -19 -> -18], 
      [(1,2,3,4) : -19 -> -17], [(1,3,4) : -19 -> -16] ] ]
gap> ainv2 := InverseGeneralMapping( aut2 ); 
groupoid homomorphism : Fd8 -> Fd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -19 -> -19], [(1,3) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ] ]
gap> ainv1 := InverseGeneralMapping( aut1 );
groupoid homomorphism : Fd8 -> Fd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -17 -> -17], [(2,4) : -17 -> -17], [(1,2,3) : -17 -> -16], 
      [() : -17 -> -19], [(1,2,3) : -17 -> -18] ] ]
gap> aut123 := aut1 * aut2 * aut3;
groupoid homomorphism : Fd8 -> Fd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -17 -> -17], [(2,4) : -17 -> -17], [(1,2,4,3) : -17 -> -16], 
      [(1,2,3,4) : -17 -> -19], [(1,4) : -17 -> -18] ] ]
gap> iso123 := isod8 * aut123;
groupoid homomorphism : Gd8 -> Fd8
[ [ [(1,2,3,4) : -9 -> -9], [(2,4) : -9 -> -9], [(1,2,3) : -9 -> -8], 
      [() : -9 -> -7], [(1,2,3) : -9 -> -6] ], 
  [ [(1,2,3,4) : -17 -> -17], [(2,4) : -17 -> -17], [(1,2,4,3) : -17 -> -16], 
      [(1,2,3,4) : -17 -> -19], [(1,4) : -17 -> -18] ] ]
gap> a0 := ImageElm( iso123, a ); 
[(1,2,4,3) : -17 -> -16]
gap> b0 := ImageElm( iso123, b ); 
[(1,2,3,4) : -17 -> -19]
gap> c0 := ImageElm( iso123, c ); 
[(1,4) : -17 -> -18]
gap> e0 := ImageElm( iso123, e ); 
[(1,4) : -16 -> -19]
gap> ainv321 := ainv3 * ainv2 * ainv1; 
groupoid homomorphism : Fd8 -> Fd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -17 -> -17], [(1,3) : -17 -> -17], [(2,4,3) : -17 -> -16], 
      [(1,2,3,4) : -17 -> -19], [(1,4,2) : -17 -> -18] ] ]
gap> invd8 := InverseGeneralMapping( isod8 ); 
groupoid homomorphism : Fd8 -> Gd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -9 -> -9], [(2,4) : -9 -> -9], [(1,2,3) : -9 -> -8], 
      [() : -9 -> -7], [(1,2,3) : -9 -> -6] ] ]
gap> inv321 := ainv321 * invd8; 
groupoid homomorphism : Fd8 -> Gd8
[ [ [(1,2,3,4) : -19 -> -19], [(2,4) : -19 -> -19], [(1,2,3) : -19 -> -18], 
      [() : -19 -> -17], [(1,2,3) : -19 -> -16] ], 
  [ [(1,2,3,4) : -7 -> -7], [(1,3) : -7 -> -7], [(2,4,3) : -7 -> -6], 
      [(1,2,3,4) : -7 -> -9], [(1,4,2) : -7 -> -8] ] ]

gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
