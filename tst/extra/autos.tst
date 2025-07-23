############################################################################
##
#W  autos.tst                   Groupoids Package              Chris Wensley
##

gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

gap> a4gens := [ (1,2,3), (2,3,4) ];;
gap> a4 := Group( a4gens );;
gap> SetName( a4, "a4" );;
gap> obs := [-9,-8,-7];;
gap> Ga4 := SinglePieceGroupoid( a4, obs );;
gap> Display( Ga4 );
single piece groupoid: 
  objects: [ -9, -8, -7 ]
    group: a4 = <[ (1,2,3), (2,3,4) ]>
gap> a4 := Ga4!.magma;; 
gap> ga4 := GeneratorsOfGroup( a4 );;
gap> obs := Ga4!.objects;; 
gap> ro := obs[1];; 
gap> nobs := Length( obs );; 
gap> AGa4 := AutomorphismGroupOfGroupoid( Ga4 ); 
<group with 8 generators>
gap> Size( AGa4 );
20736
gap> Agens := GeneratorsOfGroup( AGa4 );; 
gap> Length( Agens );
8
gap> g37 := Agens[3]*Agens[7];;
gap> g37 in AGa4; 
true
gap> g73 := Agens[7]*Agens[3];;
gap> g37 = g73; 
false

gap> a := Arrow( Ga4, (1,2,3), -9, -8 );
[(1,2,3) : -9 -> -8]
gap> b := Arrow( Ga4, (2,3,4), -8, -7 );
[(2,3,4) : -8 -> -7]
gap> c := Arrow( Ga4, (1,3,4), -7, -7 );
[(1,3,4) : -7 -> -7]
gap> p := GroupoidAutomorphismByObjectPerm( Ga4, [-8,-9,-7] ); 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -8 -> -8], [(2,3,4) : -8 -> -8], [() : -8 -> -9], 
      [() : -8 -> -7] ] ]
gap> ImageElm( p, a );
[(1,2,3) : -8 -> -9]
gap> ImageElm( p, b );
[(2,3,4) : -9 -> -7]
gap> ImageElm( p, c );
[(1,3,4) : -7 -> -7]
gap> q := GroupoidAutomorphismByObjectPerm( Ga4, [-9,-7,-8] ); 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -7], 
      [() : -9 -> -8] ] ]
gap> ImageElm( q, a );
[(1,2,3) : -9 -> -7]
gap> ImageElm( q, b );
[(2,3,4) : -7 -> -8]
gap> ImageElm( q, c );
[(1,3,4) : -8 -> -8]
gap> autk := GroupHomomorphismByImages( a4, a4, a4gens, [ (1,3,2), (1,3,4) ] ); 
[ (1,2,3), (2,3,4) ] -> [ (1,3,2), (1,3,4) ]
gap> k := GroupoidAutomorphismByGroupAuto( Ga4, autk ); 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,3,2) : -9 -> -9], [(1,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ] ]
gap> ImageElm( k, a );
[(1,3,2) : -9 -> -8]
gap> ImageElm( k, b );
[(1,3,4) : -8 -> -7]
gap> ImageElm( k, c );
[(2,3,4) : -7 -> -7]
gap> autl := GroupHomomorphismByImages( a4, a4, a4gens, [ (1,4,3), (2,4,3) ] ); 
[ (1,2,3), (2,3,4) ] -> [ (1,4,3), (2,4,3) ]
gap> l := GroupoidAutomorphismByGroupAuto( Ga4, autl ); 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,4,3) : -9 -> -9], [(2,4,3) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ] ]
gap> ImageElm( l, a );      
[(1,4,3) : -9 -> -8]
gap> ImageElm( l, b );
[(2,4,3) : -8 -> -7]
gap> ImageElm( l, c );
[(1,3,2) : -7 -> -7]
gap> rayr := [ ( ), (2,3,4), (1,3,4) ];; 
gap> r := GroupoidAutomorphismByRayShifts( Ga4, rayr ); 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [(2,3,4) : -9 -> -8], 
      [(1,3,4) : -9 -> -7] ] ]
gap> ImageElm( r, a );
[(1,3)(2,4) : -9 -> -8]
gap> ImageElm( r, b );
[(1,3,4) : -8 -> -7]
gap> ImageElm( r, c );
[(1,3,4) : -7 -> -7]
gap> rays := [ ( ), (1,2)(3,4), (1,3)(2,4) ];;
gap> s := GroupoidAutomorphismByRayShifts( Ga4, rays ); 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [(1,2)(3,4) : -9 -> -8], 
      [(1,3)(2,4) : -9 -> -7] ] ]
gap> ImageElm( s, a );
[(2,4,3) : -9 -> -8]
gap> ImageElm( s, b );
[(2,3,4) : -8 -> -7]
gap> ImageElm( s, c );
[(1,2,3) : -7 -> -7]
gap> kr := k*r; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,3,2) : -9 -> -9], [(1,3,4) : -9 -> -9], [(2,3,4) : -9 -> -8], 
      [(1,3,4) : -9 -> -7] ] ]
gap> krp := kr*p; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,3,2) : -8 -> -8], [(1,3,4) : -8 -> -8], [(2,3,4) : -8 -> -9], 
      [(1,3,4) : -8 -> -7] ] ]
gap> ls := l*s; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,4,3) : -9 -> -9], [(2,4,3) : -9 -> -9], [(1,2)(3,4) : -9 -> -8], 
      [(1,3)(2,4) : -9 -> -7] ] ]
gap> ImageElm( ls, a );                                                    
[(1,3,2) : -9 -> -8]
gap> ImageElm( ls, b );
[(1,2,3) : -8 -> -7]
gap> ImageElm( ls, c );
[(1,4,3) : -7 -> -7]
gap> lsq := ls*q; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,4,3) : -9 -> -9], [(2,4,3) : -9 -> -9], [(1,2)(3,4) : -9 -> -7], 
      [(1,3)(2,4) : -9 -> -8] ] ]
gap> ImageElm( lsq, a );
[(1,3,2) : -9 -> -7]
gap> ImageElm( lsq, b );
[(1,2,3) : -7 -> -8]
gap> ImageElm( lsq, c );
[(1,4,3) : -8 -> -8]
gap> pq := p*q; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -7 -> -7], [(2,3,4) : -7 -> -7], [() : -7 -> -9], 
      [() : -7 -> -8] ] ]
gap> ImageElm( pq, a ); 
[(1,2,3) : -7 -> -9]
gap> ImageElm( pq, b );
[(2,3,4) : -9 -> -8]
gap> ImageElm( pq, c );
[(1,3,4) : -8 -> -8]
gap> qp := q*p;
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -8 -> -8], [(2,3,4) : -8 -> -8], [() : -8 -> -7], 
      [() : -8 -> -9] ] ]
gap> ImageElm( qp, a );
[(1,2,3) : -8 -> -7]
gap> ImageElm( qp, b );
[(2,3,4) : -7 -> -9]
gap> ImageElm( qp, c );
[(1,3,4) : -9 -> -9]
gap> kl := k*l; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,3,4) : -9 -> -9], [(1,3,2) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ] ]
gap> ImageElm( kl, a );     
[(1,3,4) : -9 -> -8]
gap> ImageElm( kl, b );
[(1,3,2) : -8 -> -7]
gap> ImageElm( kl, c );
[(2,4,3) : -7 -> -7]
gap> lk := l*k;
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(2,4,3) : -9 -> -9], [(1,4,3) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ] ]
gap> ImageElm( lk, a );     
[(2,4,3) : -9 -> -8]
gap> ImageElm( lk, b );
[(1,4,3) : -8 -> -7]
gap> ImageElm( lk, c );
[(1,2,3) : -7 -> -7]
gap> rs := r*s; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [(1,2,4) : -9 -> -8], 
      [(2,4,3) : -9 -> -7] ] ]
gap> ImageElm( rs, a );
[(1,4)(2,3) : -9 -> -8]
gap> ImageElm( rs, b );
[(1,4,2) : -8 -> -7]
gap> ImageElm( rs, c );
[(1,2,3) : -7 -> -7]
gap> sr := s*r;
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [(1,3,2) : -9 -> -8], 
      [(1,4,2) : -9 -> -7] ] ]
gap> ImageElm( sr, a );
[() : -9 -> -8]
gap> ImageElm( sr, b );
[(1,3,4) : -8 -> -7]
gap> ImageElm( sr, c );
[(2,4,3) : -7 -> -7]
gap> kp := k*p; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,3,2) : -8 -> -8], [(1,3,4) : -8 -> -8], [() : -8 -> -9], 
      [() : -8 -> -7] ] ]
gap> ImageElm( kp, a );
[(1,3,2) : -8 -> -9]
gap> ImageElm( kp, b );
[(1,3,4) : -9 -> -7]
gap> ImageElm( kp, c );
[(2,3,4) : -7 -> -7]
gap> kp=p*k; 
true
gap> rp := r*p; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -8 -> -8], [(2,3,4) : -8 -> -8], [(2,3,4) : -8 -> -9], 
      [(1,3,4) : -8 -> -7] ] ]
gap> ImageElm( rp, a );                                                    
[(1,3)(2,4) : -8 -> -9]
gap> ImageElm( rp, b );
[(1,3,4) : -9 -> -7]
gap> ImageElm( rp, c );
[(1,3,4) : -7 -> -7]
gap> pr := p*r;
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,3,4) : -8 -> -8], [(2,3,4) : -8 -> -8], [(2,4,3) : -8 -> -9], 
      [(1,3,2) : -8 -> -7] ] ]
gap> ImageElm( pr, a );
[(1,2,4) : -8 -> -9]
gap> ImageElm( pr, b );
[(1,3)(2,4) : -9 -> -7]
gap> ImageElm( pr, c );
[(1,3,4) : -7 -> -7]
gap> rp = pr; 
false
gap> r*k = kr;
false

gap> ## check the formula a_c * a_pi = a_pi * a_pi(c)
gap> prayr := List( ImagesOfObjects(p), o -> rayr[ Position(obs,o) ] );
[ (2,3,4), (), (1,3,4) ]
gap> g := prayr[1];;
gap> autg := InnerAutomorphism( a4, g );;
gap> prayr := List( prayr, e -> g^-1*e );
[ (), (2,4,3), (1,3,2) ]
gap> r0 := GroupoidAutomorphismByGroupAuto( Ga4, autg )       
>          * GroupoidAutomorphismByRayShifts( Ga4, prayr );
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,3,4) : -9 -> -9], [(2,3,4) : -9 -> -9], [(2,4,3) : -9 -> -8], 
      [(1,3,2) : -9 -> -7] ] ]
gap> rp = p*r0;
true

gap> ## check the formula a_c * a_kappa = a_kappa * a_kappa(c) 
gap> krayr := List( rayr, e -> ImageElm( autk, e ) );
[ (), (1,3,4), (2,3,4) ]
gap> r1 := GroupoidAutomorphismByRayShifts( Ga4, krayr );;
gap> r*k = k*r1;                                         
true

gap> ## check the conjugation formula: c(ab)= c(a)*c(b)*c(a) = c(b)*c(a)*c(b)
gap> x := a*b;
[(1,3)(2,4) : -9 -> -7]
gap> auta:=GroupoidInnerAutomorphism( Ga4, a );
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -8 -> -8], [(1,4,3) : -8 -> -8], [(1,2,3) : -8 -> -9], 
      [(1,3,2) : -8 -> -7] ] ]
gap> autb:=GroupoidInnerAutomorphism( Ga4, b );
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [(2,3,4) : -9 -> -7], 
      [(2,4,3) : -9 -> -8] ] ]
gap> autx:=GroupoidInnerAutomorphism( Ga4, x );
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,3,4) : -7 -> -7], [(1,2,4) : -7 -> -7], [(1,3)(2,4) : -7 -> -8], 
      [() : -7 -> -9] ] ]
gap> auta*autb*auta;
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,3,4) : -7 -> -7], [(1,2,4) : -7 -> -7], [(1,3)(2,4) : -7 -> -8], 
      [() : -7 -> -9] ] ]
gap> autx=auta*autb*auta;
true
gap> autx=autb*auta*autb;
true

gap> Na4 := MaximalDiscreteSubgroupoid( Ga4 ); 
homogeneous, discrete groupoid: < a4, [ -9, -8, -7 ] >
gap> q2 := GroupoidAutomorphismByObjectPerm( Na4, [-9,-7,-8] );;
gap> ImageElm( q2, c ); 
[(1,3,4) : -8 -> -8]
gap> ida4 := IdentityMapping( a4 );; 
gap> k2 := GroupoidAutomorphismByGroupAutos( Na4, [autk,autk,ida4] );
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -9, -8, -7 ] -> [ -9, -8, -7 ]
object homomorphisms:
GroupHomomorphismByImages( a4, a4, [ (1,2,3), (2,3,4) ], [ (1,3,2), (1,3,4) 
 ] )
GroupHomomorphismByImages( a4, a4, [ (1,2,3), (2,3,4) ], [ (1,3,2), (1,3,4) 
 ] )
IdentityMapping( a4 )
gap> ImageElm( k2, c ) = c;
true
gap> qk2 := q2 * k2;;
gap> ImageElm( qk2, c ); 
[(2,3,4) : -8 -> -8]
gap> 

gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
