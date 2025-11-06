############################################################################
##
#W  rt-act.tst                   Groupoids Package             Chris Wensley
##

## Section 4.6 
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

gap> m3 := FullTransformationMonoid( 3 );;
gap> SetName( m3, "m3" );
gap> rm3 := GroupoidWithMonoidObjects( m3 ); 
groupoid with 5 pieces:
1:  single piece groupoid: < gp(m3), [ Transformation( [ 1, 1, 1 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 3, 3, 3 ] ) ] >
2:  single piece groupoid: < gp(m3), [ Transformation( [ 1, 1, 2 ] ), 
  Transformation( [ 1, 1 ] ), Transformation( [ 2, 2, 1 ] ), 
  Transformation( [ 2, 2 ] ), Transformation( [ 3, 3, 1 ] ), 
  Transformation( [ 3, 3, 2 ] ) ] >
3:  single piece groupoid: < gp(m3), [ Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 1, 3, 1 ] ), Transformation( [ 2, 1, 2 ] ), 
  Transformation( [ 2, 3, 2 ] ), Transformation( [ 3, 1, 3 ] ), 
  Transformation( [ 3, 2, 3 ] ) ] >
4:  single piece groupoid: < gp(m3), [ Transformation( [ 1, 2, 2 ] ), 
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 2, 1, 1 ] ), 
  Transformation( [ 2, 3, 3 ] ), Transformation( [ 3, 1, 1 ] ), 
  Transformation( [ 3, 2, 2 ] ) ] >
5:  single piece groupoid: < gp(m3), 
[ IdentityTransformation, Transformation( [ 1, 3, 2 ] ), 
  Transformation( [ 2, 1 ] ), Transformation( [ 2, 3, 1 ] ), 
  Transformation( [ 3, 1, 2 ] ), Transformation( [ 3, 2, 1 ] ) ] >
gap> orm3 := ObjectList( rm3 );;
gap> Homset(rm3,orm3[1],orm3[2]);
fail
gap> Homset( rm3, orm3[2], orm3[3] ); 
<homset Transformation( [ 1, 1, 2 ] ) -> Transformation( [ 1, 1 ] )
  with head group gp(m3)>
gap> Display( last );                
<homset Transformation( [ 1, 1, 2 ] ) -> Transformation( [ 1, 1 ] )
  with elements:
[IdentityTransformation : Transformation( [ 1, 1, 2 ] ) -> Transformation( [ 
 1, 1 ] )]
[Transformation( [ 1, 3, 2 ] ) : Transformation( [ 1, 1, 2 ] ) -> 
Transformation( [ 1, 1 ] )]
[Transformation( [ 2, 1 ] ) : Transformation( [ 1, 1, 2 ] ) -> 
Transformation( [ 1, 1 ] )]
[Transformation( [ 2, 3, 1 ] ) : Transformation( [ 1, 1, 2 ] ) -> 
Transformation( [ 1, 1 ] )]
[Transformation( [ 3, 1, 2 ] ) : Transformation( [ 1, 1, 2 ] ) -> 
Transformation( [ 1, 1 ] )]
[Transformation( [ 3, 2, 1 ] ) : Transformation( [ 1, 1, 2 ] ) -> 
Transformation( [ 1, 1 ] )]
gap> Homset( rm3, orm3[1], orm3[14] );
<homset Transformation( [ 1, 1, 1 ] ) -> Transformation( [ 2, 2, 2 ] )
  with head group gp(m3)>
gap> Display( last );                 
<homset Transformation( [ 1, 1, 1 ] ) -> Transformation( [ 2, 2, 2 ]\
 )
  with elements:
[IdentityTransformation : Transformation( [ 1, 1, 1 ] ) -> Transformation( [ 
 2, 2, 2 ] )]
[Transformation( [ 1, 3, 2 ] ) : Transformation( [ 1, 1, 1 ] ) -> 
Transformation( [ 2, 2, 2 ] )]
[Transformation( [ 2, 1 ] ) : Transformation( [ 1, 1, 1 ] ) -> 
Transformation( [ 2, 2, 2 ] )]
[Transformation( [ 2, 3, 1 ] ) : Transformation( [ 1, 1, 1 ] ) -> 
Transformation( [ 2, 2, 2 ] )]
[Transformation( [ 3, 1, 2 ] ) : Transformation( [ 1, 1, 1 ] ) -> 
Transformation( [ 2, 2, 2 ] )]
[Transformation( [ 3, 2, 1 ] ) : Transformation( [ 1, 1, 1 ] ) -> 
Transformation( [ 2, 2, 2 ] )]

gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
