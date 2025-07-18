############################################################################
##
#W  gpdhom.gi              GAP4 package `groupoids'            Chris Wensley
#W                                                              & Emma Moore

############################################################################
##  Standard error messages

GROUPOID_MAPPING_CONSTRUCTORS := Concatenation(
    "The standard operations which construct a groupoid mapping are:\n", 
    "1.  GroupoidHomomorphism( src, rng, hom );\n", 
    "2.  GroupoidHomomorphism( src, rng, hom, oims, imrays );\n", 
    "3.  GroupoidHomomorphismFromHomogeneousDiscrete(src,rng,homs,oims);\n", 
    " or GroupoidHomomorphism( one of the following parameter options );\n",
    "4.  GroupoidHomomorphismFromSinglePiece( src, rng, gens, images );\n",
    "5.  HomomorphismToSinglePiece( src, rng, list of [gens,images]'s );\n",
    "6.  HomomorphismByUnion( src, rng, list of disjoint homomorphisms );\n", 
    "7.  GroupoidAutomorphismByGroupAuto( gpd, auto );\n", 
    "8.  GroupoidAutomorphismByObjectPerm( gpd, oims );\n", 
    "9.  GroupoidAutomorphismByRayShifts( gpd, rims );\n" ); 

############################################################################
##
#M  IsGroupoidHomomorphismFromHomogeneousDiscrete( <hom> )
##
InstallImmediateMethod( IsGroupoidHomomorphismFromHomogeneousDiscrete, 
    "for a groupoid hom", IsGroupoidHomomorphism and 
                          IsGeneralMappingFromHomogeneousDiscrete, 0,
function( map ) 
    return true; 
end ); 

############################################################################
##
#M  IsGeneratorsOfMagmaWithInverses( <homlist> )
##
InstallMethod( IsGeneratorsOfMagmaWithInverses, "for a list of groupoid maps", 
    true, [ IsGeneralMappingWithObjectsCollection ], 0,
function( homlist ) 
    Info( InfoGroupoids, 1, 
          "#I  using IsGeneratorsOfMagmaWithInverses in gpdhom.gi" ); 
    return ForAll( homlist, 
        m -> ( ( Source(m) = Range(m) ) 
               and IsEndomorphismWithObjects(m) 
               and IsInjectiveOnObjects(m) 
               and IsSurjectiveOnObjects(m) ) ); 
end );
 
############################################################################
##
#M  IdentityMapping
##
InstallOtherMethod( IdentityMapping, "for a groupoid", true, 
    [ IsGroupoid and IsSinglePiece ], 0,
function( gpd )

    local gens, iso;

    Info( InfoGroupoids, 3, "using IdentityMapping  in gpdhom.gi" ); 
    gens := GeneratorsOfGroupoid( gpd );
    iso := GroupoidHomomorphismFromSinglePieceNC( gpd, gpd, gens, gens );
    SetIsInjectiveOnObjects( iso, true );
    SetIsSurjectiveOnObjects( iso, true );
    return iso;
end );

############################################################################
##
##  GroupoidHomomorphism( <gpd>,<hom>|<oims>|<rays>             automorphisms 
#F  GroupoidHomomorphism( <g1>,<g2>,<hom> )                 from single piece 
#F  GroupoidHomomorphism( <g1>,<g2>,<piece images> )       single piece range 
#F  GroupoidHomomorphism( <g1>,<g2>,<maps> )                union of mappings 
#F  GroupoidHomomorphism( <g>,<auto> )             automorphism by group auto
#F  GroupoidHomomorphism( <g>,<imobs> )           automorphism by object perm
#F  GroupoidHomomorphism( <g>,<rays> )           automorphism by ray products
##
InstallGlobalFunction( GroupoidHomomorphism, function( arg )

    local nargs, src, rng, id, rays, ob1, ob2, i, g, pt, ph, 
          gens1, gens2, ngens, nobs, images, e, a; 

    nargs := Length( arg );
    if ( ( nargs < 2 ) 
         or ( nargs > 5 )  
         or not IsMagmaWithObjects( arg[1] ) 
         or ( ( nargs > 2 ) and not IsMagmaWithObjects( arg[2] ) ) ) then
        Info( InfoGroupoids, 1, GROUPOID_MAPPING_CONSTRUCTORS );
        return fail;
    fi;
    # various types of automorphism 
    if ( ( nargs = 2 ) and IsSinglePiece( arg[1] ) ) then 
        Info( InfoGroupoids, 3, "gpd hom with 2 arguments" ); 
        if IsGroupHomomorphism( arg[2] ) then 
            return GroupoidAutomorphismByGroupAuto( arg[1], arg[2] ); 
        elif ( IsHomogeneousList( arg[2] ) 
               and ( arg[2][1] in arg[1]!.objects ) ) then 
            return GroupoidAutomorphismByObjectPerm( arg[1], arg[2] ); 
        else 
            return GroupoidAutomorphismByRayShifts( arg[1], arg[2] ); 
        fi; 
    # gpd, gpd, group hom 
    elif ( ( nargs = 3 ) and IsSinglePiece( arg[1] ) 
           and IsSinglePiece( arg[2] ) and IsGroupHomomorphism( arg[3] ) ) then 
        Info( InfoGroupoids, 3, "gpd hom with 3 arguments" ); 
        gens1 := GeneratorsOfGroupoid( arg[1] ); 
        ob1 := arg[1]!.objects; 
        ngens := Length( gens1 );
        ob2 := arg[2]!.objects; 
        if not ( Length( ob1 ) = Length( ob2 ) ) then 
            Error( "groupoids have different numbers of objects" );
        fi; 
        gens2 := [1..ngens]; 
        for i in [1..ngens] do 
            g := gens1[i]; 
            pt := Position( ob1, g![3] ); 
            ph := Position( ob1, g![4] ); 
            gens2[i] := Arrow( arg[2], Image(arg[3],g![2]), ob2[pt], ob2[ph] );
        od;
        return GroupoidHomomorphismFromSinglePieceNC( 
                   arg[1], arg[2], gens1, gens2 ); 
    # mwo, mwo, list of mappings
    elif ( ( nargs = 3 ) and IsHomogeneousList( arg[3] ) 
            and IsHomomorphismToSinglePiece( arg[3][1] ) ) then
        Info( InfoGroupoids, 2, "HomomorphismByUnion" );
        return HomomorphismByUnion( arg[1], arg[2], arg[3] );
    # gpd, gpd, list of [hom, images of objects, rays] triples
    elif ( ( nargs = 3 ) and IsHomogeneousList( arg[3] ) 
           and IsList( arg[3][1] ) and IsList( arg[3][1][1] ) ) then 
        Info( InfoGroupoids, 2, "HomomorphismToSinglePiece" );
        return HomomorphismToSinglePiece( arg[1], arg[2], arg[3] ); 
    elif ( ( nargs = 4 ) and IsHomogeneousList( arg[3] ) 
           and ( IsHomogeneousList( arg[4] ) ) ) then 
        Info( InfoGroupoids, 2, "HomomorphismFromSinglePiece" ); 
        return GroupoidHomomorphismFromSinglePieceNC( 
                   arg[1], arg[2], arg[3], arg[4] ); 
    elif ( nargs = 5 ) then 
        Info( InfoGroupoids, 3, "gpd hom with 5 arguments" ); 
        gens1 := GeneratorsOfGroupoid( arg[1] ); 
        images := ShallowCopy( gens1 ); 
        ngens := Length( images );
        ob1 := arg[1]!.objects; 
        nobs := Length( ob1 );
        for i in [1..ngens] do 
            g := gens1[i]; 
            pt := arg[4][ Position( ob1, g![3] ) ]; 
            ph := arg[4][ Position( ob1, g![4] ) ]; 
            if ( pt = ph ) then 
                e := ImageElm( arg[3], g![2] ); 
                a := Arrow( arg[2], e, pt, ph ); 
            else 
                a := Arrow( arg[2], arg[5][i-ngens+nobs], pt, ph ); 
                if ( a = fail ) then 
                    Error( "image arrow = fail" );
                fi;
            fi;
            images[i] := a; 
        od; 
        return GroupoidHomomorphismFromSinglePieceNC( 
                   arg[1], arg[2], gens1, images ); 
    else
        Info( InfoGroupoids, 1, GROUPOID_MAPPING_CONSTRUCTORS );
        return fail;
    fi;
end );

############################################################################
##
#M  InclusionMappingGroupoids
##
InstallMethod( InclusionMappingGroupoids, "for sgpd of single piece groupoid", 
    true, [ IsGroupoid and IsSinglePiece, IsGroupoid ], 0,
function( gpd, sgpd ) 

    local sobs, o1, c1, mappings, comps, m, gens, mor;

    if not IsSubgroupoid( gpd, sgpd ) then
        ## Error( "arg[2] is not a subgroupoid of arg[1]" );
        return fail;
    fi;
    gens := GeneratorsOfGroupoid( sgpd );
    if IsSinglePiece( sgpd ) then
        sobs := sgpd!.objects;
        if IsSinglePiece( gpd ) then
            Info( InfoGroupoids, 2, 
                  "InclusionMapping, one piece -> one piece" );
            return GroupoidHomomorphismFromSinglePieceNC( 
                       sgpd, gpd, gens, gens );
        else
            o1 := sgpd!.objects[1];
            c1 := PieceOfObject( gpd, o1 );
            if ( c1 = fail ) then
                Error( "magma mapping fails to include" );
            fi;
            mor := GroupoidHomomorphismFromSinglePieceNC( 
                       sgpd, c1, gens, gens );
            return MagmaWithObjectsHomomorphism( sgpd, gpd, [ mor ] );
        fi;
    else
        Info( InfoGroupoids, 2, "InclusionMapping, source not connected" );
        mappings := List( Pieces( sgpd ),
                           c -> InclusionMappingGroupoids( gpd, c ) );
        return HomomorphismByUnion( sgpd, gpd, mappings );
    fi;
end );

InstallMethod( InclusionMappingGroupoids, "for a subgroupoid of a groupoid", 
    true, [ IsGroupoid, IsGroupoid ], 0,
function( A, B )

    local PA, PB, nA, nB, obsA, try, maps, i, p, found, j, q, inc, incobs; 

    if not ( HasPieces( A ) or HasPieces( B ) ) then 
        Error( "unexpected case in InclusionMappingGroupoids" ); 
    fi; 
    PA := Pieces( A ); 
    PB := Pieces( B ); 
    nA := Length( PA ); 
    nB := Length( PB );
    obsA := ObjectList( A );
    try := [1..nA];
    maps := [1..nB]; 

    for i in [1..nB] do
        p := PB[i];
        found := false; 
        j := 0; 
        while ( ( not found ) and ( j < Length( try ) ) ) do 
            j := j+1; 
            q := PA[j];
            inc := InclusionMappingGroupoids( q, p );
            if not ( inc = fail ) then
                incobs := ImagesOfObjects( inc );
                if not ForAll( incobs, o -> o in obsA ) then
                    inc := fail;
                fi;
            fi;
            if not ( inc = fail ) then 
                found := true; 
                maps[i] := inc; 
                obsA := Difference( obsA, incobs );
            fi; 
        od; 
        if ( not found ) then 
            return fail; 
        fi; 
    od; 
    return HomomorphismByUnion( B, A, maps );
end );

InstallMethod( InclusionMappingGroupoids, "from hom discrete to single piece", 
    true, [ IsGroupoid and IsSinglePiece, IsHomogeneousDiscreteGroupoid ], 0,
function( gpd, sgpd ) 

    local obs, inc, homs; 

    Info( InfoGroupoids, 2, "new InclusionMappingGroupoids method" ); 
    if not IsSubdomainWithObjects( gpd, sgpd ) then
        Error( "arg[2] is not a submagma of arg[1]" );
    fi; 
    obs := sgpd!.objects; 
    inc := InclusionMappingGroups( gpd!.magma, sgpd!.magma ); 
    homs := List( obs, o -> inc ); 
    return GroupoidHomomorphismFromHomogeneousDiscrete( 
               sgpd, gpd, homs, obs ); 
end );

############################################################################
##
#M  RestrictedMappingGroupoids
##
InstallMethod( RestrictedMappingGroupoids, "for a groupoid mapping", true,
    [ IsGeneralMappingWithObjects, IsGroupoid ], 0,
function( mor, U )

    local smor, rmor, pieces, nobs, imobs, autos, genU, imres, V, res, par,
          i, P, genP, imP, obi, imobi, ogi, imgpi, impieces, imU,
          mrng, psrc, lsrc, rcomp, rrng, j, imgenP, pos, hom, rng;

    if HasIsHomogeneousDiscreteGroupoid( U )
       and IsHomogeneousDiscreteGroupoid( U )
       and HasIsGroupWithObjectsHomomorphism( mor )
       and IsGroupWithObjectsHomomorphism( mor ) then
        Info( InfoGroupoids, 1, "RestrictedMapping from hom discrete" ); 
        smor := Source( mor );
        rmor := Range( mor );
        if not ( IsSubdomainWithObjects( smor, U ) ) then
            Error( "U not a submagma of Source(mor)" );
        fi; 
        pieces := Pieces( U );
        nobs := Length( pieces );
        imobs := ListWithIdenticalEntries( nobs, 0 ); 
        autos := ListWithIdenticalEntries( nobs, 0 ); 
        impieces := [ ]; 
        for i in [1..nobs] do 
            P := pieces[i];
            genP := GeneratorsOfGroupoid( P ); 
            obi := genP[1]![3]; 
            imP := List( genP, a -> ImageElm( mor, a ) ); 
            imobi := imP[1]![2];
            imobs[i] := imobi; 
            ogi := ObjectGroup( rmor, imobi ); 
            imgpi := Subgroup( ogi, List( imP, a -> a![2] ) ); 
            autos[i] := GroupHomomorphismByImages( 
                            ObjectGroup( P, obi ), imgpi, 
                            List( genP, g->g![2] ), List( imP, g->g![2] ) ); 
            Add( impieces, SubgroupoidByPieces( rmor, [[imgpi,[imobi]]] ) );
        od; 
        imU := SubgroupoidByPieces( rmor, impieces );
        res := GroupoidHomomorphismFromHomogeneousDiscrete(
                   U, imU, autos, imobs ); 
    elif HasIsSinglePiece( U ) and IsSinglePiece( U ) then
        Info( InfoGroupoids, 1, "RestrictedMapping from a single piece" ); 
        smor := Source( mor );
        rmor := Range( mor );
        if not ( IsSubdomainWithObjects( smor, U ) ) then
            Error( "U not a submagma of Source(mor)" );
        fi; 
        genU := GeneratorsOfGroupoid( U ); 
        imres := List( genU, g -> ImageElm( mor, g ) ); 
        V := SinglePieceSubgroupoidByGenerators( Range(mor), imres );
        res := GroupoidHomomorphismFromSinglePiece( U, V, genU, imres ); 
        SetIsSurjective( res, true ); 
        if ( HasIsInjective( mor ) and IsInjective( mor ) ) then
            SetIsInjective( res, true );
        fi;
    else
        Info( InfoGroupoids, 1, "RestrictedMapping from a union" ); 
        mrng := Range( mor );
        psrc := Pieces( U ); 
        lsrc := Length( psrc );
        rcomp := ListWithIdenticalEntries( lsrc, 0 ); 
        rrng := ListWithIdenticalEntries( lsrc, 0 );
        for j in [1..lsrc] do 
            P := psrc[j]; 
            genP := GeneratorsOfGroupoid( P ); 
            imgenP := List( genP, g -> ImageElm( mor, g ) ); 
            pos := PieceNrOfObject( mrng, imgenP[1]![3] ); 
            imP := SinglePieceSubgroupoidByGenerators( 
                       Pieces(mrng)[pos], imgenP );
            hom := GroupoidHomomorphism( P, imP, genP, imgenP ); 
            rcomp[j] := hom; 
            rrng[j] := Range( hom );
        od; 
        rng := Groupoid( rrng );
        res := HomomorphismByUnionNC( U, rng, rcomp );
    fi;
    if ( HasIsInjective( mor ) and IsInjective( mor ) ) then
        SetIsInjective( res, true );
    fi;
    par := mor;
    if HasParentMappingGroupoids( mor ) then 
        par := ParentMappingGroupoids( mor ); 
    fi; 
    SetParentMappingGroupoids( res, par );
    return res;
end );

############################################################################
##
#M  RootGroupHomomorphism . . . . . . . for a groupoid hom from single piece 
##
InstallMethod( RootGroupHomomorphism, "for a groupoid hom from a single piece", 
    true, [ IsGroupoidHomomorphism and IsHomomorphismToSinglePiece ], 0,
function( mor )

    local gpd1, gpd2, ob2, imobs, roh, mgi, ray, gp2, im2, hom;

    Info( InfoGroupoids, 3, 
          "method for RootGroupHomomorphism in gpdhom.gi" );
    return MappingToSinglePieceData( mor )[1][1];

    gpd2 := Range( mor ); 
    ob2 := gpd2!.objects; 
    imobs := ImagesOfObjects( mor ); 
    roh := MappingToSinglePieceData( mor )[1][1];
    if ( imobs[1] = ob2[1] ) then  ## root maps to root 
        hom := roh; 
    elif ( HasIsDirectProductWithCompleteDigraph( gpd2 ) and 
           IsDirectProductWithCompleteDigraph( gpd2 ) ) then 
        hom := roh; 
    else 
        mgi := MappingGeneratorsImages( roh ); 
        ray := RaysOfGroupoid( gpd2 )[ Position( ob2, imobs[1] ) ]; 
        gp2 := ObjectGroup( gpd2, imobs[1] ); 
        im2 := List( mgi[2], g -> g^ray ); 
        hom := GroupHomomorphismByImages( Source( roh ), gp2, mgi[1], im2 );  
    fi; 
    return hom; 
end ); 

############################################################################
##
#M  ObjectGroupHomomorphism . . . . .  . . . . . . . . . . for a groupoid hom 
##
InstallMethod( ObjectGroupHomomorphism, "for a groupoid hom and an object", 
    true, [ IsGroupoidHomomorphism, IsObject ], 0,
function( mor, obj ) 

    local src, rng, imobs, pos, obg, gens, loops, imloops, imgens, img, hom; 

    src := Source( mor );
    rng := Range( mor );
    if ( HasIsGeneralMappingFromSinglePiece( mor ) 
         and IsGeneralMappingFromSinglePiece( mor ) ) then 
        imobs := ImagesOfObjects( mor ); 
        pos := Position( src!.objects, obj );
        obg := ObjectGroup( src, obj );
        gens := GeneratorsOfGroup( obg ); 
        loops := List( gens, g -> Arrow( src, g, obj, obj ) ); 
        imloops := List( loops, a -> ImageElm( mor, a ) ); 
        imgens := List( imloops, a -> a![2] ); 
        img := Group( imgens ); 
        hom := GroupHomomorphismByImages( obg, img, gens, imgens ); 
    elif IsGeneralMappingFromHomogeneousDiscrete( mor ) then 
        pos := Position( src!.objects, obj ); 
        hom := ObjectHomomorphisms( mor )[pos]; 
    elif HasPiecesOfMapping( mor ) then 
        pos := Position( Pieces( src ), obj ); 
        hom := ObjectGroupHomomorphism( PiecesOfMapping( mor )[pos], obj ); 
    else 
        Error( "this is a case not yet provided for" ); 
    fi; 
    return hom; 
end ); 

############################################################################
##
#M  ImageElementsOfRays . . . . . .  . . . . . . . . . . for a groupoid hom 
##
InstallMethod( ImageElementsOfRays, "for a groupoid homomorphism", 
    true, [ IsGroupoidHomomorphism ], 0,
function( hom )
    local  data;
    if HasMappingToSinglePieceData( hom ) then 
        data := MappingToSinglePieceData( hom );
        if IsList( data[1] ) then
            if ( Length( data[1] ) = 3 ) then
                return data[1][3];
            fi;
        fi;
    fi;
    Error( "fail with ImageElementsOfRays" );
    return fail;
end );

############################################################################
##
#M  MappingPermObjectsImages . . . . . for list of objects and their images 
#M  MappingTransObjectsImages . . . .  for list of objects and their images 
##
InstallMethod( MappingPermObjectsImages, "for objects and their images", true, 
    [ IsList, IsList ], 0,
function( obs, ims )

    local len, L; 

    len := Length( obs ); 
    if ( len <> Length(ims) ) then
        Error("<obs> and <ims> have different lengths");
    fi; 
    L := ShallowCopy( ims ); 
    Sort( L ); 
    if not ( L = obs ) then 
        return fail; 
    fi;
    L := List( obs, o -> Position( ims, o ) ); 
    return PermList( L ); 
end );

InstallMethod( MappingTransObjectsImages, "for objects and their images", true, 
    [ IsList, IsList ], 0,
function( obs, ims )

    local len, L; 

    if not IsSubset( Set(obs), Set(ims) ) then 
        Error( "ims is not a subset of obs" ); 
    fi;
    len := Length( obs ); 
    L := List( ims, o -> Position( obs, o ) ); 
    return Transformation( L ); 
end );

############################################################################
##
#M  ObjectTransformationOfGroupoidHomomorphism . . . for a groupoid mapping 
##
InstallMethod( ObjectTransformationOfGroupoidHomomorphism, 
    "for objects and images", true, 
    [ IsGroupWithObjectsHomomorphism and IsGroupoidEndomorphism ], 0,
function( map )

    local obs, ims; 

    ims := ImagesOfObjects( map );
    obs := Filtered( ObjectList( Range(map) ), o -> o in ims ); 
    if not ( Length(obs) = Length(ims) ) then 
        Info( InfoGroupoids, 1, 
            "ObjectTransformationOfGroupoidHomomorphism set to <fail>" );
        return fail; 
    fi; 
    if IsInjectiveOnObjects( map ) then 
        return MappingPermObjectsImages( obs, ims );
    else 
        return MappingTransObjectsImages( obs, ims ); 
    fi; 
end ); 

#############################################################################
##
#M  IsInjective( map ) . . . . . . . . . . . . . for a groupoid homomorphism
##
InstallOtherMethod( IsInjective, "for a groupoid hom from a single piece",
    true,
    [ IsGroupWithObjectsHomomorphism and IsGeneralMappingFromSinglePiece ],
    2,
    map -> ( IsInjectiveOnObjects( map ) and 
             IsInjective( RootGroupHomomorphism( map ) ) ) );

InstallOtherMethod( IsInjective, "for a groupoid hom to a single piece", true, 
    [ IsGroupWithObjectsHomomorphism and IsGeneralMappingToSinglePiece ], 0,
function( map ) 
    Error( "no method yet implemented" ); 
end );

InstallOtherMethod( IsInjective, "for a groupoid homomorphism by union", true, 
    [ IsGroupWithObjectsHomomorphism and HasPiecesOfMapping ], 0,
    map -> ForAll( PiecesOfMapping, IsInjective ) );

InstallOtherMethod( IsInjective, "for a groupoid homomorphism", true, 
    [ IsGroupWithObjectsHomomorphism ], 0,
function( map ) 
    Error( "no method yet implemented for this case" ); 
end );

#############################################################################
##
#M  IsSurjective( map ) . . . . . . . . . . . .  for a groupoid homomorphism
##
InstallOtherMethod( IsSurjective, "for a mapping from a single piece", true, 
    [ IsGroupWithObjectsHomomorphism and IsGeneralMappingFromSinglePiece ], 2,
    map -> ( IsSurjectiveOnObjects( map ) and 
             IsSurjective( RootGroupHomomorphism( map ) ) ) );

InstallOtherMethod( IsSurjective, "for a groupoid hom to a single piece", true, 
    [ IsGroupWithObjectsHomomorphism and IsGeneralMappingToSinglePiece ], 0,
function( map ) 
    Error( "no method yet implemented" ); 
end );

InstallOtherMethod( IsSurjective, "for a groupoid homomorphism by union", true, 
    [ IsGroupWithObjectsHomomorphism and HasPiecesOfMapping ], 0,
    map -> ForAll( PiecesOfMapping, IsSurjective ) );

InstallOtherMethod( IsSurjective, "for a groupoid homomorphism", true, 
    [ IsGroupWithObjectsHomomorphism ], 0,
function( map ) 
    Error( "no method yet implemented for this case" ); 
end );

#############################################################################
##
#M  IsSingleValued( map ) . . . . . . . . . . .  for a groupoid homomorphism
##
InstallOtherMethod( IsSingleValued, "method for a groupoid homomorphism", 
    true, [ IsGroupoidHomomorphism ], 0, map -> true );

#############################################################################
##
#M  IsTotal( map ) . . . . . . . . . . . . . .   for a groupoid homomorphism
##
InstallOtherMethod( IsTotal, "method for a groupoid homomorphism", true, 
    [ IsGroupoidHomomorphism ], 0, map -> true );

#############################################################################
##
#M  IsBijective( map ) . . . . . . . . . . . . .  for a 2Dimensional-mapping
##
InstallOtherMethod( IsBijective, "method for a groupoid homomorphism", true, 
    [ IsGroupoidHomomorphism ], 0,
    map -> ( IsInjective( map ) and IsSurjective( map ) ) );

############################################################################
##
#M  MappingGeneratorsImages 
##
InstallMethod( MappingGeneratorsImages, "for a mapping to a single piece", 
    true, [ IsGroupoidHomomorphism and IsHomomorphismToSinglePiece ], 0,
function ( hom ) 
    
    local maps; 

    maps := MappingToSinglePieceMaps( hom ); 
    return List( maps, MappingGeneratorsImages ); 
end );

InstallMethod( MappingGeneratorsImages, "for mapping from hom discrete", true,
    [ IsGroupoidHomomorphism and 
      IsGroupoidHomomorphismFromHomogeneousDiscrete ], 0,
function ( hom ) 
    
    local src, rng, obs, nobs, obhoms, oims, gens, ngens, imgs, 
          i, a, g, o, pos, imo, img; 

    src := Source( hom ); 
    rng := Range( hom ); 
    obs := ObjectList( src );
    nobs := Length( obs );
    oims := ImagesOfObjects( hom );
    obhoms := ObjectHomomorphisms( hom ); 
    gens := GeneratorsOfGroupoid( src ); 
    ngens := Length( gens ); 
    imgs := ListWithIdenticalEntries( ngens, 0 ); 
    for i in [1..ngens] do 
        a := gens[i]; 
        g := a![2]; 
        o := a![3]; 
        pos := Position( obs, o ); 
        imo := oims[pos];         
        img := ImageElm( obhoms[pos], g ); 
        imgs[i] := ArrowNC( a![1], true, img, imo, imo ); 
    od;
    return [ gens, imgs ]; 
end );

############################################################################
##
#M  Display
##
InstallMethod( Display, "for a mapping from a homogeneous discrete gpd", true, 
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep ], 0,
function ( map ) 

    local h; 

    Info( InfoGroupoids, 2, "display method for discrete homs in gpdhom.gi" );
    Print( "homogeneous discrete groupoid mapping: " ); 
    Print( "[ ", Source(map), " ] -> [ ", Range(map), " ]\n" ); 
    Print( "images of objects: ", ImagesOfObjects( map ), "\n" ); 
    Print( "object homomorphisms:\n" ); 
    for h in ObjectHomomorphisms( map ) do 
        Print( h, "\n" ); 
    od; 
end );

#############################################################################
##
#M  \=( <hom1>, <hom2> ) . . . . test if two groupoid homomorphisms are equal
##
InstallMethod( \=, "for a groupoid homomorphisms", true, 
    [ IsGroupoidHomomorphism, IsGroupoidHomomorphism ], 
function ( hom1, hom2 )
    local  genS, a, im1, im2;
    Info( InfoGroupoids, 2, "running \= for groupoid homomorphisms" );
    if not ( Source( hom1 ) = Source( hom2 ) )
       and ( Range( hom1 ) = Range( hom2 ) ) then
        Info( InfoGroupoids, 2, "unequal source and/or range" );
        return false;
    fi;
    genS := GeneratorsOfGroupoid( Source( hom1 ) );
    for a in genS do
        im1 := ImageElm( hom1, a );
        im2 := ImageElm( hom2, a );
        if ( im1 <> im2 ) then
            return false;
        fi;
    od;
    return true;
end );

############################################################################
##
#M  GroupoidHomomorphismFromSinglePieceNC 
#M  GroupoidHomomorphismFromSinglePiece 
##
InstallMethod( GroupoidHomomorphismFromSinglePieceNC,
    "generic method for a mapping of connected groupoids", true,
    [ IsGroupoid and IsSinglePiece, IsGroupoid and IsSinglePiece, 
      IsHomogeneousList, IsHomogeneousList ], 0,
function( src, rng, gens, images )

    local isfrom, isto, obs, nobs, ngens, nggens, posr, imr, map, 
          gps, gpr, hgen, himg, hom, oims, gprid, rims, ok;

    isfrom := ( HasIsGroupoidByIsomorphisms( src ) 
                and IsGroupoidByIsomorphisms( src ) ); 
    isto := ( HasIsGroupoidByIsomorphisms( rng ) 
              and IsGroupoidByIsomorphisms( rng ) ); 
    obs := src!.objects; 
    nobs := Length( obs ); 
    ngens := Length( gens ); 
    nggens := ngens-nobs+1; 
    posr := nggens+1; 
    imr := images[1]![3];
    gps := src!.magma; 
    hgen := List( [1..nggens], i -> gens[i]![2] ); 
    himg := List( [1..nggens], i -> images[i]![2] ); 
    gpr := ObjectGroup( rng, imr );
    oims := Concatenation( [imr], 
                           List( [posr..ngens], i -> images[i]![4] ) ); 
    if isfrom then 
        hgen := List( hgen, L -> L[1] ); 
    fi;
    if isto then 
        gprid := [ One( gpr ), One( gpr ) ]; 
        himg := List( himg, L -> L[1] ); 
    else 
        gprid := One( gpr ); 
    fi;
    hom := GroupHomomorphismByImagesNC( gps, gpr, hgen, himg ); 
    rims := Concatenation( [ gprid ], 
                           List( [posr..ngens], i -> images[i]![2] ) );
    map := rec(); 
    ObjectifyWithAttributes( map, GroupoidHomomorphismType, 
        Source, src, 
        Range, rng, 
        RootGroupHomomorphism, hom, 
        ImagesOfObjects, oims, 
        ImageElementsOfRays, rims,  
        IsGeneralMappingWithObjects, true, 
        IsGroupWithObjectsHomomorphism, true, 
        IsHomomorphismToSinglePiece, true, 
        RespectsMultiplication, true );
    ok := IsInjectiveOnObjects( map ); 
    ok := IsSurjectiveOnObjects( map ); 
    SetIsHomomorphismFromSinglePiece( map, true ); 
    SetMappingToSinglePieceData( map, [ [ hom, oims, rims ] ] );
    if ( src = rng ) then 
        SetIsEndoGeneralMapping( map, true ); 
        SetIsEndomorphismWithObjects( map, true ); 
        ## ok := IsAutomorphismWithObjects( map ); 
    fi; 
    SetMappingGeneratorsImages( map, [ gens, images ] ); 
    if ( isfrom or isto ) then 
        SetIsGroupoidHomomorphismWithGroupoidByIsomorphisms( map, true ); 
    fi;
    return map; 
end );

InstallMethod( GroupoidHomomorphismFromSinglePiece,
    "generic method for a mapping of connected groupoids", true,
    [ IsGroupoid and IsSinglePiece, IsGroupoid and IsSinglePiece, 
      IsHomogeneousList, IsHomogeneousList ], 0, 
function( src, rng, gens, images ) 

    local obs, nobs, ngens, nggens, posr, imr, i; 

    if not ( gens = GeneratorsOfGroupoid( src ) ) then 
        Error( "gens <> GeneratorsOfGroupoid(src)" ); 
    fi; 
    if not ForAll( images, g -> ( g in rng ) ) then
        Error( "images not all in rng" ); 
    fi; 
    if not ( Length( gens ) = Length( images ) ) then 
        Error( "gens and images should have the same length" ); 
    fi;
    obs := ObjectList( src ); 
    nobs := Length( obs ); 
    ngens := Length( gens ); 
    nggens := ngens-nobs+1;
    posr := nggens+1; 
    imr := images[1]![3]; 
    for i in [1..nggens] do 
        if not ( ( images[i]![3] = imr ) and ( images[i]![4] = imr ) ) then 
            Error( "images[i] not a loop at the root vertex" ); 
        fi; 
    od;
    for i in [posr+1..ngens] do 
        if not ( images[i]![3] = imr ) then 
            Error( "all ray images should have the same source" ); 
        fi; 
    od; 
    return GroupoidHomomorphismFromSinglePieceNC( src, rng, gens, images ); 
end ); 

############################################################################
##
#M  ImageElm( <map>, <e> )
##
InstallOtherMethod( ImageElm, "for a groupoid mapping between single pieces", 
    true, [ IsGroupoidHomomorphism and IsHomomorphismFromSinglePiece, 
    IsGroupoidElement ], 0,
function ( map, e )

    local src, rng, imo, obs1, pt1, ph1, ray1, rims, loop, iloop, g2;

    Info( InfoGroupoids, 3, 
          "this is the first ImageElm method in gpdhom.gi" ); 
    src := Source( map ); 
    rng := Range( map );
    if not ( e in src ) then 
        Error( "the element e is not in the source of mapping map" ); 
    fi; 
    imo := ImagesOfObjects( map ); 
    obs1 := src!.objects; 
    pt1 := Position( obs1, e![3] );
    ph1 := Position( obs1, e![4] ); 
    ray1 := RaysOfGroupoid( src ); 
    loop := ray1[pt1] * e![2] * ray1[ph1]^(-1); 
    iloop := ImageElm( RootGroupHomomorphism( map ), loop ); 
    rims := ImageElementsOfRays( map ); 
    g2 := rims[pt1]^-1 * iloop * rims[ph1]; 
    return ArrowNC( rng, true, g2, imo[pt1], imo[ph1] );
end ); 

InstallOtherMethod( ImageElm, "for a mapping from/to groupoid by isomorphisms", 
    true, [ IsGroupoidHomomorphismWithGroupoidByIsomorphisms,  
    IsGroupoidElement ], 10,
function ( map, e )

    local src, rng, isfrom, isto, isos1, isos2, obs1, obs2, rays1, rays2, 
          pt1, ph1, it1, gt1, ih1, gh1, rh1, irh1, loop, iloop, isoth, 
          rgh, imo, pr2, pt2, ph2, ir2, it2, ih2, rims, g2;

    Info( InfoGroupoids, 3, 
          "this is the second ImageElm method in gpdhom.gi" ); 
    src := Source( map ); 
    rng := Range( map ); 
    isfrom := ( HasIsGroupoidByIsomorphisms( src ) 
                and IsGroupoidByIsomorphisms( src ) ); 
    if isfrom then 
        isos1 := src!.isomorphisms; 
    fi; 
    isto := ( HasIsGroupoidByIsomorphisms( rng ) 
              and IsGroupoidByIsomorphisms( rng ) ); 
    if isto then 
        isos2 := rng!.isomorphisms; 
    fi; 
    if not ( e in src ) then 
        Error( "the element e is not in the source of mapping map" ); 
    fi;
    obs1 := src!.objects; 
    obs2 := rng!.objects; 
    rays1 := RaysOfGroupoid( src ); 
    rays2 := RaysOfGroupoid( rng ); 
    pt1 := Position( obs1, e![3] ); 
    ph1 := Position( obs1, e![4] ); 
    rgh := RootGroupHomomorphism( map ); 
    if isfrom then 
        it1 := InverseGeneralMapping( isos1[pt1] ); 
        gt1 := ImageElm( it1, e![2][1] ); 
        ih1 := InverseGeneralMapping( isos1[ph1] ); 
        gh1 := ImageElm( ih1, e![2][2] ); 
        if not ( gt1 = gh1 ) then 
            Error( "gt1 <> gh1" );
        fi;
        loop := gt1;  
    else 
        loop := rays1[pt1] * e![2] * rays1[ph1]^(-1); 
    fi;
    iloop := ImageElm( rgh, loop ); 
    rims := ImageElementsOfRays( map ); 
    imo := ImagesOfObjects( map ); 
    pt2 := Position( obs2, imo[pt1] ); 
    ph2 := Position( obs2, imo[ph1] ); 
    pr2 := Position( obs2, imo[1] );
    if isto then 
        ir2 := InverseGeneralMapping( isos2[pr2] );
        it2 := ir2 * isos2[pt2]; 
        ih2 := ir2 * isos2[ph2]; 
        g2 := [ ImageElm( it2, iloop ), ImageElm( ih2, iloop ) ]; 
        return Arrow( rng, g2, imo[pt1], imo[ph1] ); 
    else 
        g2 := rims[pt1]^-1 * iloop * rims[ph1]; 
        return Arrow( rng, g2, imo[pt1], imo[ph1] ); 
    fi; 
end ); 

InstallOtherMethod( ImageElm, "for a map from homogeneous, discrete groupoid", 
    true, [ IsGroupoidHomomorphismFromHomogeneousDiscrete, 
            IsGroupoidElement ], 6,
function ( map, e )

    local p1, t2, g2, a;

    Info( InfoGroupoids, 3, 
          "this is the third ImageElm method in gpdhom.gi" ); 
    if not ( e in Source(map) ) then 
        Error( "the element e is not in the source of mapping map" ); 
    fi; 
    p1 := Position( Source( map )!.objects, e![3] ); 
    t2 := ImagesOfObjects( map )[ p1 ]; 
    g2 := ImageElm( ObjectHomomorphisms( map )[ p1 ], e![2] ); 
    a := ArrowNC( e![1], true, g2, t2, t2 );
    return a;
end ); 

InstallOtherMethod( ImageElm, "for a groupoid mapping", true, 
    [ IsGroupoidHomomorphism, IsGroupoidElement ], 0,
function ( map, e )

    local src, rng, pe, mape, part;

    Info( InfoGroupoids, 3, 
          "this is the fourth ImageElm method in gpdhom.gi" ); 
    if not ( e in Source(map) ) then 
        Error( "the element e is not in the source of mapping map" ); 
    fi; 
    src := Source( map ); 
    rng := Range( map ); 
    pe := Position( Pieces(src), PieceOfObject( src, e![3] ) ); 
    if ( HasIsSinglePiece( rng ) and IsSinglePiece( rng ) ) then 
        mape := MappingToSinglePieceMaps( map )[pe]; 
    else 
        part := PartitionOfSource( map ); 
        if ( part = fail ) then 
            Error( "map does not have PartitionOfSource" ); 
        fi; 
        pe := Position( part, [ pe ] ); 
        mape := PiecesOfMapping( map )[ pe ]; 
    fi;
    return ImageElm( mape, e ); 
end );

InstallOtherMethod( ImagesRepresentative, "for a groupoid homomorphism", true, 
    [ IsGroupoidHomomorphism, IsGroupoidElement ], 0, 
function( map, e ) 
    Info( InfoGroupoids, 3, "ImagesRepresentative at gpdhom.gi line 1258" ); 
    return ImageElm( map, e ); 
end );

############################################################################
##
#M  TestAllProductsUnderGroupoidHomomorphism( <hom> )
##
##  added 12/01/11 to check all z=x*y in costar(o)*star(o) -> iz=ix*iy 
##
InstallMethod( TestAllProductsUnderGroupoidHomomorphism,
    "generic method for a groupoid homomorphism", true,
    [ IsGroupoidHomomorphism ], 0,
function( hom )

    local t, src, obs, o, st, cst, x, ix, y, iy, z, iz; 

    t := 0; 
    src := Source( hom ); 
    obs := src!.objects; 
    for o in obs do 
        st := ObjectStar( src, o ); 
        cst := ObjectCostar( src, o ); 
        for x in cst do 
            ix := ImageElm( hom, x ); 
            for y in st do 
                iy := ImageElm( hom, y );
                z := x*y; 
                t := t+1; 
                iz := ImageElm( hom, z ); 
                if not ( iz = ix*iy ) then 
                    Print( " x, y, z = ", [x,y,z], 
                           "ix,iy,iz = ", [ix,iy,iz], "\n" ); 
                    return false; 
                fi;
            od;
        od;
    od;
    Print( "#I ", t, " products tested\n" ); 
    return true; 
end ); 

############################################################################
##
#M  IsomorphismPermGroupoid
#M  IsomorphismPcGroupoid
#M  RegularActionHomomorphismGroupoids
##
InstallMethod( IsomorphismPermGroupoid, "for a connected groupoid", true,
    [ IsGroupoid and IsSinglePiece ], 0,
function( g1 )

    local obs, gp1, iso, gp2, g2, par1, isopar, par2, ray1, ray2, 
          gen1, gen2, isog;

    obs := g1!.objects;
    gp1 := g1!.magma;
    if ( HasIsDirectProductWithCompleteDigraphDomain( g1 ) 
         and IsDirectProductWithCompleteDigraphDomain( g1 ) ) then 
        iso := IsomorphismPermGroup( gp1 );
        gp2 := Image( iso ); 
        g2 := Groupoid( gp2, obs ); 
    else 
        par1 := LargerDirectProductGroupoid( g1 ); 
        isopar := IsomorphismPermGroupoid( par1 ); 
        iso := RootGroupHomomorphism( isopar ); 
        par2 := Image( isopar ); 
        gp2 := Image( iso, gp1 ); 
        ray1 := RayArrowsOfGroupoid( g1 );  
        ray2 := List( ray1, g -> ImageElm( iso, g![2] ) ); 
        g2 := SubgroupoidWithRays( par2, gp2, ray2 ); 
    fi; 
    gen1 := GeneratorsOfGroupoid( g1 ); 
    gen2 := List( gen1, 
                g -> Arrow( g2, ImageElm(iso,g![2]), g![3], g![4] ) ); 
    isog := GroupoidHomomorphismFromSinglePieceNC( g1, g2, gen1, gen2 );
    return isog;
end );

InstallMethod( IsomorphismPermGroupoid, "generic method for a groupoid", 
    true, [ IsGroupoid ], 0,
function( g1 )

    local isos, comp1, nc1, i, g2, iso;

    comp1 := Pieces( g1 );
    nc1 := Length( comp1 );
    isos := ListWithIdenticalEntries( nc1, 0 );
    for i in [1..nc1] do 
        isos[i] := IsomorphismPermGroupoid( comp1[i] );
    od;
    g2 := UnionOfPieces( List( isos, Image ) );
    iso := HomomorphismByUnion( g1, g2, isos );
    return iso;
end );

InstallMethod( IsomorphismPcGroupoid, "for a connected groupoid", true,
    [ IsGroupoid and IsSinglePiece ], 0,
function( g1 )

    local obs, gp1, iso, gp2, g2, par1, isopar, par2, ray1, ray2, 
          gen1, gen2, isog;

    obs := g1!.objects;
    gp1 := g1!.magma;
    if ( HasIsDirectProductWithCompleteDigraphDomain( g1 ) 
         and IsDirectProductWithCompleteDigraphDomain( g1 ) ) then 
        iso := IsomorphismPcGroup( gp1 ); 
        if ( iso = fail ) then 
            Info( InfoGroupoids, 1, "IsomorphismPcGroup fails" ); 
            return fail;
        fi;
        gp2 := Image( iso ); 
        g2 := Groupoid( gp2, obs ); 
    else 
        par1 := LargerDirectProductGroupoid( g1 ); 
        isopar := IsomorphismPcGroupoid( par1 ); 
        if ( isopar = fail ) then 
            Info( InfoGroupoids, 1, "IsomorphismPcGroup fails with parent" ); 
            return fail;
        fi;
        iso := RootGroupHomomorphism( isopar ); 
        par2 := Image( isopar ); 
        gp2 := Image( iso, gp1 ); 
        ray1 := RayArrowsOfGroupoid( g1 );  
        ray2 := List( ray1, g -> ImageElm( iso, g![2] ) ); 
        g2 := SubgroupoidWithRays( par2, gp2, ray2 ); 
    fi; 
    gen1 := GeneratorsOfGroupoid( g1 ); 
    gen2 := List( gen1, 
                g -> Arrow( g2, ImageElm(iso,g![2]), g![3], g![4] ) ); 
    if not ( Length( gen1 ) = Length( gen2 ) ) then 
        Error("generating sets have different lengths");
    fi;
    isog := GroupoidHomomorphismFromSinglePieceNC( g1, g2, gen1, gen2 );
    return isog;
end );

InstallMethod( IsomorphismPcGroupoid, "generic method for a groupoid", 
    true, [ IsGroupoid ], 0,
function( g1 )

    local isos, comp1, nc1, i, g2, iso;

    comp1 := Pieces( g1 );
    nc1 := Length( comp1 );
    isos := ListWithIdenticalEntries( nc1, 0 );
    for i in [1..nc1] do 
        isos[i] := IsomorphismPcGroupoid( comp1[i] );
    od;
    g2 := UnionOfPieces( List( isos, Image ) );
    iso := HomomorphismByUnion( g1, g2, isos );
    return iso;
end );

InstallMethod( RegularActionHomomorphismGroupoid, "for a connected groupoid", 
    true, [ IsGroupoid and IsSinglePiece ], 0,
function( g1 )

    local obs, gp1, iso, gp2, g2, par1, isopar, par2, ray1, ray2, 
          gen1, gen2, isog;

    obs := g1!.objects;
    gp1 := g1!.magma;
    if ( HasIsDirectProductWithCompleteDigraphDomain( g1 ) 
         and IsDirectProductWithCompleteDigraphDomain( g1 ) ) then 
        iso := RegularActionHomomorphism( gp1 );
        gp2 := Image( iso ); 
        g2 := Groupoid( gp2, obs ); 
    else 
        par1 := LargerDirectProductGroupoid( g1 ); 
        isopar := RegularActionHomomorphismGroupoid( par1 ); 
        iso := RootGroupHomomorphism( isopar ); 
        par2 := Image( isopar ); 
        gp2 := Image( iso, gp1 ); 
        ray1 := RayArrowsOfGroupoid( g1 );  
        ray2 := List( ray1, g -> ImageElm( iso, g![2] ) ); 
        g2 := SubgroupoidWithRays( par2, gp2, ray2 ); 
    fi; 
    gen1 := GeneratorsOfGroupoid( g1 ); 
    gen2 := List( gen1, 
                g -> Arrow( g2, ImageElm(iso,g![2]), g![3], g![4] ) ); 
    isog := GroupoidHomomorphismFromSinglePieceNC( g1, g2, gen1, gen2 );
    return isog;
end );

InstallMethod( RegularActionHomomorphismGroupoid, 
    "generic method for a groupoid", true, [ IsGroupoid ], 0,
function( g1 )

    local isos, comp1, nc1, i, g2, iso;

    comp1 := Pieces( g1 );
    nc1 := Length( comp1 );
    isos := ListWithIdenticalEntries( nc1, 0 );
    for i in [1..nc1] do 
        isos[i] := RegularActionHomomorphismGroupoid( comp1[i] );
    od;
    g2 := UnionOfPieces( List( isos, Image ) );
    iso := HomomorphismByUnion( g1, g2, isos );
    return iso;
end );

#############################################################################
##
#M  IsomorphismNewObjects
##
InstallMethod( IsomorphismNewObjects, "for a single piece groupoid", true,
    [ IsGroupoid, IsHomogeneousList ], 0,
function( gpd1, ob2 )

    local iso, ob1, nobs, gp, gpd2, gens1, gens2, ngens, i, g, pt, ph, 
          gpgens, id, homs, pgpd, rays, rgp, piso, pgpd2;

    ob1 := ObjectList( gpd1 ); 
    nobs := Length( ob1 );
    if not ( Length(ob2) = nobs ) then
        Error( "object sets have different lengths" );
    fi;
    gp := gpd1!.magma;
    if IsSinglePiece( gpd1 ) then 
        gens1 := GeneratorsOfGroupoid( gpd1 ); 
        ngens := Length( gens1 );
        gens2 := [1..ngens]; 
        if IsDirectProductWithCompleteDigraphDomain( gpd1 ) then 
            gpd2 := SinglePieceGroupoidNC( gp, ShallowCopy( Set( ob2 ) ) );
            for i in [1..ngens] do 
                g := gens1[i]; 
                pt := Position( ob1, g![3] ); 
                ph := Position( ob1, g![4] ); 
                gens2[i] := Arrow( gpd2, g![2], ob2[pt], ob2[ph] );
            od; 
        else 
            pgpd := Parent( gpd1 ); 
            rays := RaysOfGroupoid( gpd1 ); 
            rgp := RootGroup( gpd1 ); 
            piso := IsomorphismNewObjects( pgpd, ob2 ); 
            pgpd2 := Range( piso );
            gpd2 := SubgroupoidWithRays( pgpd2, rgp, rays ); 
            gens2 := List( gens1, g -> ImageElm( piso, g ) ); 
        fi; 
        iso := GroupoidHomomorphismFromSinglePiece( gpd1, gpd2, gens1, gens2 ); 
    elif IsHomogeneousDiscreteGroupoid( gpd1 ) then 
        gpd2 := HomogeneousDiscreteGroupoid( gp, ob2 ); 
        gpgens := GeneratorsOfGroup( gp ); 
        id := GroupHomomorphismByImages( gp, gp, gpgens, gpgens );
        homs := ListWithIdenticalEntries( nobs, id ); 
        iso := GroupoidHomomorphismFromHomogeneousDiscrete( 
                   gpd1, gpd2, homs, ob2 ); 
    elif HasPieces( gpd1 ) then 
        Error( "apply IsomorphismNewObjects to individual pieces" );  
    else 
        return fail; 
    fi;
    return iso;
end );

############################################################################
##
#M  IsomorphismStandardGroupoid
##
InstallMethod( IsomorphismStandardGroupoid, "for a single piece groupoid", 
    true, [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd1, obs )

    local isdp, obs1, obs2, gp, gpd2, gens1, gens2;

    isdp := IsDirectProductWithCompleteDigraphDomain( gpd1 ); 
    if isdp then 
        if ( obs = gpd1!.objects ) then 
            return IdentityMapping( gpd1 ); 
        else 
            return IsomorphismNewObjects( gpd1, obs ); 
        fi; 
    fi;
    obs1 := gpd1!.objects; 
    obs2 := Set( obs ); 
    if not ( Length(obs1) = Length(obs2) ) then
        Error( "object sets have different lengths" );
    fi;
    gp := gpd1!.magma;
    gpd2 := SinglePieceGroupoidNC( gp, obs2 );
    gens1 := GeneratorsOfGroupoid( gpd1 ); 
    gens2 := GeneratorsOfGroupoid( gpd2 );
    return GroupoidHomomorphismFromSinglePiece( gpd1, gpd2, gens1, gens2 ); 
end );

InstallMethod( IsomorphismStandardGroupoid, "for a groupoid with pieces", 
    true, [ IsGroupoid, IsHomogeneousList ], 0,
function( gpd1, obs )

    local obs1, len1, obs2, k, pieces, nump, maps, range, i, p, lenp, m;

    obs1 := ObjectList( gpd1 ); 
    len1 := Length( obs1 );
    obs2 := Set( obs ); 
    if not ( Length(obs1) = Length(obs2) ) then
        Error( "object sets have different lengths" );
    fi; 
    k := 0; 
    pieces := Pieces( gpd1 ); 
    nump := Length( pieces ); 
    maps := ListWithIdenticalEntries( nump, 0 ); 
    range := ListWithIdenticalEntries( nump, 0 ); 
    for i in [1..nump] do 
        p := pieces[i]; 
        lenp := Length( p!.objects ); 
        m := IsomorphismStandardGroupoid( p, obs2{[k+1..k+lenp]} );
        maps[i] := m; 
        range[i] := Range( m );
        k := k + lenp;
    od;
    range := UnionOfPieces( range ); 
    return HomomorphismByUnion( gpd1, range, maps );
end );

InstallMethod( IsomorphismStandardGroupoid, "for a groupoid by isomorphisms", 
    true, [ IsGroupoidByIsomorphisms, IsHomogeneousList ], 0,
function( gpd1, obs )

    local isdp, obs1, obs2, gp, gpd2, gens1, gens2;

    isdp := IsDirectProductWithCompleteDigraphDomain( gpd1 ); 
    if isdp then 
        if ( obs = gpd1!.objects ) then 
            return IdentityMapping( gpd1 ); 
        else 
            return IsomorphismNewObjects( gpd1, obs ); 
        fi; 
    fi;
    obs1 := gpd1!.objects; 
    obs2 := Set( obs ); 
    if not ( Length(obs1) = Length(obs2) ) then
        Error( "object sets have different lengths" );
    fi;
    gp := gpd1!.magma;
    gpd2 := SinglePieceGroupoidNC( gp, obs2 );
    gens1 := GeneratorsOfGroupoid( gpd1 ); 
    gens2 := GeneratorsOfGroupoid( gpd2 );
    return GroupoidHomomorphismFromSinglePiece( gpd1, gpd2, gens1, gens2 ); 
end );

#############################################################################
##
#M  IsomorphismGroupoids
##
InstallMethod( IsomorphismGroupoids, "for two groupoids", true, 
    [ IsGroupoid, IsGroupoid ], 0,
function( A, B )

    local PA, PB, n, try, used, isos, i, p, found, j, k, q, iso; 

    if not ( HasPieces( A ) or HasPieces( B ) ) then 
        Error( "unexpected case in IsomorphismGroupoids" ); 
    fi; 
    if not ( HasPieces( A ) and HasPieces( B ) ) then 
        return fail; 
    fi; 
    PA := Pieces( A ); 
    PB := Pieces( B ); 
    n := Length( PA ); 
    if not ( Length( PB ) = n ) then 
        return fail; 
    fi; 
    try := [1..n]; 
    used := [ ]; 
    isos := [1..n]; 

    for i in [1..n] do
        p := PA[i]; 
        found := false; 
        try := Difference( try, used ); 
        j := 0; 
        while ( ( not found ) and ( j < Length( try ) ) ) do 
            j := j+1; 
            k := try[j];
            q := PB[k]; 
            iso := IsomorphismGroupoids( p, q );
            if not ( iso = fail ) then 
                found := true; 
                isos[i] := iso; 
                Add( used, k ); 
            fi; 
        od; 
        if ( not found ) then 
            return fail; 
        fi; 
    od; 
    return HomomorphismByUnion( A, B, isos );
end );

InstallMethod( IsomorphismGroupoids, "for two single piece groupoids", 
    true, [ IsGroupoid and IsSinglePiece, IsGroupoid and IsSinglePiece ], 0,
function( gpd1, gpd2 )

    local obs1, obs2, gp1, gp2, giso, gen1, len1, im2, i, a, g, u, v, iso, inv; 

    obs1 := ObjectList( gpd1 ); 
    obs2 := ObjectList( gpd2 ); 
    if not ( Length( obs1 ) = Length( obs2 ) ) then 
        return fail; 
    fi;
    gp1 := gpd1!.magma; 
    gp2 := gpd2!.magma; 
    giso := IsomorphismGroups( gp1, gp2 ); 
    if ( giso = fail ) then 
        return fail; 
    fi; 
    gen1 := GeneratorsOfGroupoid( gpd1 ); 
    len1 := Length( gen1 );
    im2 := ListWithIdenticalEntries( len1, 0 ); 
    for i in [1..len1] do 
        a := gen1[i]; 
        g := ImageElm( giso, a![2] ); 
        u := obs2[ Position( obs1, a![3] ) ]; 
        v := obs2[ Position( obs1, a![4] ) ]; 
        im2[i] := ArrowNC( gpd2, true, g, u, v ); 
    od;
    iso := GroupoidHomomorphismFromSinglePiece( gpd1, gpd2, gen1, im2 ); 
    inv := GroupoidHomomorphismFromSinglePiece( gpd2, gpd1, im2, gen1 );
    SetInverseGeneralMapping( iso, inv ); 
    return iso; 
end );

## ======================================================================== ##
##                     Homogeneous groupoid homomorphisms                   ##
## ======================================================================== ##

############################################################################
##
#M  GroupoidHomomorphismFromHomogeneousDiscreteNC 
#M  GroupoidHomomorphismFromHomogeneousDiscrete 
##
InstallMethod( GroupoidHomomorphismFromHomogeneousDiscreteNC,
    "method for a mapping from a homogeneous, discrete groupoid", true,
    [ IsHomogeneousDiscreteGroupoid, IsGroupoid, IsHomogeneousList, 
      IsHomogeneousList ], 0,
function( src, rng, homs, oims )

    local obs, map, ok, oki, oks, mgi, p;

    obs := ObjectList( src ); 
    map := rec(); 
    ObjectifyWithAttributes( map, GroupoidHomomorphismDiscreteType, 
        Source, src, 
        Range, rng, 
        ObjectHomomorphisms, homs, 
        ImagesOfObjects, oims, 
        IsGeneralMappingWithObjects, true, 
        IsGroupWithObjectsHomomorphism, true, 
        RespectsMultiplication, true ); 
    oki := IsInjectiveOnObjects( map ); 
    oks := IsSurjectiveOnObjects( map ); 
    SetIsGeneralMappingFromSinglePiece( map, false ); 
    mgi := MappingGeneratorsImages( map ); 
    if ( src = rng ) then 
        SetIsEndoGeneralMapping( map, true ); 
        SetIsEndomorphismWithObjects( map, true ); 
        p := ObjectTransformationOfGroupoidHomomorphism( map ); 
        ok := IsAutomorphismWithObjects( map ); 
    fi; 
    return map; 
end );

InstallMethod( GroupoidHomomorphismFromHomogeneousDiscrete,
    "method for a mapping from a homogeneous, discrete groupoid", true,
    [ IsHomogeneousDiscreteGroupoid, IsGroupoid, IsHomogeneousList,
      IsHomogeneousList ], 0,
function( src, rng, homs, oims ) 

    local gps, gpr, obs, obr; 

    Info( InfoGroupoids,3, "method for GpdHomFromHomDisc to discrete gpd" );
    gps := src!.magma; 
    gpr := rng!.magma; 
    if not ForAll( homs, 
        h -> ( Source(h) = gps ) and IsSubgroup( gpr, Range(h) ) ) then 
        Error( "homs not a list of maps RootGroup(src) -> RootGroup(rng) " ); 
    fi; 
    obs := src!.objects; 
    obr := rng!.objects; 
    if not ( Length(oims) = Length(obs) ) then 
        Error( "<oims> has incorrect length" ); 
    fi; 
    if not ForAll( oims, o -> o in obr ) then 
        Error( "object images not all objects in <rng>" ); 
    fi; 
    return GroupoidHomomorphismFromHomogeneousDiscreteNC(src,rng,homs,oims); 
end );
