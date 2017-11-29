############################################################################## 
##
#W  gpdhom.gi              GAP4 package `groupoids'              Chris Wensley
#W                                                                & Emma Moore
#Y  Copyright (C) 2000-2017, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

#############################################################################
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

#############################################################################
##
#M  IsGroupoidHomomorphismFromHomogeneousDiscrete( <hom> )
##
InstallImmediateMethod( IsGroupoidHomomorphismFromHomogeneousDiscrete, 
    "for a groupoid hom", IsGroupoidHomomorphism and 
                          IsGeneralMappingFromHomogeneousDiscrete, 0,
function( map ) 
    return true; 
end ); 

#############################################################################
##
#M  IsGeneratorsOfMagmaWithInverses( <homlist> )
##
InstallMethod( IsGeneratorsOfMagmaWithInverses, "for a list of groupoid maps", 
    true, [ IsGeneralMappingWithObjectsCollection ], 0,
function( homlist ) 
    Print( "#I  using IsGeneratorsOfMagmaWithInverses in gpdhom.gi\n" ); 
    return ForAll( homlist, 
        m -> ( ( Source(m) = Range(m) ) 
               and IsEndomorphismWithObjects(m) 
               and IsInjectiveOnObjects(m) 
               and IsSurjectiveOnObjects(m) ) ); 
end );
 
#############################################################################
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

#############################################################################
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

    local nargs, id, rays, ob1, ob2, i, g, pt, ph, gens1, gens2, ngens, nobs, 
          images, e, a; 

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
            pt := Position( ob1, g![2] ); 
            ph := Position( ob1, g![3] ); 
            gens2[i] := Arrow( arg[2], Image(arg[3],g![1]), ob2[pt], ob2[ph] );
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
            pt := arg[4][ Position( ob1, g![2] ) ]; 
            ph := arg[4][ Position( ob1, g![3] ) ]; 
            if ( pt = ph ) then 
                e := ImageElm( arg[3], g![1] ); 
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

#############################################################################
##
#M  InclusionMappingGroupoids
##
InstallMethod( InclusionMappingGroupoids, "for sgpd of single piece groupoid", 
    true, [ IsGroupoid and IsSinglePiece, IsGroupoid ], 0,
function( gpd, sgpd ) 

    local sobs, o1, c1, mappings, comps, m, gens, mor;

    if not IsSubgroupoid( gpd, sgpd ) then
        Error( "arg[2] is not a subgroupoid of arg[1]" );
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
function( gpd, sgpd )
    if not IsSubdomainWithObjects( gpd, sgpd ) then
        Error( "arg[2] is not a submagma of arg[1]" );
    fi;
    if not IsSinglePiece( sgpd ) then
        Error( "not yet implemented when arg[2] is not connected" );
    fi;
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

#############################################################################
##
#M  RestrictedMappingGroupoids
##
InstallMethod( RestrictedMappingGroupoids, "for a groupoid mapping", true,
    [ IsGeneralMappingWithObjects, IsGroupoid and IsSinglePiece ], 0,
function( mor, U )

    local smor, rmor, genU, imres, V, res;

    Info( InfoGroupoids, 3, "RestrictedMapping from a single piece" ); 
    smor := Source( mor );
    rmor := Range( mor );
    if not ( IsSubdomainWithObjects( smor, U ) ) then
        Error( "U not a submagma of Source(mor)" );
    fi; 
    genU := GeneratorsOfGroupoid( U ); 
    imres := List( genU, g -> ImageElm( mor, g ) ); 
    V := SinglePieceSubgroupoidByGenerators( Range(mor), imres );
    res := GroupoidHomomorphismFromSinglePiece( U, V, genU, imres ); 
    SetIsSurjective( mor, true ); 
    if ( HasIsInjective( mor ) and IsInjective( mor ) ) then
        SetIsInjective( res, true );
    fi;
    return res; 
end );

InstallMethod( RestrictedMappingGroupoids, "for a groupoid mapping", true,
    [ IsGroupWithObjectsHomomorphism, 
      IsGroupoid and IsHomogeneousDiscreteGroupoid ], 0,
function( mor, U )

    local smor, rmor, pieces, nobs, imobs, autos, 
          i, P, genP, imP, obi, imobi, ogi, imgpi, impieces, imU;

    Info( InfoGroupoids, 3, "RestrictedMapping from hom discrete" ); 
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
        obi := genP[1]![2]; 
        imP := List( genP, a -> ImageElm( mor, a ) ); 
        imobi := imP[1]![2];
        imobs[i] := imobi; 
        ogi := ObjectGroup( rmor, imobi ); 
        imgpi := Subgroup( ogi, List( imP, a -> a![1] ) ); 
        autos[i] := GroupHomomorphismByImages( ObjectGroup( P, obi ), imgpi, 
                        List( genP, g->g![1] ), List( imP, g->g![1] ) ); 
        Add( impieces, SubgroupoidByPieces( rmor, [ [imgpi,[imobi]] ] ) );
    od; 
    imU := SubgroupoidByPieces( rmor, impieces );
    return GroupoidHomomorphismFromHomogeneousDiscrete(U,imU,autos,imobs); 
end );

InstallMethod( RestrictedMappingGroupoids, "for a groupoid mapping", true,
    [ IsGeneralMappingWithObjects, IsGroupoid ], 0,
function( mor, src )

    local mrng, psrc, lsrc, rcomp, rrng, j, P, genP, imgenP, imP, 
          pos, hom, rng, res;

    Info( InfoGroupoids, 3, "RestrictedMapping from a union" ); 
    mrng := Range( mor );
    psrc := Pieces( src ); 
    lsrc := Length( psrc );
    rcomp := ListWithIdenticalEntries( lsrc, 0 ); 
    rrng := ListWithIdenticalEntries( lsrc, 0 );
    for j in [1..lsrc] do 
        P := psrc[j]; 
        genP := GeneratorsOfGroupoid( P ); 
        imgenP := List( genP, g -> ImageElm( mor, g ) ); 
        pos := PieceNrOfObject( mrng, imgenP[1]![2] ); 
        imP := SinglePieceSubgroupoidByGenerators( Pieces(mrng)[pos], imgenP );
        hom := GroupoidHomomorphism( P, imP, genP, imgenP ); 
        rcomp[j] := hom; 
        rrng[j] := Range( hom );
    od; 
    rng := Groupoid( rrng );
    res := HomomorphismByUnionNC( src, rng, rcomp ); 
    if ( HasIsInjective( mor ) and IsInjective( mor ) ) then
        SetIsInjective( res, true );
    fi;
    return res;
end );

#############################################################################
##
#M  RootGroupHomomorphism . . . . . . . for a groupoid hom from single piece 
##
InstallMethod( RootGroupHomomorphism, "for a groupoid hom from a single piece", 
    true, [ IsGroupoidHomomorphism and IsHomomorphismToSinglePiece ], 0,
function( mor )

    local gpd1, gpd2, ob2, imobs, roh, mgi, ray, gp2, im2, hom;

    Info( InfoGroupoids, 3, 
          "method for RootGroupHomomorphism in gpdhom.gi" ); 
    gpd2 := Range( mor ); 
    ob2 := gpd2!.objects; 
    imobs := ImagesOfObjects( mor ); 
    roh := RootGroupHomomorphism( mor ); 
    if ( imobs[1] = ob2[1] ) then  ## root maps to root 
        hom := roh; 
    elif ( HasIsDirectProductWithCompleteDigraph( gpd2 ) and 
           IsDirectProductWithCompleteDigraph( gpd2 ) ) then 
        hom := roh; 
    else 
        mgi := MappingGeneratorsImages( roh ); 
        ray := RayElementsOfGroupoid( gpd2 )[ Position( ob2, imobs[1] ) ]; 
        gp2 := ObjectGroup( gpd2, imobs[1] ); 
        im2 := List( mgi[2], g -> g^ray ); 
        hom := GroupHomomorphismByImages( Source( roh ), gp2, mgi[1], im2 );  
    fi; 
    return hom; 
end ); 

#############################################################################
##
#M  ObjectGroupHomomorphism . . . . .  . . . . . . . . . . for a groupoid hom 
##
InstallMethod( ObjectGroupHomomorphism, "for a groupoid hom and an object", 
    true, [ IsGroupoidHomomorphism, IsObject ], 0,
function( mor, obj ) 

    local src, imobs, pos, obg, gens, loops, imloops, imgens, img, hom; 

    src := Source( mor ); 
    if ( HasIsGeneralMappingFromSinglePiece( mor ) 
         and IsGeneralMappingFromSinglePiece( mor ) ) then 
        imobs := ImagesOfObjects( mor ); 
        pos := Position( src!.objects, obj );
        obg := ObjectGroup( src, obj );
        gens := GeneratorsOfGroup( obg ); 
        loops := List( gens, g -> Arrow( src, g, obj, obj ) ); 
        imloops := List( loops, a -> ImageElm( mor, a ) ); 
        imgens := List( imloops, a -> a![1] ); 
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

#############################################################################
##
#M  MappingPermObjectsImages . . . . . . for list of objects and their images 
#M  MappingTransObjectsImages . . . . .  for list of objects and their images 
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

#############################################################################
##
#M  ObjectTransformationOfGroupoidHomomorphism . . . . for a groupoid mapping 
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
#M  MappingGeneratorsImages 
##
InstallMethod( MappingGeneratorsImages, "for a mapping to a single piece", 
    true, [ IsGroupoidHomomorphism and IsHomomorphismToSinglePiece ], 0,
function ( hom ) 
    
    local maps; 

    maps := MappingToSinglePieceMaps( hom ); 
    return List( maps, m -> MappingGeneratorsImages(m) ); 
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
        g := a![1]; 
        o := a![2]; 
        pos := Position( obs, o ); 
        imo := oims[pos];         
        img := ImageElm( obhoms[pos], g ); 
        imgs[i] := ArrowNC( true, img, imo, imo ); 
    od;
    return [ gens, imgs ]; 
end );

#############################################################################
##
#M  Display
##
InstallMethod( Display, "for a mapping of connected groupoids", true,
    [ IsGroupoidHomomorphism and IsHomomorphismFromSinglePiece 
      and IsHomomorphismToSinglePiece ], 0,
function ( map ) 
    Print( " groupoid mapping: " ); 
    Print( "[ ", Source(map), " ] -> [ ", Range(map), " ]\n" ); 
    Print( "root homomorphism: ", 
           MappingGeneratorsImages( RootGroupHomomorphism(map) ), "\n" ); 
    Print( "images of objects: ", ImagesOfObjects( map ), "\n" ); 
    Print( "   images of rays: ", 
           List( RaysOfGroupoid( Source(map) ), r -> ImageElm(map,r) ), 
           "\n" ); 
end );

InstallMethod( Display, "for a mapping from a homogeneous discrete gpd", true, 
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep ], 0,
function ( map ) 

    local h; 

    Print( " homogeneous discrete groupoid mapping: " ); 
    Print( "[ ", Source(map), " ] -> [ ", Range(map), " ]\n" ); 
    Print( "images of objects: ", ImagesOfObjects( map ), "\n" ); 
    Print( "object homomorphisms:\n" ); 
    for h in ObjectHomomorphisms( map ) do 
        Print( h, "\n" ); 
    od; 
end );

#############################################################################
##
#M  GroupoidHomomorphismFromSinglePieceNC 
#M  GroupoidHomomorphismFromSinglePiece 
##
InstallMethod( GroupoidHomomorphismFromSinglePieceNC,
    "generic method for a mapping of connected groupoids", true,
    [ IsGroupoid and IsSinglePiece, IsGroupoid and IsSinglePiece, 
      IsHomogeneousList, IsHomogeneousList ], 0,
function( src, rng, gens, images )

    local obs, nobs, ngens, nggens, posr, imr, map, 
          gps, gpr, hgen, himg, hom, oims, rims, ok;

    obs := src!.objects; 
    nobs := Length( obs ); 
    ngens := Length( gens ); 
    nggens := ngens-nobs+1; 
    posr := nggens+1; 
    imr := images[1]![2];
    gps := src!.magma; 
    hgen := List( [1..nggens], i -> gens[i]![1] ); 
    himg := List( [1..nggens], i -> images[i]![1] ); 
    gpr := ObjectGroup( rng, imr );
    hom := GroupHomomorphismByImagesNC( gps, gpr, hgen, himg ); 
    oims := Concatenation( [imr], List( [posr..ngens], i -> images[i]![3] ) ); 
    rims := Concatenation( [ One( gpr ) ], 
                           List( [posr..ngens], i -> images[i]![1] ) );
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
    imr := images[1]![2]; 
    for i in [1..nggens] do 
        if not ( ( images[i]![2] = imr ) and ( images[i]![3] = imr ) ) then 
            Error( "images[i] not a loop at the root vertex" ); 
        fi; 
    od;
    for i in [posr+1..ngens] do 
        if not ( images[i]![2] = imr ) then 
            Error( "all ray images should have the same source" ); 
        fi; 
    od; 
    return GroupoidHomomorphismFromSinglePieceNC( src, rng, gens, images ); 
end ); 

#############################################################################
##
#M  GroupoidAutomorphismByObjectPermNC  
#M  GroupoidAutomorphismByObjectPerm 
##
InstallMethod( GroupoidAutomorphismByObjectPermNC , 
    "for a single piece groupoid and a permutation of objects", true, 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd, oims ) 

    local obs, gens, ngens, images, i, a, pt, ph, mor, L; 

    obs := gpd!.objects; 
    gens := GeneratorsOfGroupoid( gpd ); 
    ngens := Length( gens ); 
    images := [1..ngens]; 
    for i in [1..ngens] do 
        a := gens[i]; 
        pt := Position( obs, a![2] ); 
        ph := Position( obs, a![3] );
        images[i] := Arrow( gpd, a![1], oims[pt], oims[ph] ); 
    od; 
    mor := GroupoidHomomorphismFromSinglePiece( gpd, gpd, gens, images );
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    L := [1..Length(obs)]; 
    SortParallel( ShallowCopy( oims ), L );  
    SetOrder( mor, Order( PermList( L ) ) ); 
    SetIsGroupoidAutomorphismByObjectPerm( mor, true ); 
    return mor; 
end ); 

InstallMethod( GroupoidAutomorphismByObjectPermNC,  
    "for a hom discrete groupoid and a permutation of objects", true, 
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ], 0,
function( gpd, oims )

    local gpd1, gp, id, homs, mor, L; 

    gpd1 := Pieces( gpd )[1]; 
    gp := gpd1!.magma; 
    id := IdentityMapping( gp ); 
    homs := List( oims, o -> id ); 
    mor := GroupoidHomomorphismFromHomogeneousDiscreteNC
               ( gpd, gpd, homs, oims ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    L := [1..Length(oims)]; 
    SortParallel( ShallowCopy( oims ), L );  
    SetOrder( mor, Order( PermList( L ) ) );
    SetIsGroupoidAutomorphismByObjectPerm( mor, true ); 
    return mor; 
end ); 

InstallMethod( GroupoidAutomorphismByObjectPerm, 
    "for a groupoid and a permutation of objects", true, 
    [ IsGroupoid, IsHomogeneousList ], 0,
function( gpd, oims ) 

    local obs, pos; 

    obs := ObjectList( gpd ); 
    pos := PermList( List( oims, o -> Position( obs, o ) ) ); 
    if ( pos = fail ) then 
        Error( "object images not a permutation of the objects" ); 
    fi; 
    return GroupoidAutomorphismByObjectPermNC( gpd, oims ); 
end ); 

#############################################################################
##
#M  GroupoidAutomorphismByGroupAutoNC  
#M  GroupoidAutomorphismByGroupAuto 
##
InstallMethod( GroupoidAutomorphismByGroupAutoNC , 
    "for a groupoid and an automorphism of the root group", true, 
    [ IsGroupoid and IsSinglePiece, IsGroupHomomorphism and IsBijective ], 0,
function( gpd, hom ) 

    local gens, images, mor; 

    gens := GeneratorsOfGroupoid( gpd ); 
    images := List( gens, 
                    a -> Arrow( gpd, ImageElm( hom, a![1] ), a![2], a![3] ) ); 
    mor := GroupoidHomomorphismFromSinglePiece( gpd, gpd, gens, images ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    SetOrder( mor, Order( hom ) ); 
    SetIsGroupoidAutomorphismByGroupAuto( mor, true ); 
    return mor; 
end ); 

InstallMethod( GroupoidAutomorphismByGroupAuto, 
    "for a groupoid and an automorphism of the root group", true, 
    [ IsGroupoid and IsSinglePiece, IsGroupHomomorphism ], 0,
function( gpd, hom ) 

    local rgp; 

    rgp := gpd!.magma; 
    if not ( (Source(hom) = rgp) and (Range(hom) = rgp) ) then 
        Error( "hom not an endomorphism of the root group" ); 
    fi; 
    if not IsBijective( hom ) then 
        Error( "hom is not an automorphism" ); 
    fi;
    return GroupoidAutomorphismByGroupAutoNC( gpd, hom ); 
end ); 

#############################################################################
##
#M  GroupoidAutomorphismByGroupAutosNC  
#M  GroupoidAutomorphismByGroupAutos 
##
InstallMethod( GroupoidAutomorphismByGroupAutosNC , 
    "for homogeneous discrete groupoid and automorphism list", true, 
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ], 0,
function( gpd, homs ) 

    local obs, orders, m; 

    obs := ObjectList( gpd );
    m := GroupoidHomomorphismFromHomogeneousDiscreteNC( gpd, gpd, homs, obs ); 
    SetIsEndoGeneralMapping( m, true ); 
    SetIsInjectiveOnObjects( m, true ); 
    SetIsSurjectiveOnObjects( m, true ); 
    orders := List( homs, h -> Order( h ) ); 
    SetOrder( m, Lcm( orders ) ); 
    return m; 
end ); 

InstallMethod( GroupoidAutomorphismByGroupAutos, 
    "for homogeneous discrete groupoid and automorphism list", true, 
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ], 0,
function( gpd, homs )  

    local pieces, len, i, p, g, h; 

    pieces := Pieces( gpd ); 
    len := Length( pieces ); 
    if not ( len = Length( homs ) ) then 
        Error( "length of <homs> not equal to number of objects on <gpd>," ); 
    fi; 
    for i in [1..len] do 
        p := pieces[i];  
        g := p!.magma; 
        h := homs[i]; 
        if not ( (Source(h) = g) and (Range(h) = g) ) then 
            Error( "<h> not an endomorphism of group <g> at object <i>," ); 
        fi; 
        if not IsBijective( h ) then 
            Error( "<h> not an automorphism of group <g> at object <i>," ); 
        fi; 
    od; 
    return GroupoidAutomorphismByGroupAutosNC( gpd, homs ); 
end ); 

#############################################################################
##
#M  GroupoidAutomorphismByRayShiftsNC  
#M  GroupoidAutomorphismByRayShifts 
##
InstallMethod( GroupoidAutomorphismByRayShiftsNC , 
    "for a groupoid and a list of elements of the root group", true, 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd, shifts ) 

    local gens, ngens, nobs, images, i, k, a, mor; 

    gens := GeneratorsOfGroupoid( gpd ); 
    ngens := Length( gens ); 
    nobs := Length( gpd!.objects ); 
    images := ShallowCopy( gens ); 
    k := ngens - nobs;
    for i in [2..nobs] do 
        a := gens[i+k]; 
        images[i+k] := Arrow( gpd, a![1]*shifts[i], a![2], a![3] ); 
    od; 
    mor := GroupoidHomomorphismFromSinglePiece( gpd, gpd, gens, images ); 
    SetOrder( mor, Lcm( List( shifts, i -> Order(i) ) ) ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    SetIsGroupoidAutomorphismByRayShifts( mor, true ); 
    return mor;
end ); 

InstallMethod( GroupoidAutomorphismByRayShifts, 
    "for a groupoid and a list of elements of the root group", true, 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd, shifts ) 

    local rgp, rays, nobs, conj; 

    rgp := gpd!.magma; 
    rays := gpd!.rays; 
    nobs := Length( gpd!.objects );
    if not ForAll( shifts, s -> s in rgp ) then  
        Error( "ray shifts not all in the relevant homsets" ); 
    fi; 
    if not ( shifts[1] = One( rgp ) ) then 
        Error( "the first ray shift is not the identity" ); 
    fi; 
    return GroupoidAutomorphismByRayShiftsNC( gpd, shifts ); 
end ); 

#############################################################################
##
#M  GroupoidInnerAutomorphism 
##
InstallMethod( GroupoidInnerAutomorphism, 
    "for a groupoid and an element", true, 
    [ IsGroupoid, IsGroupoidElement ], 0,
function( gpd, e ) 
    Error( "not yet implemented for unions of groupoids" );
end );

InstallMethod( GroupoidInnerAutomorphism, 
    "for a groupoid and an element", true, 
    [ IsGroupoid and IsSinglePieceDomain, IsGroupoidElement ], 0,
function( gpd, e ) 

    local gens, images; 

    Info( InfoGroupoids, 3, "GroupoidInnerAutomorphism from single piece" );  
    gens := GeneratorsOfGroupoid( gpd ); 
    images := List( gens, g -> g^e ); 
    return GroupoidHomomorphism( gpd, gpd, gens, images );
end );

InstallMethod( GroupoidInnerAutomorphism, 
    "for a groupoid and an element", true, 
    [ IsGroupoid and IsDiscreteDomainWithObjects, IsGroupoidElement ], 0,
function( gpd, e ) 

    local obs, nobs, pos, id, auts; 

    Info( InfoGroupoids, 3, "GroupoidInnerAutomorphism from discrete domain" );  
    obs := gpd!.objects; 
    nobs := Length( obs );
    pos := Position( obs, e![2] ); 
    id := IdentityMapping( gpd!.magma );
    auts := List( [1..nobs], i -> id ); 
    auts[pos] := InnerAutomorphism( gpd!.magma, e![1] ); 
    return GroupoidAutomorphismByGroupAutosNC( gpd, auts ); 
end );

InstallOtherMethod( GroupoidInnerAutomorphism, 
    "for a groupoid, a subgroupoid, and an element", true, 
    [ IsGroupoid and IsSinglePieceDomain, IsGroupoid, IsGroupoidElement ], 0,
function( gpd, sub, e ) 

    local gens, images, inn, res; 

    Info( InfoGroupoids, 3, "GroupoidInnerAutomorphism for a subgroupoid" ); 
    if not IsSubgroupoid( gpd, sub ) then 
        Error( "sub is not a subgroupoid of gpd" ); 
    fi; 
    gens := GeneratorsOfGroupoid( gpd ); 
    images := List( gens, g -> g^e ); 
    inn := GroupoidHomomorphism( gpd, gpd, gens, images ); 
    res := RestrictedMappingGroupoids( inn, sub ); 
    return res;
end );

#############################################################################
##
#M  NiceObjectAutoGroupGroupoid( <gpd>, <aut> ) . . create a nice monomorphism 
##
InstallMethod( NiceObjectAutoGroupGroupoid, "for a single piece groupoid", 
    true, [ IsGroupoid and IsSinglePieceDomain, IsAutomorphismGroup ], 0,
function( gpd, aut ) 

    local genaut, rgp, genrgp, nrgp, agp, genagp, nagp, iso1, im1, iso2, 
          pagp, iso, obs, n, L, symm, gensymm, nsymm, ngp, genngp, nngp, 
          ninfo, nemb, gens1, i, pi, imi, j, gens2, k, krgp, ikrgp, 
          pasy, esymm, epagp, genpasy, gens12, actgp, ok, action, sdp, 
          sinfo, siso, ssdp, epasy, engp, gennorm, norm, a, c, c1, c2, c12, 
          agens1, agens2, agens3, agens, nat, autgen, eno, gim1, gim2, gim3, 
          niceob, nicemap; 

    genaut := GeneratorsOfGroup( aut );
    ## first: automorphisms of the root group 
    rgp := gpd!.magma; 
    genrgp := GeneratorsOfGroup( rgp );
    nrgp := Length( genrgp ); 
    agp := AutomorphismGroup( rgp ); 
    genagp := SmallGeneratingSet( agp ); 
    nagp := Length( genagp ); 
    iso1 := IsomorphismPermGroup( agp ); 
    im1 := Image( iso1 );
    iso2 := SmallerDegreePermutationRepresentation( im1 );
    pagp := Image( iso2 ); 
    iso := iso1*iso2; 
    agens1 := List( genagp, a -> ImageElm( iso, a ) ); 
    ## second: permutations of the objects 
    obs := gpd!.objects; 
    n := Length( obs );
    if ( n = 1 ) then 
        return pagp; 
    elif ( n = 2 ) then 
        gensymm := [ (1,2) ]; 
    else 
        L := [2..n]; 
        Append( L, [1] ); 
        gensymm := [ PermList(L), (1,2) ];  #? replace (1,2) with (n-1,n) ?? 
    fi;
    symm := Group( gensymm ); 
    nsymm := Length( gensymm ); 
    ## third: ray products 
    ngp := DirectProduct( ListWithIdenticalEntries( n, rgp ) ); 
    genngp := GeneratorsOfGroup( ngp ); 
    nngp := Length( genngp );
    ninfo := DirectProductInfo( ngp ); 
    ## force construction of the n embeddings 
    for i in [1..n] do 
        k := Embedding( ngp, i ); 
    od; 
    nemb := ninfo!.embeddings; 
    # action of agp on ngp 
    gens1 := [1..nagp]; 
    for i in [1..nagp] do 
        k := genagp[i]; 
        krgp := List( genrgp, r -> ImageElm( k, r ) ); 
        ikrgp := [ ]; 
        for j in [1..n] do 
            Append( ikrgp, List( krgp, x -> ImageElm( nemb[j], x ) ) ); 
        od; 
        gens1[i] := GroupHomomorphismByImages( ngp, ngp, genngp, ikrgp ); 
    od; 
    ## action of symm on ngp = rgp^n 
    gens2 := [1..nsymm]; 
    for i in [1..nsymm] do 
        pi := gensymm[i]^-1; 
        gens2[i] := GroupHomomorphismByImages( ngp, ngp, genngp,  
            List( [1..nngp], j -> genngp[ RemInt( j-1, nrgp ) + 1  
            + nrgp * (( QuoInt( j-1, nrgp ) + 1 )^pi - 1 ) ] ) ); 
    od; 
    pasy := DirectProduct( pagp, symm ); 
    epagp := Embedding( pasy, 1 ); 
    agens1 := List( agens1, g -> ImageElm( epagp, g ) ); 
    esymm := Embedding( pasy, 2 ); 
    agens2 := List( gensymm, g -> ImageElm( esymm, g ) ); 
    ## genpasy := GeneratorsOfGroup( pasy ); 
    genpasy := Concatenation( agens1, agens2 );
    #  construct the semidirect product 
    gens12 := Concatenation( gens1, gens2 ); 
    actgp := Group( gens12 ); 
    ok := IsGroupOfAutomorphisms( actgp ); 
    action := GroupHomomorphismByImages( pasy, actgp, genpasy, gens12 ); 
    sdp := SemidirectProduct( pasy, action, ngp ); 
    Info( InfoGroupoids, 2, "sdp has ", 
          Length(GeneratorsOfGroup(sdp)), " gens." ); 
    sinfo := SemidirectProductInfo( sdp ); 
    siso := SmallerDegreePermutationRepresentation( sdp ); 
    ssdp := Image( siso ); 
    epasy := Embedding( sdp, 1 ) * siso; 
    agens1 := List( agens1, g -> ImageElm( epasy, g ) );  
    agens2 := List( agens2, g -> ImageElm( epasy, g ) ); 
    engp := Embedding( sdp, 2 ) * siso; 
    #  why [3..nngp]?  no doubt because Sn has two generators? 
    agens3 := List( genngp{[3..nngp]}, g -> ImageElm( engp, g ) ); 
    #  construct the normal subgroup 
    gennorm := [1..nrgp ]; 
    for i in [1..nrgp] do 
        c := genrgp[i]; 
        a := GroupHomomorphismByImages( rgp, rgp, genrgp, List(genrgp,x->x^c) );
        c1 := ImageElm( epasy, ImageElm( epagp, ImageElm( iso, a ) ) );
        c := c^-1; 
        c2 := ImageElm( engp, 
            Product( List( [1..n], j->ImageElm( nemb[j], c ) ) ) ); 
        gennorm[i] := c1 * c2; 
    od; 
    norm := Subgroup( ssdp, gennorm ); 
    ok := IsNormal( ssdp, norm ); 
    if not ok then 
        Error( "norm should be a normal subgroup of ssdp" ); 
    fi; 
    nat := NaturalHomomorphismByNormalSubgroup( ssdp, norm ); 
    niceob := Image( nat ); 
    eno := ListWithIdenticalEntries( n+2, 0 );
    eno[1] := iso * epagp * epasy * nat;   ## agp->pagp->pasy->ssdp->niceob
    eno[2] := esymm * epasy * nat;         ##      symm->pasy->ssdp->niceob
    for i in [1..n] do 
        gim1 := List( genrgp, g -> ImageElm( nemb[i], g ) ); 
        gim2 := List( gim1, g -> ImageElm( engp, g ) ); 
        gim3 := List( gim2, g -> ImageElm( nat, g ) ); 
        eno[i+2] := GroupHomomorphismByImages( rgp, niceob, genrgp, gim3 ); 
    od; 
    autgen := Concatenation( agens1, agens2, agens3 ); 
    agens := List( autgen, g -> ImageElm( nat, g ) ); 
    return [ niceob, eno ]; 
end );

InstallMethod( NiceObjectAutoGroupGroupoid, "for a hom discrete groupoid", 
    true, [ IsHomogeneousDiscreteGroupoid, IsAutomorphismGroup ], 0,
function( gpd, aut ) 

    local pieces, obs, m, p1, g1, geng1, ng1, ag1, genag1, nag, 
          iso1, ag2, genag2, mag2, genmag2, nmag2, minfo, i, k, memb, 
          K, L, symm, gensymm, imact, pi, actgp, ok, action, sdp, sinfo, 
          siso, ssdp, esymm, egensymm, emag2, egenmag2, agens, 
          im1, im2, im3, eno;  

    pieces := Pieces( gpd ); 
    obs := ObjectList( gpd ); 
    m := Length( obs );
    ## first: automorphisms of the object groups 
    p1 := pieces[1]; 
    g1 := p1!.magma; 
    geng1 := GeneratorsOfGroup( g1 );
    ng1 := Length( geng1 ); 
    ag1 := AutomorphismGroup( g1 ); 
    genag1 := SmallGeneratingSet( ag1 ); 
    nag := Length( genag1 ); 
    iso1 := IsomorphismPermGroup( ag1 ); 
    genag2 := List( genag1, g -> ImageElm( iso1, g ) ); 
    ag2 := Group( genag2 ); 
    mag2 := DirectProduct( ListWithIdenticalEntries( m, ag2 ) ); 
    genmag2 := GeneratorsOfGroup( mag2 ); 
    nmag2 := Length( genmag2 );
    minfo := DirectProductInfo( mag2 ); 
    ## force construction of the m embeddings 
    for i in [1..m] do 
        k := Embedding( mag2, i ); 
    od; 
    memb := minfo!.embeddings; 
    ## second: permutations of the objects 
    if ( m = 1 ) then 
        Error( "only one object, so no permutations" ); 
    elif ( m = 2 ) then 
        K := [1]; 
        gensymm := [ (1,2) ]; 
    else 
        K := [1,2]; 
        L := [2..m]; 
        Append( L, [1] ); 
        gensymm := [ PermList(L), (1,2) ];  #? replace (1,2) with (m-1,m) ?? 
    fi;
    symm := Group( gensymm ); 
    ## action of symm on mag2 = ag2^m 
    imact := ShallowCopy( K ); 
    for i in K do 
        pi := gensymm[i]^-1; 
        imact[i] := GroupHomomorphismByImages( mag2, mag2, genmag2,  
            List( [1..nmag2], j -> genmag2[ RemInt( j-1, nag ) + 1  
            + nag * (( QuoInt( j-1, nag ) + 1 )^pi - 1 ) ] ) ); 
    od; 
    #  construct the semidirect product: symm acting on mag2
    actgp := Group( imact ); 
    ok := IsGroupOfAutomorphisms( actgp ); 
    action := GroupHomomorphismByImages( symm, actgp, gensymm, imact ); 
    sdp := SemidirectProduct( symm, action, mag2 ); 
    Info( InfoGroupoids, 2, "sdp has ", 
          Length(GeneratorsOfGroup(sdp)), " gens." ); 
    sinfo := SemidirectProductInfo( sdp ); 
    ## (13/04/16) comment this out for the moment and replace with identity 
    ## siso := SmallerDegreePermutationRepresentation( sdp ); 
    siso := IdentityMapping( sdp ); 
    ssdp := Image( siso ); 
    esymm := Embedding( sdp, 1 ) * siso; 
    egensymm := List( gensymm, g -> ImageElm( esymm, g ) );  
    emag2 := Embedding( sdp, 2 ) * siso; 
    egenmag2 := List( genmag2{[1..nag]}, g -> ImageElm( emag2, g ) ); 
    eno := ListWithIdenticalEntries( m+1, 0 );
    eno[1] := esymm; 
    for i in [1..m] do 
        im1 := List( genag1, g -> ImageElm( iso1, g ) ); 
        im2 := List( im1, g -> ImageElm( memb[i], g ) ); 
        im3 := List( im2, g -> ImageElm( emag2, g ) ); 
        eno[i+1] := GroupHomomorphismByImages( ag1, ssdp, genag1, im3 ); 
    od; 
    agens := Concatenation( egensymm, egenmag2 ); 
    return [ ssdp, agens, eno ]; 
end );

#############################################################################
##
#M  AutomorphismGroupOfGroupoid( <gpd> )
##
InstallMethod( AutomorphismGroupOfGroupoid, "for one-object groupoid", true, 
    [ IsGroupoid and IsSinglePieceDomain and IsDiscreteDomainWithObjects ], 0,
function( gpd ) 

    local autgen, rgp, agp, genagp, a, a0, ok, id, aut;  

    Info( InfoGroupoids, 2, 
          "AutomorphismGroupOfGroupoid for one-object groupoids" ); 
    autgen := [ ]; 
    rgp := gpd!.magma; 
    agp := AutomorphismGroup( rgp ); 
    genagp := SmallGeneratingSet( agp ); 
    for a in genagp do  
        a0 := GroupoidAutomorphismByGroupAutoNC( gpd, a ); 
        ok := IsAutomorphismWithObjects( a0 ); 
        Add( autgen, a0 );  
    od; 
    id := IdentityMapping( gpd ); 
    aut := GroupWithGenerators( autgen, id ); 
    SetIsAutomorphismGroup( aut, true ); 
    SetIsFinite( aut, true ); 
    SetIsAutomorphismGroupOfGroupoid( aut, true ); 
    SetAutomorphismGroup( gpd, aut );
    Info( InfoGroupoids, 2, "nice object not yet coded in this case" ); 
    return aut; 
end ); 

InstallMethod( AutomorphismGroupOfGroupoid, "for a single piece groupoid", 
    true, [ IsGroupoid and IsSinglePieceDomain ], 0,
function( gpd ) 

    local autgen, nautgen, rgp, genrgp, agp, genagp, a, obs, n, imobs, 
          L, ok, id, ids, cids, i, c, aut, niceob, nicemap, rgh, ioo, ior;  

    Info( InfoGroupoids, 2, 
          "AutomorphismGroupOfGroupoid for single piece groupoids" ); 
    ##  first: automorphisms of the root group 
    autgen := [ ]; 
    rgp := gpd!.magma; 
    genrgp := GeneratorsOfGroup( rgp ); 
    agp := AutomorphismGroup( rgp ); 
    genagp := SmallGeneratingSet( agp ); 
    for a in genagp do  
        Add( autgen, GroupoidAutomorphismByGroupAutoNC( gpd, a ) );  
    od; 
    ##  second: permutations of the objects 
    obs := gpd!.objects; 
    n := Length( obs );
    if ( n = 2 ) then 
        imobs := [ obs[2], obs[1] ]; 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
    else 
        L := [2..n]; 
        Append( L, [1] ); 
        imobs := List( L, i -> obs[i] ); 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
        imobs := ShallowCopy( obs ); 
        imobs[1] := obs[2]; 
        imobs[2] := obs[1]; 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
    fi; 
    ##  third: add in the ray prods 
    ids := List( obs, o -> One( rgp ) ); 
    for i in [2..n] do 
        for c in genrgp do 
            cids := ShallowCopy( ids ); 
            cids[i] := c; 
            Add( autgen, GroupoidAutomorphismByRayShifts( gpd, cids ) ); 
        od; 
    od; 
    nautgen := Length( autgen ); 
    ##  generating set for the automorphism group now complete 
    for a in autgen do 
        ok := IsAutomorphismWithObjects( a ); 
    od; 
    id := IdentityMapping( gpd ); 
    ## imobs := L[0]; 
    aut := GroupWithGenerators( autgen, id ); 
    SetIsAutomorphismGroup( aut, true ); 
    SetIsFinite( aut, true ); 
    SetIsAutomorphismGroupOfGroupoid( aut, true );
    niceob := NiceObjectAutoGroupGroupoid( gpd, aut ); 
    SetNiceObject( aut, niceob[1] ); 
    SetEmbeddingsInNiceObject( aut, niceob[2] ); 
    #?  SetInnerAutomorphismsAutomorphismGroup( aut, ?? ); 
    SetAutomorphismGroup( gpd, aut );

    ## now construct nicemap using GroupHomomorphismByFunction 
    nicemap := GroupHomomorphismByFunction( aut, niceob[1], 
        function( alpha ) 
            local G, autG, q, eno, obG, nobG, oha, j, ioa, Lpos, Lmap; 
            G := Source( alpha ); 
            autG := AutomorphismGroup( G ); 
            q := One( NiceObject( autG ) );
            eno := EmbeddingsInNiceObject( autG ); 
            obG := ObjectList( G ); 
            nobG := Length( obG ); 
            rgh := RootGroupHomomorphism( alpha ); 
            q := ImageElm( eno[1], rgh ); 
            ior := ImageElementsOfRays( alpha ); 
            for j in [1..nobG] do 
                q := q * ImageElm( eno[j+2], ior[j] ); 
            od;
            ioo := ImagesOfObjects( alpha ); 
            Lpos := List( ioo, j -> Position( obG, j ) ); 
            Lmap := MappingPermListList( [1..nobG], Lpos ); 
            q := q * ImageElm( eno[2], Lmap ); 
            return q;             
        end); 

    SetNiceMonomorphism( aut, nicemap ); 
    SetIsHandledByNiceMonomorphism( aut, true ); 
    SetIsCommutative( aut, IsCommutative( niceob[1] ) );
    return aut; 
end ); 

InstallMethod( AutomorphismGroupOfGroupoid, "for a hom. discrete groupoid", 
    true, [ IsHomogeneousDiscreteGroupoid ], 0,
function( gpd ) 

    local pieces, autgen, nautgen, p1, g1, ag1, id1, genag1, a, b, auts, 
          obs, n, imobs, L, ok, id, ids, aut, niceob, nicemap;  

    Info( InfoGroupoids, 2, 
          "AutomorphismGroupOfGroupoid for homogeneous discrete groupoids" ); 
    pieces := Pieces( gpd ); 
    obs := ObjectList( gpd ); 
    n := Length( obs );
    ##  first: automorphisms of the first object group 
    autgen := [ ]; 
    p1 := pieces[1]; 
    g1 := p1!.magma; 
    ag1 := AutomorphismGroup( g1 ); 
    id1 := IdentityMapping( g1 ); 
    auts := ListWithIdenticalEntries( n, id1 ); 
    genag1 := SmallGeneratingSet( ag1 ); 
    for a in genag1 do 
        auts[1] := a; 
        b := GroupoidAutomorphismByGroupAutos( gpd, auts ); 
        SetIsGroupWithObjectsHomomorphism( b, true ); 
        Add( autgen, b );  
    od; 
    ##  second: permutations of the objects 
    if ( n = 2 ) then 
        imobs := [ obs[2], obs[1] ]; 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
    else 
        L := [2..n]; 
        Append( L, [1] ); 
        imobs := List( L, i -> obs[i] ); 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
        imobs := ShallowCopy( obs ); 
        imobs[1] := obs[2]; 
        imobs[2] := obs[1]; 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
    fi; 
    nautgen := Length( autgen ); 
    ##  generating set for the automorphism group now complete 
    for a in autgen do 
        ok := IsAutomorphismWithObjects( a ); 
    od; 
    id := IdentityMapping( gpd ); 
    ## imobs := L[0];
    aut := GroupWithGenerators( autgen, id ); 
    SetIsAutomorphismGroup( aut, true ); 
    SetIsFinite( aut, true ); 
    SetIsAutomorphismGroupOfGroupoid( aut, true );
    niceob := NiceObjectAutoGroupGroupoid( gpd, aut ); 
    SetNiceObject( aut, niceob[1] ); 
    SetEmbeddingsInNiceObject( aut, niceob[3] ); 
    SetAutomorphismGroup( gpd, aut ); 

    ## now construct nicemap using GroupHomomorphismByFunction 
    nicemap := GroupHomomorphismByFunction( aut, niceob[1], 
        function( alpha ) 
            local G, autG, q, eno, obG, nobG, oha, j, ioa, Lpos, Lmap; 
            G := Source( alpha ); 
            autG := AutomorphismGroup( G ); 
            q := One( NiceObject( autG ) );
            eno := EmbeddingsInNiceObject( autG ); 
            obG := ObjectList( G ); 
            nobG := Length( obG ); 
            oha := ObjectHomomorphisms( alpha ); 
            for j in [1..nobG] do 
                q := q * ImageElm( eno[j+1], oha[j] ); 
            od;
            ioa := ImagesOfObjects( alpha ); 
            Lpos := List( ioa, j -> Position( obG, j ) ); 
            Lmap := MappingPermListList( Lpos, [1..nobG] ); 
            q := q * ImageElm( eno[1], Lmap ); 
            return q;             
        end); 

    SetNiceMonomorphism( aut, nicemap ); 
    SetIsHandledByNiceMonomorphism( aut, true ); 
    #?  SetInnerAutomorphismsAutomorphismGroup( aut, ?? );  
    SetIsCommutative( aut, IsCommutative( niceob[1] ) );
    return aut; 
end ); 

#############################################################################
##
#M  InAutomorphismGroupOfGroupoid( <a>, <aut> ) 
##  . . . . for an element in an automorphism group of a groupoid 
##  . . . . should be a method for \in, but cannot make that work at present 
##
InstallMethod( InAutomorphismGroupOfGroupoid, 
    "for groupoid hom and automorphism group : single piece", 
    true, [ IsGroupoidHomomorphism and IsDefaultGroupoidHomomorphismRep, 
            IsAutomorphismGroupOfGroupoid ], 0,
function( a, aut ) 
    #? this needs to be coded 
    Error( "not yet written" ); 
end ); 

InstallMethod( InAutomorphismGroupOfGroupoid, 
    "for groupoid hom and automorphism group : discrete", true, 
    [ IsGroupoidHomomorphismFromHomogeneousDiscrete and IsGroupoidHomomorphism,
      IsAutomorphismGroupOfGroupoid ], 0, 
function( a, aut ) 

    local gens, g1, gpd, obs, imobs, G, AG; 

    gens := GeneratorsOfGroup( aut ); 
    gpd := Source( gens[1] ); 
    if not ( ( Source(a) = gpd ) and ( Range(a) = gpd ) ) then 
        Info( InfoGroupoids, 2, "require Source(a) = Range(a) = gpd" ); 
        return false; 
    fi; 
    obs := ObjectList( gpd );
    imobs := ShallowCopy( ImagesOfObjects( a ) ); 
    Sort( imobs ); 
    if not ( obs = imobs ) then 
        Info( InfoGroupoids, 2, "incorrect images of objects" ); 
        return false;  
    fi; 
    G := Source(a)!.magma; 
    AG := AutomorphismGroup( G ); 
    if not ForAll( ObjectHomomorphisms(a), h -> h in AG ) then 
        Info( InfoGroupoids, 2, "object homomorphisms incorrect" ); 
        return false; 
    fi; 
    #? is there anything else to test? 
    return true; 
end ); 

#############################################################################
##
#M  ImageElm( <map>, <e> )
##
InstallOtherMethod( ImageElm, "for a groupoid mapping between single pieces", 
    true, [ IsGroupoidHomomorphism and IsHomomorphismFromSinglePiece, 
    IsGroupoidElement ], 0,
function ( map, e )

    local m1, imo, obs1, pt1, ph1, ray1, rims, loop, iloop, g2;

    Info( InfoGroupoids, 3, 
          "this is the first ImageElm method in gpdhom.gi" ); 
    m1 := Source( map ); 
    if not ( e in m1 ) then 
        Error( "the element e is not in the source of mapping map" ); 
    fi; 
    imo := ImagesOfObjects( map ); 
    obs1 := m1!.objects; 
    pt1 := Position( obs1, e![2] ); 
    ph1 := Position( obs1, e![3] ); 
    ray1 := RayElementsOfGroupoid( m1 ); 
    loop := ray1[pt1] * e![1] * ray1[ph1]^-1; 
    iloop := ImageElm( RootGroupHomomorphism( map ), loop ); 
    rims := ImageElementsOfRays( map ); 
    g2 := rims[pt1]^-1 * iloop * rims[ph1]; 
    return ArrowNC( true, g2, imo[pt1], imo[ph1] );
end ); 

InstallOtherMethod( ImageElm, "for a map from homogeneous, discrete groupoid", 
    true, [ IsGroupoidHomomorphismFromHomogeneousDiscrete, 
            IsGroupoidElement ], 6,
function ( map, e )

    local p1, t2, g2, a;

    Info( InfoGroupoids, 3, 
          "this is the second ImageElm method in gpdhom.gi" ); 
    if not ( e in Source(map) ) then 
        Error( "the element e is not in the source of mapping map" ); 
    fi; 
    p1 := Position( Source( map )!.objects, e![2] ); 
    t2 := ImagesOfObjects( map )[ p1 ]; 
    g2 := ImageElm( ObjectHomomorphisms( map )[ p1 ], e![1] ); 
    a := ArrowNC( true, g2, t2, t2 );
    return a;
end ); 

InstallOtherMethod( ImageElm, "for a groupoid mapping", true, 
    [ IsGroupoidHomomorphism, IsGroupoidElement ], 0,
function ( map, e )

    local src, rng, pe, mape;

    Info( InfoGroupoids, 3, 
          "this is the third ImageElm method in gpdhom.gi" ); 
    if not ( e in Source(map) ) then 
        Error( "the element e is not in the source of mapping map" ); 
    fi; 
    src := Source( map ); 
    rng := Range( map ); 
    pe := Position( Pieces(src), PieceOfObject( src, e![2] ) ); 
    if ( HasIsSinglePiece( rng ) and IsSinglePiece( rng ) ) then 
        mape := MappingToSinglePieceMaps( map )[pe]; 
    else 
        mape := PiecesOfMapping( map )[pe]; 
    fi;
    return ImageElm( mape, e ); 
end );

InstallOtherMethod( ImagesRepresentative, "for a groupoid homomorphism", true, 
    [ IsGroupoidHomomorphism, IsGroupoidElement ], 0, 
function( map, e ) 
    Info( InfoGroupoids, 3, "ImagesRepresentative at gpdhom.gi line 1258" ); 
    return ImageElm( map, e ); 
end );

#############################################################################
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

#############################################################################
##
#M  IsomorphismPermGroupoid
#M  IsomorphismPcGroupoid
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
        ray1 := RaysOfGroupoid( g1 );  
        ray2 := List( ray1, g -> ImageElm( iso, g![1] ) ); 
        g2 := SubgroupoidWithRays( par2, gp2, ray2 ); 
    fi; 
    gen1 := GeneratorsOfGroupoid( g1 ); 
    gen2 := List( gen1, g -> Arrow( g2, ImageElm(iso,g![1]), g![2], g![3] ) ); 
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
    g2 := UnionOfPieces( List( isos, m -> Image(m) ) );
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
        ray1 := RaysOfGroupoid( g1 );  
        ray2 := List( ray1, g -> ImageElm( iso, g![1] ) ); 
        g2 := SubgroupoidWithRays( par2, gp2, ray2 ); 
    fi; 
    gen1 := GeneratorsOfGroupoid( g1 ); 
    gen2 := List( gen1, g -> Arrow( g2, ImageElm(iso,g![1]), g![2], g![3] ) ); 
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
    g2 := UnionOfPieces( List( isos, m -> Image(m) ) );
    iso := HomomorphismByUnion( g1, g2, isos );
    return iso;
end );

#############################################################################
##
#M  IsomorphismNewObjects
##
InstallMethod( IsomorphismNewObjects, "for a single piece groupoid", true,
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd1, ob2 )

    local iso, ob1, gp, gpd2, gens1, gens2, ngens, i, g, pt, ph;

    if not IsDirectProductWithCompleteDigraphDomain( gpd1 ) then 
        Error( "make an isomorphism for the parent groupoid first" );
    fi;
    ob1 := gpd1!.objects; 
    if not ( Length(ob1) = Length(ob2) ) then
        Error( "object sets have different lengths" );
    fi;
    gp := gpd1!.magma;
    gpd2 := SinglePieceGroupoidNC( gp, ShallowCopy( Set( ob2 ) ) );
    gens1 := GeneratorsOfGroupoid( gpd1 ); 
    ngens := Length( gens1 );
    gens2 := [1..ngens]; 
    for i in [1..ngens] do 
        g := gens1[i]; 
        pt := Position( ob1, g![2] ); 
        ph := Position( ob1, g![3] ); 
        gens2[i] := Arrow( gpd2, g![1], ob2[pt], ob2[ph] );
    od;
    iso := GroupoidHomomorphismFromSinglePiece( gpd1, gpd2, gens1, gens2 ); 
    return iso;
end );

#############################################################################
##
#M  IsomorphismStandardGroupoid
##
InstallMethod( IsomorphismStandardGroupoid, "for a single piece groupoid", 
    true, [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd1, obs )

    local obs1, obs2, gp, gpd2, gens1, gens2;

    if IsDirectProductWithCompleteDigraphDomain( gpd1 ) then 
        return IdentityMapping( gpd1 ); 
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


## ======================================================================== ##
##                     Homogeneous groupoid homomorphisms                   ##
## ======================================================================== ##

#############################################################################
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
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousDiscreteGroupoid, 
      IsHomogeneousList, IsHomogeneousList ], 0,
function( src, rng, homs, oims ) 

    local gps, gpr, obs, obr; 

    Info( InfoGroupoids,3, "method for GpdHomFromHomDisc to discrete gpd" );
    gps := src!.magma; 
    gpr := rng!.magma; 
    if not ForAll( homs, 
        h -> ( Source(h) = gps ) and ( Range(h) = gpr ) ) then 
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
    return GroupoidHomomorphismFromHomogeneousDiscreteNC( src,rng,homs,oims ); 
end );

InstallMethod( GroupoidHomomorphismFromHomogeneousDiscrete,
    "method for a mapping from a homogeneous, discrete groupoid", true,
    [ IsHomogeneousDiscreteGroupoid, IsGroupoid, IsHomogeneousList, 
      IsHomogeneousList ], 0,
function( src, rng, homs, oims ) 

    local gps, obs, obr, lens; 

    Info( InfoGroupoids,3, "method for GpdHomFromHomDiscNC to general gpd" );
    obs := ObjectList( src ); 
    obr := ObjectList( rng); 
    lens := Length( obs ); 
    if not ( Length(oims) = lens ) then 
        Error( "<oims> has incorrect length" ); 
    fi; 
    if not ForAll( oims, o -> o in obr ) then 
        Error( "object images not all objects in <rng>" ); 
    fi; 
    gps := src!.magma; 
    if not ForAll( [1..lens], 
        j -> ( Source( homs[j] ) = gps ) 
               and ( Range( homs[j] ) = ObjectGroup( rng, oims[j] ) ) ) then 
        Error("homs not a list of maps ObjectGroup(src) -> ObjectGroup(rng),"); 
    fi; 
    return GroupoidHomomorphismFromHomogeneousDiscreteNC( src,rng,homs,oims ); 
end );

##############################################################################
##
#E  gpdhom.gi  . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##
