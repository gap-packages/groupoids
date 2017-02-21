############################################################################## 
##
#W  gpdhom.gi                 GAP4 package `gpd'                 Chris Wensley
#W                                                                & Emma Moore
#Y  Copyright (C) 2000-2017, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

#############################################################################
##  Standard error messages

GROUPOID_MAPPING_CONSTRUCTORS := Concatenation(
    "The standard operations which construct a groupoid mapping are:\n", 
    "1.  GroupoidHomomorphism( src, rng, hom | src, rng, hom, oims );\n", 
    " or GroupoidHomomorphism( one of the following parameter options );\n",
    "2.  GroupoidHomomorphismFromSinglePiece( src, rng, hom, oims, rims );\n",
    "3.  HomomorphismToSinglePiece( src, rng, list of [hom,imobs,rays]'s );\n",
    "4.  HomomorphismByUnion( src, rng, list of disjoint mappings );\n", 
    "5.  GroupoidAutomorphismByGroupAuto( gpd, auto );\n", 
    "6.  GroupoidAutomorphismByObjectPerm( gpd, oims );\n", 
    "7.  GroupoidAutomorphismByRayShifts( gpd, rims );\n" ); 

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

    local  one, rays, iso;

    Info( InfoGpd, 3, "using IdentityMapping  in gpdaut.gi" ); 
    if ( HasIsDirectProductWithCompleteGraphDomain( gpd ) 
         and IsDirectProductWithCompleteGraphDomain( gpd ) ) then 
        one := One( gpd!.magma ); 
        rays := List( gpd!.objects, o -> one ); 
    else 
        rays := gpd!.rays; 
    fi; 
    iso := GroupoidHomomorphismFromSinglePieceNC
        ( gpd, gpd, IdentityMapping(gpd!.magma), gpd!.objects, rays );
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
#?  more cases ?? 
##
InstallGlobalFunction( GroupoidHomomorphism, function( arg )

    local  nargs, id, rays;
    nargs := Length( arg );

    if ( ( nargs < 2 ) 
         or ( nargs > 5 )  
         or not IsMagmaWithObjects( arg[1] ) 
         or ( ( nargs > 2 ) and not IsMagmaWithObjects( arg[2] ) ) 
       ) then Info( InfoGpd, 1, GROUPOID_MAPPING_CONSTRUCTORS );
              return fail;
    fi; 
    # various types of automorphism 
    if ( ( nargs = 2 ) and IsSinglePiece( arg[1] ) ) then 
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
        return GroupoidHomomorphismFromSinglePieceNC( arg[1], arg[2], arg[3], 
                   arg[2]!.objects, RayElementsOfGroupoid( arg[2] ) ); 
    # mwo, mwo, list of mappings
    elif ( ( nargs = 3 ) and IsHomogeneousList( arg[3] ) 
            and IsHomomorphismToSinglePiece( arg[3][1] ) ) then
        Info( InfoGpd, 2, "HomomorphismByUnion" );
        return HomomorphismByUnion( arg[1], arg[2], arg[3] );
    # gpd, gpd, list of [hom, images of objects, rays] triples
    elif ( ( nargs = 3 ) and IsHomogeneousList( arg[3] ) 
           and IsList( arg[3][1] ) and IsList( arg[3][1][1] ) ) then 
        Info( InfoGpd, 2, "HomomorphismToSinglePiece" );
        return HomomorphismToSinglePiece( arg[1], arg[2], arg[3] ); 
    elif ( ( nargs = 4 ) and IsGroupHomomorphism( arg[3] ) 
           and ( IsHomogeneousList( arg[4] ) ) ) then 
        return GroupoidHomomorphismFromSinglePieceNC( arg[1], arg[2], arg[3], 
                   arg[4], RayElementsOfGroupoid( arg[2] ) ); 
    elif ( nargs = 5 ) then 
        return GroupoidHomomorphismFromSinglePieceNC( 
                   arg[1], arg[2], arg[3], arg[4], arg[5] );
    else
        Info( InfoGpd, 1, GROUPOID_MAPPING_CONSTRUCTORS );
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
    local  sobs, o1, c1, mappings, comps, m, inc, mor;

    if not IsSubgroupoid( gpd, sgpd ) then
        Error( "arg[2] is not a subgroupoid of arg[1]" );
    fi;
    if IsSinglePiece( sgpd ) then
        sobs := sgpd!.objects;
        if IsSinglePiece( gpd ) then
            Info( InfoGpd, 2, "InclusionMapping, one piece -> one piece" );
            inc := InclusionMappingGroups( gpd!.magma, sgpd!.magma );
            if ( inc = fail ) then
                Error( "magma mapping fails to include" );
            fi;
            return GroupoidHomomorphismFromSinglePieceNC( 
                       sgpd, gpd, inc, sobs, RayElementsOfGroupoid( sgpd ) );
        else
            o1 := sgpd!.objects[1];
            c1 := PieceOfObject( gpd, o1 );
            inc := InclusionMappingGroups( c1!.magma, sgpd!.magma );
            if ( inc = fail ) then
                Error( "magma mapping fails to include" );
            fi;
            mor := GroupoidHomomorphismFromSinglePieceNC( 
                       sgpd, c1, inc, sobs, RayElementsOfGroupoid( sgpd ) );
            return MagmaWithObjectsHomomorphism( sgpd, gpd, [ mor ] );
        fi;
    else
        Info( InfoGpd, 2, "InclusionMapping, source not connected" );
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
    local  obs, inc, homs; 
    Info( InfoGpd, 2, "new InclusionMappingGroupoids method" ); 
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
    [ IsGroupoidHomomorphism, IsGroupoid and IsSinglePiece ], 0,
function( mor, U )

    local  smor, rmor, images, imo, hom, obsrc, obsU, len, imobsU, i, 
           pos, sres, sgen, rres, qres, qgen, rhom, gens, imgens, ghom, 
           rays, rims, e, V, res;

    smor := Source( mor );
    rmor := Range( mor );
    if not ( IsDirectProductWithCompleteGraph( smor ) and 
             IsDirectProductWithCompleteGraph( rmor ) ) then 
        Error( "only implemented for standard groupoids at present" ); 
    fi;
    if not ( IsSubdomainWithObjects( smor, U ) ) then
        Error( "U not a submagma of Source(mor)" );
    fi;
    images := PieceImages( mor );
    imo := images[1][2];
    hom := images[1][1];
    obsrc := smor!.objects; 
    obsU := ObjectList( U );
    len := Length( obsU );
    imobsU := ListWithIdenticalEntries( len, 0 ); 
    for i in [1..len] do 
        pos := Position( obsrc, obsU[i] );
        imobsU[i] := imo[pos]; 
    od;
    sres := U!.magma; 
    rhom := RestrictedMapping( hom, sres );
    rres := Image( rhom );
    sgen := GeneratorsOfGroup( sres ); 
    gens := List( GeneratorsOfGroup(sres), s -> Arrow(U,s,obsU[1],obsU[1]) ); 
    imgens := List( gens, s -> ImageElm(mor,s) ); 
    rays := RaysOfGroupoid( U );
    rims := List( rays, r -> ImageElm(mor,r) ); 
    V := SinglePieceSubgroupoidByGenerators( rmor, 
             Concatenation( imgens, rims{[2..len]} ) ); 
    qres := RootGroup( V );
    qgen := GeneratorsOfGroup( qres ); 
    ghom := GroupHomomorphismByImages( sres, qres, sgen, qgen );  
    rims := List( rims, r -> r![1] ); 
    res := GroupoidHomomorphismFromSinglePiece( U, V, ghom, imobsU, rims ); 
    return res; 
end );

#?  this one not checked: use GeneralRestrictedMapping? 
InstallMethod( RestrictedMappingGroupoids, "for a groupoid mapping", true,
    [ IsGroupoidHomomorphism, IsGroupoid ], 0,
function( mor, ssrc )

    local  comp, len, rcomp, j, m;

    comp := PiecesOfMapping( mor );
    len := Length( comp );
    rcomp := ListWithIdenticalEntries( len, 0 );
    for j in [1..len] do
        rcomp[j] := RestrictedMappingGroupoids( comp[j], ssrc );
    od;
    return HomomorphismByUnionNC( Source(mor), Range(mor), rcomp );
end );

############################################################################# 
##
#M  GroupoidHomomorphismFromDiscreteToSinglePiece
##
## InstallMethod( GroupoidHomomorphismFromDiscreteToSinglePiece,
##     "for a list of groupoid mappings with common connected range", true,
##     [ IsGroupoid and IsDiscrete, IsGroupoid and IsSinglePiece, 
##       IsHomogeneousList ], 0,  
## function( src, rng, maps )

#############################################################################
##
#M  RootGroupHomomorphism . . . . . . . for a groupoid hom from single piece 
##
InstallMethod( RootGroupHomomorphism, "for a groupoid hom from a single piece", 
    true, [ IsGroupoidHomomorphism and IsHomomorphismToSinglePiece ], 0,
function( mor )
    local  gpd1, gpd2, ob2, imobs, roh, mgi, ray, gp2, im2, hom;
    gpd2 := Range( mor ); 
    ob2 := gpd2!.objects; 
    imobs := ImagesOfObjects( mor ); 
    roh := RootGroupHomomorphism( mor ); 
    if ( imobs[1] = ob2[1] ) then  ## root maps to root 
        hom := roh; 
    elif ( HasIsDirectProductWithCompleteGraph( gpd2 ) and 
           IsDirectProductWithCompleteGraph( gpd2 ) ) then 
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

    local  src, imobs, pos, obg, gens, loops, imloops, imgens, img, hom; 
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
    local  len, L; 
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
    local  len, L; 
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
    "for objects and images", true, [ IsDefaultGroupoidHomomorphismRep ], 0,
function( map )
    local  obs, ims; 
    ims := ImagesOfObjects( map );
    obs := Filtered( Range( map )!.objects, o -> o in ims ); 
    if not ( Length(obs) = Length(ims) ) then 
        Info( InfoGpd, 1, 
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
#M  Display
##
InstallMethod( Display, "for a mapping of connected groupoids", true,
    [ IsGroupoidHomomorphism and IsHomomorphismFromSinglePiece ], 0,
function ( map ) 
    Print( "  groupoid mapping: " ); 
    Print( "[ ", Source(map), " ] -> [ ", Range(map), " ]\n" ); 
    Print( " root homomorphism: ", 
           MappingGeneratorsImages( RootGroupHomomorphism(map) ), "\n" ); 
    Print( " images of objects: ", ImagesOfObjects( map ), "\n" ); 
    Print( "image elts of rays: ", ImageElementsOfRays( map ), "\n" ); 
end );

InstallMethod( Display, "for a mapping from a homogeneous discrete gpd", true, 
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep ], 0,
function ( map ) 
    local  h; 
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
      IsGroupHomomorphism, IsHomogeneousList, IsHomogeneousList ], 0,
function( src, rng, hom, oims, rims )

    local  fam, filter, map, ok;

    ## ?? (23/04/10)  fam := FamilyObj( [ src, rng, hom, oims, rims ] );
    fam := GroupoidHomomorphismFamily; 
    filter := IsDefaultGroupoidHomomorphismRep and IsGroupoidHomomorphism; 
    map := rec(); 
    ObjectifyWithAttributes( map, NewType( fam, filter ), 
        Source, src, 
        Range, rng, 
        RootGroupHomomorphism, hom, 
        ImagesOfObjects, oims, 
        ImageElementsOfRays, rims,  
        IsGeneralMappingWithObjects, true, 
        IsGroupoidHomomorphism, true, 
        IsHomomorphismToSinglePiece, true, 
        RespectsMultiplication, true ); 
    ok := IsInjectiveOnObjects( map ); 
    ok := IsSurjectiveOnObjects( map ); 
    SetIsHomomorphismFromSinglePiece( map, true ); 
    #? (10/10/08) do we really need this ?? 
    ## (15/06/11) added extra [ ] 
    SetPieceImages( map, [ [ hom, oims, rims ] ] );
    if ( src = rng ) then 
        SetIsEndoGeneralMapping( map, true ); 
        SetIsEndomorphismWithObjects( map, true ); 
        ## ok := IsAutomorphismWithObjects( map ); 
    fi; 
    return map; 
end );

InstallMethod( GroupoidHomomorphismFromSinglePiece,
    "generic method for a mapping of connected groupoids", true,
    [ IsGroupoid and IsSinglePiece, IsGroupoid and IsSinglePiece, 
      IsGroupHomomorphism, IsHomogeneousList, IsHomogeneousList ], 0,
function( src, rng, hom, oims, rims ) 

    local  gps, gpr, obs, obr, lens, lenr, rayr, pos1, i, posi, rayi; 

    gps := RootGroup( src ); 
    gpr := RootGroup( rng ); 
    if not ( ( Source(hom) = gps ) and ( Range(hom) = gpr ) ) then 
        Error( "hom not a map from RootGroup(src) to RootGroup(rng) " ); 
    fi; 
    obs := ObjectList( src ); 
    lens := Length( obs ); 
    obr := ObjectList( rng ); 
    lenr := Length( obr ); 
    if not ( ( Length(oims) = lens ) and ( Length(rims) = lens ) ) then 
        Error( "oims and/or rims have incorrect length" ); 
    fi; 
    if not ForAll( oims, o -> o in obr ) then 
        Error( "object images not all objects in rng" ); 
    fi; 
    if not ( rims[1] = One( gpr ) ) then 
        Error( "first ray image not the identity" ); 
    fi; 
    rayr := RayElementsOfGroupoid( rng ); 
    pos1 := Position( obr, oims[1] ); 
    for i in [2..lens] do 
        posi := Position( obr, oims[i] ); 
        rayi := rayr[pos1] * rims[i] * rayr[posi]^-1; 
        if ( IsGroupoidHomomorphism( rayi ) and 
             IsDefaultGroupoidHomomorphismRep( rayi ) ) then 
            ## this is the automorphism group of groupoid case 
            if not InAutomorphismGroupOfGroupoid( rayi, gpr ) then 
                Error( "ray images not all in relevant automorphism homset" ); 
            fi; 
        elif HasParentAttr( gpr ) then 
            if not ( rayi in Parent( gpr ) ) then 
                Error( "ray images not all in relevant parent homset" ); 
            fi; 
        else 
            if not ( rayi in gpr ) then 
                Error( "ray images not all in relevant homset" ); 
            fi; 
        fi; 
    od;
    return GroupoidHomomorphismFromSinglePieceNC( src, rng, hom, oims, rims ); 
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
    local  rgp, obs, nobs, hom, rims, pos, rays, pos1, ior1, posi, mor, L; 
    rgp := gpd!.magma; 
    hom := InclusionMappingGroups( rgp, gpd!.magma ); 
    obs := gpd!.objects; 
    nobs := Length( obs ); 
    if ( HasIsDirectProductWithCompleteGraph( gpd ) 
         and IsDirectProductWithCompleteGraph( gpd ) ) then 
        rims := ListWithIdenticalEntries( nobs, One( rgp ) ); 
    else 
        rays := gpd!.rays; 
        pos1 := Position( obs, oims[1] ); 
        ior1 := rays[pos1]^-1; 
        rims := List( [1..nobs], i -> ior1 * rays[ Position(obs,oims[i]) ] ); 
    fi; 
    #? (06/07/11) removed NC in next line so that checks are made
    mor := GroupoidHomomorphismFromSinglePiece( gpd, gpd, hom, oims, rims ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    L := [1..nobs]; 
    SortParallel( ShallowCopy( oims ), L );  
    SetOrder( mor, Order( PermList( L ) ) );
    return mor; 
end ); 

InstallMethod( GroupoidAutomorphismByObjectPermNC,  
    "for a hom discrete groupoid and a permutation of objects", true, 
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ], 0,
function( gpd, oims )

    local  gpd1, gp, id, homs, mor, L; 

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
    return mor; 
end ); 

InstallMethod( GroupoidAutomorphismByObjectPerm, 
    "for a hom discrete groupoid and a permutation of objects", true, 
    [ IsGroupoid, IsHomogeneousList ], 0,
function( gpd, oims )
    local  obs, pos; 
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
    local  rgp, obs, pos, rays, mor; 
    rgp := gpd!.magma; 
    obs := gpd!.objects;
    rays := RayElementsOfGroupoid( gpd ); 
    mor := GroupoidHomomorphismFromSinglePieceNC( gpd, gpd, hom, obs, rays ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    SetOrder( mor, Order( hom ) ); 
    return mor; 
end ); 

InstallMethod( GroupoidAutomorphismByGroupAuto, 
    "for a groupoid and an automorphism of the root group", true, 
    [ IsGroupoid and IsSinglePiece, IsGroupHomomorphism ], 0,
function( gpd, hom )  
    local  rgp; 
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
    local  obs, orders, m; 
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

    local  pieces, len, i, p, g, h; 

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
    local  obs, hom, mor, rays, rims; 
    obs := gpd!.objects;
    hom := IdentityMapping( gpd!.magma ); 
    rays := gpd!.rays; 
    rims := List( [1..Length(obs)], i -> shifts[i]*rays[i] );
    mor := GroupoidHomomorphismFromSinglePieceNC( gpd, gpd, hom, obs, rims ); 
    SetOrder( mor, Lcm( List( shifts, i -> Order(i) ) ) ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    return mor;
end ); 

InstallMethod( GroupoidAutomorphismByRayShifts, 
    "for a groupoid and a list of elements of the root group", true, 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd, shifts )  
    local  rgp, rays, nobs, conj; 
    rgp := gpd!.magma; 
    rays := gpd!.rays; 
    nobs := Length( gpd!.objects );
    if not ForAll( shifts, s -> s in rgp ) then  
        Error( "ray shifts not all in the relevant homsets" ); 
    fi; 
    return GroupoidAutomorphismByRayShiftsNC( gpd, shifts ); 
end ); 

#############################################################################
##
#M  NiceObjectAutoGroupGroupoid( <gpd>, <aut> ) . . create a nice monomorphism 
##
InstallMethod( NiceObjectAutoGroupGroupoid, "for a single piece groupoid", 
    true, [ IsGroupoid and IsSinglePieceDomain, IsAutomorphismGroup ], 0,

function( gpd, aut ) 

    local  genaut, rgp, genrgp, nrgp, agp, genagp, nagp, iso1, im1, iso2, 
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
    Info( InfoGpd, 2, "sdp has ", Length(GeneratorsOfGroup(sdp)), " gens." ); 
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

    local  pieces, obs, m, p1, g1, geng1, ng1, ag1, genag1, nag, 
           iso1, ag2, genag2, mag2, genmag2, nmag2, minfo, i, k, memb, 
           L, symm, gensymm, imact, pi, actgp, ok, action, sdp, sinfo, 
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
    ## iso2 := SmallerDegreePermutationRepresentation( ag2 );
    ## ag3 := Image( iso2 ); 
    ## iso := iso1*iso2; 
    ## genag3 := List( genag1, a -> ImageElm( iso, a ) ); 
    ## nag3 := Length( genag3 ); 
    ## now form the m-fold direct product of ag2
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
    if ( m = 2 ) then 
        gensymm := [ (1,2) ]; 
    else 
        L := [2..m]; 
        Append( L, [1] ); 
        gensymm := [ PermList(L), (1,2) ];  #? replace (1,2) with (m-1,m) ?? 
    fi;
    symm := Group( gensymm ); 
    ## action of symm on mag2 = ag2^m 
    imact := [1,2]; 
    for i in [1,2] do 
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
    Info( InfoGpd, 2, "sdp has ", Length(GeneratorsOfGroup(sdp)), " gens." ); 
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
InstallMethod( AutomorphismGroupOfGroupoid, "for a single piece groupoid", 
    true, [ IsGroupoid and IsSinglePieceDomain ], 0,

function( gpd ) 
    local  autgen, nautgen, rgp, genrgp, agp, genagp, a, obs, n, imobs, 
           L, ok, id, ids, cids, i, c, aut, niceob, nicemap, rgh, ioo, ior;  

    Info( InfoGpd, 2, "AutomorphismGroup for single piece groupoids" ); 
    ##  first: automorphisms of the root group 
    autgen := [ ]; 
    rgp := gpd!.magma; 
    genrgp := GeneratorsOfGroup( rgp ); 
    agp := AutomorphismGroup( rgp ); 
    genagp := SmallGeneratingSet( agp ); 
    for a in genagp do  
        Add( autgen, GroupoidAutomorphismByGroupAuto( gpd, a ) );  
    od; 
    ##  second: permutations of the objects 
    obs := gpd!.objects; 
    n := Length( obs );
    if ( n = 1 ) then 
        Print( "#I  single object case not yet dealt with\n" ); 
        # return pagp; 
    fi; 
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
            local  G, autG, q, eno, obG, nobG, oha, j, ioa, Lpos, Lmap; 
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
    local  pieces, autgen, nautgen, p1, g1, ag1, id1, genag1, a, auts, 
           obs, n, imobs, L, ok, id, ids, aut, niceob, nicemap;  

    Info( InfoGpd, 2, "AutomorphismGroup for homogeneous discrete groupoids" ); 
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
        Add( autgen, GroupoidAutomorphismByGroupAutos( gpd, auts ) );  
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
            local  G, autG, q, eno, obG, nobG, oha, j, ioa, Lpos, Lmap; 
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
    Error( "not yet written" ); 
end ); 

InstallMethod( InAutomorphismGroupOfGroupoid, 
    "for groupoid hom and automorphism group : discrete", true, 
    [ IsGroupoidHomomorphismFromHomogeneousDiscrete and IsGroupoidHomomorphism,
      IsAutomorphismGroupOfGroupoid ], 0, 
function( a, aut ) 

    local  gens, g1, gpd, obs, imobs, G, AG; 

    gens := GeneratorsOfGroup( aut ); 
    gpd := Source( gens[1] ); 
    if not ( ( Source(a) = gpd ) and ( Range(a) = gpd ) ) then 
        Info( InfoGpd, 2, "require Source(a) = Range(a) = gpd" ); 
        return false; 
    fi; 
    obs := ObjectList( gpd );
    imobs := ShallowCopy( ImagesOfObjects( a ) ); 
    Sort( imobs ); 
    if not ( obs = imobs ) then 
        Info( InfoGpd, 2, "incorrect images of objects" ); 
        return false;  
    fi; 
    G := Source(a)!.magma; 
    AG := AutomorphismGroup( G ); 
    if not ForAll( ObjectHomomorphisms(a), h -> h in AG ) then 
        Info( InfoGpd, 2, "object homomorphisms incorrect" ); 
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

    local  m1, imo, obs1, pt1, ph1, ray1, rims, loop, iloop, g2;

    #?  need to include some tests here ?? 
    m1 := Source( map ); 
    imo := ImagesOfObjects( map );                   ### BUT NOT YET USED!!! 
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

    local  p1, t2, g2, a;

    Info( InfoGpd, 5, 
        "using ImageElm for IsGeneralMappingFromHomDiscrete, line 1140" ); 
    #?  need to include some tests here ?? 
    p1 := Position( Source( map )!.objects, e![2] ); 
    t2 := ImagesOfObjects( map )[ p1 ]; 
    g2 := ImageElm( ObjectHomomorphisms( map )[ p1 ], e![1] ); 
    a := ArrowNC( true, g2, t2, t2 );
    return a;
end ); 

InstallOtherMethod( ImageElm, "for a groupoid mapping", true, 
    [ IsGroupoidHomomorphism, IsGroupoidElement ], 0,
function ( map, e )

    local  G1, C1, pe, obs1, pim, obs2, t2, h2, g2;

    Info( InfoGpd, 5, 
        "using ImageElm for general groupoid homomorphisms, line 1158" ); 
    #?  need to include some tests here ??
    G1 := Source( map ); 
    C1 := Pieces( G1 );
    pe := Position( C1, PieceOfObject( G1, e![2] ) ); 
    obs1 := C1[pe]!.objects; 
    pim := PieceImages( map )[pe]; 
    obs2 := pim[1]; 
    t2 := obs2[ Position( obs1, e![2] ) ];
    h2 := obs2[ Position( obs1, e![3] ) ]; 
    g2 := ImageElm( pim[2], e![1] );
    return Arrow( Range( map ), g2, t2, h2 );
end );

InstallOtherMethod( ImagesRepresentative, "for a groupoid homomorphism", true, 
    [ IsGroupoidHomomorphism, IsGroupoidElement ], 0, 
function( map, e ) 
    Info( InfoGpd, 3, "ImagesRep at gpdhom.gi line 1175" ); 
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

    local  t, src, obs, o, st, cst, x, ix, y, iy, z, iz; 
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
##
InstallMethod( IsomorphismPermGroupoid, "for a connected groupoid", true,
    [ IsGroupoid and IsSinglePiece ], 0,
function( g1 )

    local  iso, obs, g, g2p, p, g2;

    obs := g1!.objects;
    g := g1!.magma;
    g2p := IsomorphismPermGroupoid( g );
    p := Image( g2p );
    g2 := SinglePieceMagmaWithObjects( p, obs ); 
    #? (28/01/11) added "NC" and the rays, but not checked! 
    iso := GroupoidHomomorphismFromSinglePieceNC( 
               g1, g2, obs, g2p, RayElementsOfGroupoid( g2 ) );
    SetIsInjectiveOnObjects( iso, true );
    SetIsSurjectiveOnObjects( iso, true );
    return iso;
end );

InstallMethod( IsomorphismPermGroupoid, "generic method for a groupoid", 
    true, [ IsGroupoid ], 0,
function( g1 )

    local  isos, comp1, nc1, i, g2, iso;

    comp1 := Pieces( g1 );
    nc1 := Length( comp1 );
    isos := ListWithIdenticalEntries( nc1, 0 );
    for i in [1..nc1] do 
        isos[i] := IsomorphismPermGroupoid( comp1[i] );
    od;
    g2 := UnionOfPieces( List( isos, m -> Image(m) ) );
    iso := HomomorphismByUnion( g1, g2, isos );
    SetIsInjectiveOnObjects( iso, true );
    SetIsSurjectiveOnObjects( iso, true );
    return iso;
end );

#############################################################################
##
#M  IsomorphismNewObjects
##
InstallMethod( IsomorphismNewObjects, "for a single piece groupoid", true,
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd1, ob2 )

    local  iso, ob1, gp, gpd2, id;

    if not IsDirectProductWithCompleteGraphDomain( gpd1 ) then 
        Error( "make an isomorphism for the parent groupoid first" );
    fi;
    ob1 := gpd1!.objects; 
    if not ( Length(ob1) = Length(ob2) ) then
        Error( "object sets have different lengths" );
    fi;
    gp := gpd1!.magma;
    gpd2 := SinglePieceGroupoidNC( gp, ShallowCopy( Set( ob2 ) ) );
    id := IdentityMapping( gp );
## Print( id, ob2, RayElementsOfGroupoid(gpd1), "\n" ); 
    iso := GroupoidHomomorphismFromSinglePiece( 
               gpd1, gpd2, id, ob2, RayElementsOfGroupoid( gpd1 ) ); 
    return iso;
end );

#############################################################################
##
#M  IsomorphismStandardGroupoid
##
InstallMethod( IsomorphismStandardGroupoid, "for a single piece groupoid", 
    true, [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd1, obs )

    local  obs1, obs2, gp, id, rays, gpd2, iso;

    if IsDirectProductWithCompleteGraphDomain( gpd1 ) then 
        return IdentityMapping( gpd1 ); 
    fi;
    obs1 := gpd1!.objects; 
    obs2 := Set( obs ); 
    if not ( Length(obs1) = Length(obs2) ) then
        Error( "object sets have different lengths" );
    fi;
    gp := gpd1!.magma;
    gpd2 := SinglePieceGroupoidNC( gp, obs2 );
    rays := List( obs2, o -> One(gp) ); 
    id := IdentityMapping( gp );
    iso := GroupoidHomomorphismFromSinglePiece( gpd1, gpd2, id, obs2, rays ); 
    return iso;
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

    local  fam, filter, map, ok, p;

    fam := GroupoidHomomorphismFamily; 
    filter := IsGroupoidHomomorphismFromHomogeneousDiscreteRep 
              and IsGroupoidHomomorphismFromHomogeneousDiscrete; 
    map := rec(); 
    ObjectifyWithAttributes( map, NewType( fam, filter ), 
        Source, src, 
        Range, rng, 
        ObjectHomomorphisms, homs, 
        ImagesOfObjects, oims, 
        IsGeneralMappingWithObjects, true, 
        IsGroupoidHomomorphism, true, 
        RespectsMultiplication, true ); 
#    ok := IsInjectiveOnObjects( map ); 
#    p := ObjectTransformationOfGroupoidHomomorphism( map ); 
#    ok := IsSurjectiveOnObjects( map ); 
    SetIsGeneralMappingFromSinglePiece( map, false ); 
    SetPieceImages( map, [ [ homs, oims, [ ] ] ] );
#    if ( src = rng ) then 
#        SetIsEndoGeneralMapping( map, true ); 
#        SetIsEndomorphismWithObjects( map, true ); 
        ## ok := IsAutomorphismWithObjects( map ); 
#    fi; 
    return map; 
end );

InstallMethod( GroupoidHomomorphismFromHomogeneousDiscrete,
    "method for a mapping from a homogeneous, discrete groupoid", true,
    [ IsHomogeneousDiscreteGroupoid, IsGroupoid and IsDiscreteDomainWithObjects, 
      IsHomogeneousList, IsHomogeneousList ], 0,
function( src, rng, homs, oims ) 

    local  gps, gpr, obs, obr; 

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

    local  gps, obs, obr, lens; 

    obs := src!.objects; 
    obr := rng!.objects; 
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
