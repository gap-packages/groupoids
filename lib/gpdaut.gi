#############################################################################
##
#W  gpdaut.gi              GAP4 package `groupoids'            Chris Wensley
#W                                                              & Emma Moore

#############################################################################
##
#M  GroupoidAutomorphismByObjectPermNC  
#M  GroupoidAutomorphismByObjectPerm 
##
InstallMethod( GroupoidAutomorphismByObjectPermNC , 
    "for a single piece groupoid and a permutation of objects", true, 
    [ IsGroupoid and IsDirectProductWithCompleteDigraphDomain, 
      IsHomogeneousList ], 0,
function( gpd, oims ) 

    local obs, gens, ngens, images, i, a, pt, ph, mor, L; 

    obs := gpd!.objects; 
    gens := GeneratorsOfGroupoid( gpd ); 
    ngens := Length( gens ); 
    images := [1..ngens]; 
    for i in [1..ngens] do 
        a := gens[i]; 
        pt := Position( obs, a![3] ); 
        ph := Position( obs, a![4] );
        images[i] := Arrow( gpd, a![2], oims[pt], oims[ph] ); 
    od; 
    if ( fail in images ) then 
        Error( "the set of images contains 'fail'" ); 
    fi;
    mor := GroupoidHomomorphismFromSinglePiece( gpd, gpd, gens, images );
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    L := [1..Length(obs)]; 
    SortParallel( ShallowCopy( oims ), L );  
    SetOrder( mor, Order( PermList( L ) ) ); 
    SetIsGroupoidAutomorphismByObjectPerm( mor, true ); 
    SetAutomorphismDomain( mor, gpd ); 
    return mor; 
end ); 

InstallMethod( GroupoidAutomorphismByObjectPermNC , 
    "for a single piece groupoid with rays rep", true, 
    [ IsGroupoid and IsSinglePieceRaysRep, IsHomogeneousList ], 0,
function( gpd, oims ) 

    local iso, inv, sgpd, aut; 

    iso := IsomorphismStandardGroupoid( gpd, ObjectList( gpd ) ); 
    inv := InverseGeneralMapping( iso ); 
    sgpd := Image( iso ); 
    aut := GroupoidAutomorphismByObjectPermNC( sgpd, oims ); 
    return iso * aut * inv; 
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
    SetAutomorphismDomain( mor, gpd ); 
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

    local gens, images, i, a, mor; 

    gens := GeneratorsOfGroupoid( gpd ); 
    images := ShallowCopy( gens );
    for i in [1..Length(gens) - Length(ObjectList(gpd)) + 1] do 
        a := gens[i];
        images[i] := Arrow( gpd, ImageElm(hom,a![2]), a![3], a![4] ); 
    od;
    mor := GroupoidHomomorphismFromSinglePiece( gpd, gpd, gens, images ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    SetOrder( mor, Order( hom ) ); 
    SetIsGroupoidAutomorphismByGroupAuto( mor, true ); 
    SetAutomorphismDomain( mor, gpd ); 
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
    orders := List( homs, Order ); 
    SetOrder( m, Lcm( orders ) ); 
    SetAutomorphismDomain( m, gpd ); 
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
#M  GroupoidAutomorphismByNtupleNC  
#M  GroupoidAutomorphismByNtuple 
##
InstallMethod( GroupoidAutomorphismByNtupleNC , 
    "for a groupoid and a list of elements of the root group", true, 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd, shifts )

    local gens, ngens, obs, images, i, p, q, a, mor; 

    gens := GeneratorsOfGroupoid( gpd ); 
    ngens := Length( gens ); 
    obs := gpd!.objects;
    images := ShallowCopy( gens ); 
    for i in [1..ngens] do 
        a := gens[i];
        p := Position( obs, a![3] );
        q := Position( obs, a![4] );
        images[i] := Arrow( gpd, (shifts[p])^-1*a![2]*shifts[q], a![3], a![4] );
    od; 
    mor := GroupoidHomomorphismFromSinglePiece( gpd, gpd, gens, images ); 
    SetOrder( mor, Lcm( List( shifts, Order ) ) ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    SetIsGroupoidAutomorphismByRayShifts( mor, true ); 
    return mor;
end ); 

InstallMethod( GroupoidAutomorphismByNtuple, 
    "for a groupoid and a list of elements of the root group", true, 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd, shifts ) 

    local rgp, rays, nobs, conj; 

    rgp := gpd!.magma; 
    rays := gpd!.rays; 
    nobs := Length( gpd!.objects );
    if not ForAll( shifts, s -> s in rgp ) then  
        Error( "ray shifts not all in the root group" ); 
    fi; 
    return GroupoidAutomorphismByNtupleNC( gpd, shifts ); 
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
    if not ( shifts[1] = One( gpd!.magma ) ) then 
        Error( "the first ray shift is not the identity" ); 
    fi; 
    return GroupoidAutomorphismByNtupleNC( gpd, shifts );
end );

InstallMethod( GroupoidAutomorphismByRayShifts, 
    "for a groupoid and a list of elements of the root group", true, 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd, shifts ) 
    if not ( shifts[1] = One( gpd!.magma ) ) then 
        Error( "the first ray shift is not the identity" ); 
    fi; 
    return GroupoidAutomorphismByNtuple( gpd, shifts ); 
end ); 

#############################################################################
##
#M  GroupoidInnerAutomorphism 
#M  GroupoidInnerAutomorphismNormalSubgroupoid
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
    pos := Position( obs, e![3] ); 
    id := IdentityMapping( gpd!.magma );
    auts := List( [1..nobs], i -> id ); 
    auts[pos] := InnerAutomorphism( gpd!.magma, e![2] ); 
    return GroupoidAutomorphismByGroupAutosNC( gpd, auts ); 
end );

InstallMethod( GroupoidInnerAutomorphismNormalSubgroupoid, 
    "for a groupoid, a subgroupoid, and an element", true, 
    [ IsGroupoid and IsSinglePieceDomain, IsGroupoid, IsGroupoidElement ], 0,
function( gpd, sub, e ) 

    local obs, gens, images, pieces, n, homs, oims, i, p, o, q, h; 

    Info( InfoGroupoids, 3, "GroupoidInnerAutomorphismNormalSubgroupoid" ); 
    if not IsWideSubgroupoid( gpd, sub ) then 
        Error( "sub is not a subgroupoid of gpd" ); 
    fi;
    if IsSinglePiece( sub ) then
        if not ( e![2] in RootGroup( sub ) ) then
            Error( "element e![2] in not in the root group of sub" );
        fi;
    elif IsHomogeneousDomainWithObjects( sub ) then
        if not ( e![2] in RootGroup( Pieces(sub)[1] ) ) then 
            Error( "element e![2] in not in the root groups of sub" );
        fi;
    else
        Error( "invalid subgroupoid sub" );
    fi;
    obs := gpd!.objects;
    if IsSinglePiece( sub ) then
        gens := GeneratorsOfGroupoid( sub );
        images := List( gens, g -> g^e );
        return GroupoidHomomorphism( sub, sub, gens, images );
    elif IsHomogeneousDomainWithObjects( sub ) then
        pieces := Pieces( sub );
        n := Length( pieces );
        homs := ListWithIdenticalEntries( n, 0 );
        oims := ListWithIdenticalEntries( n, 0 );
        for i in [1..n] do
            p := pieces[i];
            gens := GeneratorsOfGroupoid( p );
            images := List( gens, g -> g^e );
            o := images[1]![3];
            oims[i] := o;
            q := pieces[ Position( obs, o ) ];
            h := GroupoidHomomorphism( p, q, gens, images );
            homs[i] := RootGroupHomomorphism( h );
        od;
        return GroupoidHomomorphismFromHomogeneousDiscrete( sub, sub, homs, oims );
    else
        return fail;
    fi;
end );

#############################################################################
##
#M  Size( <agpd> ) . . . . . . . . . . . . . . . . . for a connected groupoid
##
InstallMethod( Size, "for a groupoid automorphism group", true,  
    [ IsAutomorphismGroupOfGroupoid ], 0,
function( agpd ) 

    local gpd, gp, n, aut; 

    gpd := AutomorphismDomain( agpd ); 
    gp := gpd!.magma; 
    if IsSinglePieceDomain( gpd ) then  
        n := Length( ObjectList( gpd ) ); 
        aut := AutomorphismGroup( gpd!.magma );
        return Factorial( n ) * Size( aut ) * Size( gp )^(n-1); 
    elif IsDiscreteDomainWithObjects( gpd ) 
             and IsHomogeneousDomainWithObjects( gpd ) then 
        n := Length( ObjectList( gpd ) ); 
        aut := AutomorphismGroup( gpd!.magma ); 
        return Factorial( n ) * Size( aut )^n;             
    fi;
    return fail; 
end ); 

##############################################################################
##
#M  NiceObjectAutoGroupGroupoid( <gpd>, <aut> ) . . create a nice monomorphism 
##
InstallMethod( NiceObjectAutoGroupGroupoid, "for a single piece groupoid", true, 
    [ IsGroupoid and IsSinglePieceDomain, IsAutomorphismGroupOfGroupoid ], 0,
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
        a := GroupHomomorphismByImages( rgp, rgp, genrgp, 
                                        List( genrgp, x->x^c ) );
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

InstallMethod( NiceObjectAutoGroupGroupoid, "for a hom discrete groupoid", true,
    [ IsHomogeneousDiscreteGroupoid, IsAutomorphismGroupOfGroupoid ], 0,
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
    SetAutomorphismDomain( aut, gpd );
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
    SetAutomorphismDomain( aut, gpd );

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

    SetIsInjective( nicemap, true );
    SetNiceMonomorphism( aut, nicemap ); 
    ## SetIsHandledByNiceMonomorphism( aut, true ); 
    SetIsCommutative( aut, IsCommutative( niceob[1] ) );
    if HasName( gpd ) then 
        SetName( aut, Concatenation( "Aut(", Name( gpd ), ")" ) ); 
    fi;
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
    ##  generating set for the automorphism group now complete 
    nautgen := Length( autgen ); 
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
    SetAutomorphismDomain( aut, gpd );

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

    SetIsInjective( nicemap, true );
    SetNiceMonomorphism( aut, nicemap ); 
    ## SetIsHandledByNiceMonomorphism( aut, true ); 
    #?  SetInnerAutomorphismsAutomorphismGroup( aut, ?? );  
    SetIsCommutative( aut, IsCommutative( niceob[1] ) );
    if HasName( gpd ) then 
        SetName( aut, Concatenation( "Aut(", Name( gpd ), ")" ) ); 
    fi;
    return aut; 
end ); 

InstallMethod( AutomorphismGroupOfGroupoid, "for an arbitrary groupoid", 
    true, [ IsGroupoid ], 0,
function( gpd ) 
    Info( InfoGroupoids, 1, 
          "use AutomorphismGroupoidOfGroupoid for a union of pieces" ); 
    return fail; 
end ); 

## ========================================================================
##                     Homogeneous groupoid automorphisms                  
## ======================================================================== ##

## ????? more work to be done here 

InstallMethod( \in,
    "method for an automorphism of a single object groupoid", true,
    [ IsGroupWithObjectsHomomorphism, IsGroup ], 
    0,
function( a0, agp )

    local gens, src, obs, rng, mgi, rgp, argp;

    gens := GeneratorsOfGroup( agp ); 
    if not ForAll( gens, g -> IsGroupoidHomomorphism(g) 
                              and IsAutomorphismWithObjects(g) ) then 
        return false; 
    fi; 
    src := Source( a0 ); 
    obs := ObjectList( src ); 
    if ( Length( obs ) = 1 ) then 
        rng := Range( a0 ); 
        if not ( src = rng ) then 
            return false; 
        fi; 
        mgi := MappingGeneratorsImages( a0 ); 
        rgp := RootGroup( src ); 
        argp := AutomorphismGroup( rgp ); 
        return ForAll( mgi, g -> g in argp ); 
    else 
        Error( "method not yet implemented" ); 
    fi;
end ); 

InstallMethod( \in,
    "method for an automorphism of a single piece groupoid", true,
    [ IsGroupWithObjectsHomomorphism, IsAutomorphismGroupOfGroupoid ], 
    0,
function( a, aut )

    local gpd, gp, data;

    gpd := AutomorphismDomain( aut ); 
    gp := gpd!.magma; 
    data := MappingToSinglePieceData( a )[1];
    if not ( data[1] in AutomorphismGroup( gp ) ) then 
        return false; 
    fi; 
    if not ForAll( data[2], o -> o in ObjectList( gpd ) ) then 
        return false; 
    fi; 
    if not ForAll( data[3], e -> e in gp ) then 
        return false; 
    fi;
    return true; 
end ); 

InstallMethod( \in,
    "for groupoid hom and automorphism group : discrete", true, 
    [ IsGroupoidHomomorphismFromHomogeneousDiscrete and IsGroupoidHomomorphism,
      IsAutomorphismGroupOfGroupoid ], 0, 
function( a, aut ) 

    local gens, g1, gpd, obs, imobs, G, AG; 

    gens := GeneratorsOfGroup( aut ); 
    gpd := AutomorphismDomain( aut ); 
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

############################################################################## 
##  methods added 11/09/18 for automorphisms of homogeneous groupoids 

InstallMethod( GroupoidAutomorphismByPiecesPermNC,  
    "for a homogeneous groupoid and a permutation of the pieces", true, 
    [ IsGroupoid and IsHomogeneousDomainWithObjects, IsPerm ], 0,
function( gpd, p )

    local pieces, n, isos, mor; 

    pieces := Pieces( gpd ); 
    n := Length( pieces ); 
    isos := List( [1..n], 
              i -> IsomorphismNewObjects( pieces[i], pieces[i^p]!.objects ) );
    mor := HomomorphismByUnion( gpd, gpd, isos );
    SetOrder( mor, Order( p ) );
    SetIsGroupoidAutomorphismByPiecesPerm( mor, true ); 
    return mor; 
end ); 

InstallMethod( GroupoidAutomorphismByPiecesPerm, 
    "for a homogeneous groupoid and a permutation of pieces", true, 
    [ IsGroupoid and IsHomogeneousDomainWithObjects, IsPerm ], 0,
function( gpd, p ) 

    local pieces, n, obs, pos; 

    pieces := Pieces( gpd ); 
    n := Length( pieces ); 
    if ( LargestMovedPoint( p ) > n ) then 
        Error( "degree of permutation too large" ); 
    fi; 
    return GroupoidAutomorphismByPiecesPermNC( gpd, p ); 
end ); 

#############################################################################
##
#M  IsomorphismClassPositionsOfGroupoid
##
InstallMethod( IsomorphismClassPositionsOfGroupoid, "for a gpd with pieces", 
    true, [ IsGroupoid ], 0,
function( gpd ) 

    local P, lenP, found, i, classes, L, j, iso;  

    P := Pieces( gpd );
    lenP := Length( P );
    found := ListWithIdenticalEntries( lenP, false ); 
    i := 0; 
    classes := [ ];
    while ( i < lenP ) do 
        i := i+1; 
        while ( ( i <= lenP ) and found[i] ) do 
            i := i + 1; 
        od;
        if ( i <= lenP ) then 
            L := [ i ]; 
            found[i] := true;
            for j in [i+1..lenP] do 
                if not found[j] then 
                    iso := IsomorphismGroupoids( P[i], P[j] ); 
                    if not ( iso = fail ) then 
                        Add( L, j );
                        found[j] := true;
                        fi;
                    fi;
                od;
            Add( classes, L );                     
        fi;
    od; 
    return classes; 
end );

#############################################################################
##
#M  AutomorphismGroupoidOfGroupoid
##
InstallMethod( AutomorphismGroupoidOfGroupoid, "for a single piece groupoid", 
    true, [ IsGroupoid and IsSinglePieceDomain ], 0,
function( gpd ) 

    local obs, A; 

    A := AutomorphismGroupOfGroupoid( gpd ); 
    obs := ObjectList( gpd );
    return MagmaWithSingleObject( A, obs ); 
end );

InstallMethod( AutomorphismGroupoidOfGroupoid, "for a homogeneous groupoid", 
    true, [ IsGroupoid and IsHomogeneousDomainWithObjects ], 0,
function( gpd ) 

    local pieces, obs, n, p1, ap1, rays, aut, niceob, nicemap;  

    Info( InfoGroupoids, 2, 
          "AutomorphismGroupoidOfGroupoid for homogeneous groupoids" ); 
    pieces := Pieces( gpd ); 
    obs := List( pieces, p -> p!.objects ); 
    n := Length( pieces );
    p1 := pieces[1]; 
    ap1 := AutomorphismGroupOfGroupoid( p1 ); 
    rays := Concatenation( [ One( ap1 ) ], 
            List( [1..n-1], i -> IsomorphismNewObjects( p1, obs[i+1] ) ) ); 
    aut := SinglePieceGroupoidWithRays( ap1, obs, rays ); 
    if HasName( gpd ) then 
        SetName( aut, Concatenation( "Aut(", Name( gpd ), ")" ) ); 
    fi;
    SetAutomorphismDomain( aut, gpd );
    return aut; 
end );

InstallMethod( AutomorphismGroupoidOfGroupoid, "for a groupoid", true, 
    [ IsGroupoid ], 0,
function( gpd ) 

    local pieces, cpos, numc, obs, comp, i, lenc, pos, pi, auti, geni, 
          lenpi, idp, isos, obsi, j, pj, isoj, invj, genj, autj, isosj; 

    pieces := Pieces( gpd ); 
    cpos := IsomorphismClassPositionsOfGroupoid( gpd ); 
    numc := Length( cpos );
    obs := List( pieces, p -> p!.objects ); 
    obs := List( cpos, K -> obs{K} ); 
    obs := List( obs, K -> Set( Flat( K ) ) ); 
    comp := ListWithIdenticalEntries( numc, 0 );
    for i in [1..numc] do 
        pos := cpos[i]; 
        lenc := Length( pos ); 
        pi := pieces[ pos[1] ]; 
        auti := AutomorphismGroupOfGroupoid( pi ); 
        if HasName( pi ) then 
            SetName( auti, Concatenation( "Aut(", Name(pi), ")" ) ); 
        fi; 
        geni := GeneratorsOfGroup( auti ); 
        lenpi := Length( pi!.objects ); 
        if ( lenc = 1 ) then 
            comp[i] := MagmaWithSingleObject( auti, pi!.objects ); 
        else 
            obsi := List( pos, k -> pieces[k]!.objects ); 
            isos := ListWithIdenticalEntries( lenpi, 0 ); 
            isos[1] := IdentityMapping( auti ); 
            for j in [2..lenc] do 
                pj := pieces[ pos[j] ]; 
                isoj := IsomorphismGroupoids( pi, pj ); 
                invj := InverseGeneralMapping( isoj ); 
                genj := List( geni, g -> invj * g * isoj ); 
                autj := Group( genj ); 
                SetAutomorphismGroupOfGroupoid( pj, autj ); 
                if HasName( pj ) then 
                    SetName( autj, Concatenation( "Aut(", Name(pj), ")" ) ); 
                fi; 
                isosj := GroupHomomorphismByImagesNC(auti,autj,geni,genj); 
                SetIsInjective( isosj, true );
                SetIsSurjective( isosj, true ); 
                isos[j] := isosj; 
            od; 
            comp[i] := GroupoidByIsomorphisms( auti, obsi, isos );
        fi;
    od; 
    return UnionOfPieces( comp ); 
end );

############################# GROUPOID ACTIONS ##############################

#############################################################################
##
#M  GroupoidActionByConjugation                        sets up the action map
##
InstallMethod( GroupoidActionByConjugation, "method for a groupoid", true,
    [ IsGroupoid and IsSinglePieceDomain ], 0,
function( gpd )

    local aut, map, act;

    aut := AutomorphismGroupOfGroupoid( gpd );
    map := function(a) return GroupoidInnerAutomorphism( gpd, a ); end;
    act := rec(); 
    ObjectifyWithAttributes( act, GroupoidActionType, 
        Source, gpd,
        Range, aut,
        ActionMap, map,
        IsGroupoidAction, true );
    return act;
end );

InstallOtherMethod( GroupoidActionByConjugation, 
    "method for a groupoid and a normal subgroupoid", true,
    [ IsGroupoid and IsSinglePieceDomain, IsGroupoid ], 0,
function( gpd, sub )

    local issing, ishomd, aut, map, act;

    issing := false;
    ishomd := false;
    if not IsWideSubgroupoid( gpd, sub ) then 
        Error( "sub is not a subgroupoid of gpd" ); 
    fi;
    if IsSinglePiece( sub ) then
        issing := true;
    elif IsHomogeneousDomainWithObjects( sub ) then
        ishomd := true;
    else
        Error( "invalid subgroupoid sub" );
    fi;
    aut := AutomorphismGroupOfGroupoid( sub );
    map := function(a) 
               return GroupoidInnerAutomorphismNormalSubgroupoid(gpd,sub,a);
           end;
    act := rec(); 
    ObjectifyWithAttributes( act, GroupoidActionType, 
        Source, gpd,
        Range, aut,
        ActionMap, map,
        IsGroupoidAction, true );
    return act;
end );
