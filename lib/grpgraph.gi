############################################################################
##
#W  grpgraph.gi             GAP4 package `groupoids'           Chris Wensley
#W                                                              & Emma Moore
##  
##  This file contains generic methods for FpWeightedDigraphs and 
##  FpWeightedDigraphs of groups 

#############################################################################
##
#M  ArcsIsosFromMatrices
##
InstallMethod( ArcsIsosFromMatrices, "generic method for a digraph",
    true, [ IsHomogeneousList, IsHomogeneousList, IsHomogeneousList ], 0,
function( V, A, H ) 

    local n, i, j, arcs, isos; 

    n := Length( A ); 
    if not ForAll( A, L -> ( IsList(L) and ( Length(L) = n ) ) ) then 
        Error( "homogeneous list is not a matrix" ); 
    fi; 
    arcs := [ ]; 
    isos := [ ]; 
    for i in [1..n] do 
        for j in [1..n] do 
            if ( A[i][j] <> 0 ) then 
                Add( arcs, [ A[i][j], V[i], V[j] ] ); 
                Add( isos, H[i][j] );  
            fi; 
        od; 
    od; 
    return [ arcs, isos ]; 
end ); 

#############################################################################
##
#M  FpWeightedDigraphNC                                               
##
InstallMethod( FpWeightedDigraphNC, "generic method for a digraph",
    true, [ IsGroup, IsHomogeneousList, IsHomogeneousList ], 0,
function( gp, v, e )

    local dig;
     
    dig := Objectify( IsFpWeightedDigraphType, rec () );
    dig!.group := gp; 
    dig!.vertices := v;
    dig!.arcs := e; 
    SetIsFpWeightedDigraph( dig, true );
    return dig; 
end );

#############################################################################
##
#M  FpWeightedDigraph                                             
##
InstallMethod( FpWeightedDigraph, "generic method for a FpWeightedDigraph",
    true, [ IsGroup, IsHomogeneousList, IsHomogeneousList ], 0,
function( gp, v, e )

    local lenv, lene, i, ie, ij, inve;

    lenv := Length( v );
    lene := Length( e ); 
   
    ## check that each element of e is a triple
    ## and that source vertex is in vertex list
    ## and that target vertex is in vertex list

    for i in [1..lene] do 
        if ( Length(e[i]) <> 3 ) then
            Error("edge list not correct format \n");
        fi;
        if not ( e[i][1] in gp ) then 
            Error("source vertex not in vertex list \n");
        fi;
        if not ( e[i][2] in v ) then 
            Error("source vertex not in vertex list \n");
        fi;
        if not ( e[i][3] in v ) then
            Error("target vertex not in vertex list \n");
        fi;
    od;
    return FpWeightedDigraphNC( gp, v, e );
end);

#############################################################################
##
#M  FpWeightedAdjacencyMatrix                                              
##
InstallMethod( FpWeightedAdjacencyMatrix, 
    "generic method for FpWeightedDigraph", true, [ IsFpWeightedDigraph ], 0,
function( dig )

    local verts, n, mat, arcs, a, u, posu, v, posv; 

    verts := dig!.vertices;
    n := Length( verts ); 
    mat := NullMat( n, n );
    arcs := dig!.arcs; 
    for a in arcs do 
        u := a[2]; 
        posu := Position( verts, u );
        v := a[3]; 
        posv := Position( verts, v ); 
        mat[posu][posv] := a[1]; 
    od; 
    return mat; 
end ); 

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . . . . . . . . for a weighted digraph 
##
InstallMethod( String, "for a weighted digraph", true, 
    [ IsFpWeightedDigraph ], 0, 
function( gg ) 
    return( STRINGIFY( "graph of groupoids" ) ); 
end );

InstallMethod( ViewString, "for a weighted digraph", true, 
    [ IsFpWeightedDigraph ], 0, String ); 

InstallMethod( PrintString, "for a weighted digraph", true, 
    [ IsFpWeightedDigraph ], 0, String ); 

InstallMethod( ViewObj, "for a weighted digraph", true, 
    [ IsFpWeightedDigraph ], 0, PrintObj ); 

InstallMethod( PrintObj, "method for a weighted digraph",
    [ IsFpWeightedDigraph ],
function( dig )
    if HasName( dig ) then
        Print( Name( dig ), "\n" );
    else
        Print("weighted digraph with vertices: ", dig!.vertices, "\n");
        Print("and arcs: ", dig!.arcs, "\n" );
    fi;
end );

InstallMethod( ViewObj, "method for a weighted digraph",
    [ IsFpWeightedDigraph ],
function( dig )
    if HasName( dig ) then
        Print( Name( dig ), "\n" );
    else
        Print("weighted digraph with vertices: ", dig!.vertices, "\n");
        Print("and arcs: ", dig!.arcs );
    fi;
end );

#############################################################################
##
#M  InvolutoryArcs
##
InstallMethod( InvolutoryArcs, "generic method for a digraph",
    true, [ IsFpWeightedDigraph ], 0,
function( dig )

    local vdig, adig, a, lenv, lena, i, ia, ij, inva, pos, inv;

    vdig := dig!.vertices;
    adig := dig!.arcs;
    lenv := Length( vdig );
    lena := Length( adig ); 
    ia := ListWithIdenticalEntries( lena, 0 );
    ij := ListWithIdenticalEntries( lena, 0 );
    inv := ListWithIdenticalEntries( lena, 0 );
    for i in [1..lena] do
        a := adig[i];
        ia[i] := a[1];
        ij[i] := InverseOp( ia[i] );
        inva := [ ij[i], a[3], a[2] ];
	pos := Position( adig, inva );
	if ( pos = fail ) then
            Info( InfoGroupoids, 1, "involutory arc not available" );
	    return fail;
	else
	    inv[i] := pos ;
	fi;
    od;
    return inv;
end);

## ------------------------------------------------------------------------##
##                          Graphs of Groups                               ##
## ------------------------------------------------------------------------##
 
#############################################################################
##
#M  GraphOfGroupsNC                                               
##
InstallMethod( GraphOfGroupsNC, "generic method for a digraph of groups",
    true, [ IsFpWeightedDigraph, IsList, IsList ], 0,
function( dig, gps, isos )

    local gg, ok; 
     
    gg := rec(); 
    ObjectifyWithAttributes( gg, IsGraphOfGroupsType, 
        DigraphOfGraphOfGroups, dig, 
        GroupsOfGraphOfGroups, gps, 
        IsomorphismsOfGraphOfGroups, isos );
    if ForAll( gps, IsPermGroup ) then
        SetIsGraphOfPermGroups( gg, true );
    elif ForAll( gps, IsFpGroup ) then
        SetIsGraphOfFpGroups( gg, true );
    fi;
    return gg; 
end );

#############################################################################
##
#M  GraphOfGroups                                             
##
InstallMethod( GraphOfGroups, "generic method for a digraph of  groups",
    true, [ IsFpWeightedDigraph, IsList, IsList ], 0,
function( dig, gps, isos )

    local g, i, v, e, lenV, lenE, tgtL, pos, inv, einvpos;

    v := dig!.vertices;
    e := dig!.arcs; 
    lenV:= Length(v); 
    lenE:= Length(e);
    
    # checking that list sizes are compatible
    if ( (lenV <> Length(gps)) 
         or (lenE <> Length(isos)) ) then
        Error( "list sizes are not compatible for assignments" );
    fi; 
    # checking that we have groups
    # lenV and length of groups are equal 
    for i in [1..lenV] do
        if ( IsGroup( gps[i] ) = false ) then
            Error( "groups are needed");
        fi;
    od;
    # checking that isomorphisms are isos and form correct groups.
    inv := InvolutoryArcs(dig);
    for i in [1..lenE] do
    #?  THIS LINE DOES NOT MAKE SENSE :-
    #?         einvpos := Position( e, e[inv[i]] );
        for g in GeneratorsOfGroup( Source( isos[i] ) ) do
            if not ( ImageElm( isos[inv[i]], ImageElm(isos[i],g) ) = g ) then
                Error( "isos are not correct");
            fi;
        od;
    od;
    return GraphOfGroupsNC( dig, gps, isos );
end );

##############################################################################
##
#M  \=( <gg1>, <gg2> ) . . . . . . . . . test if two graph of groups are equal
##
InstallOtherMethod( \=,
    "generic method for two graphs of groups",
    IsIdenticalObj, [ IsGraphOfGroups, IsGraphOfGroups ], 0,
function ( gg1, gg2 )
return ( ( DigraphOfGraphOfGroups(gg1) = DigraphOfGraphOfGroups(gg2) )
     and ( GroupsOfGraphOfGroups(gg1) = GroupsOfGraphOfGroups(gg2) )
     and ( IsomorphismsOfGraphOfGroups(gg1) = IsomorphismsOfGraphOfGroups(gg2))
     );
end );

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . . . . . . . . . for a graph of groups 
##
InstallMethod( String, "for a graph of groups", true, [ IsGraphOfGroups ], 0, 
function( gg ) 
    return( STRINGIFY( "graph of groups" ) ); 
end );

InstallMethod( ViewString, "for a graph of groups", true, 
    [ IsGraphOfGroups ], 0, String ); 

InstallMethod( PrintString, "for a graph of groups", true, 
    [ IsGraphOfGroups ], 0, String ); 

InstallMethod( ViewObj, "for a graph of groups", true, 
    [ IsGraphOfGroups ], 0, PrintObj ); 

InstallMethod( PrintObj, "method for a graph of groups", [ IsGraphOfGroups ],
function( gg )
    
    local dig;

    dig := DigraphOfGraphOfGroups( gg );
    Print( "Graph of Groups: " );
    Print( Length( dig!.vertices ), " vertices; " );
    Print( Length( dig!.arcs ), " arcs; " );
    Print( "groups ", GroupsOfGraphOfGroups( gg ) );
end );

InstallMethod( ViewObj, "method for a graph of groups", [ IsGraphOfGroups ],
function( gg )
    
    local dig;

    dig := DigraphOfGraphOfGroups( gg );
    Print( "Graph of Groups: " );
    Print( Length( dig!.vertices ), " vertices; " );
    Print( Length( dig!.arcs ), " arcs; " );
    Print( "groups ", GroupsOfGraphOfGroups( gg ) );
end );

##############################################################################
##
#M  Display( <gg> ) . . . . . . . . . . . . . . . . . . view a graph of groups
##
InstallMethod( Display, "method for a graph of groups", [ IsGraphOfGroups ],
function( gg )
    
    local dig;

    dig := DigraphOfGraphOfGroups( gg );
    Print( "Graph of Groups with :- \n" );
    Print( "    vertices: ", dig!.vertices, "\n" );
    Print( "        arcs: ", dig!.arcs, "\n" );
    Print( "      groups: ", GroupsOfGraphOfGroups( gg ), "\n" );
    Print( "isomorphisms: ", List( IsomorphismsOfGraphOfGroups( gg ), 
                                   MappingGeneratorsImages ), "\n" );
end );

##############################################################################
##
#M  IsGraphOfPermGroups( <gg> ) . . . . . . . . . . . .  for a graph of groups
#M  IsGraphOfFpGroups( <gg> ) . . . . . . . . . . . . .  for a graph of groups
#M  IsGraphOfPcGroups( <gg> ) . . . . . . . . . . . . .  for a graph of groups
##
InstallMethod( IsGraphOfPermGroups, "generic method", [ IsGraphOfGroups ],
function( gg )
    return ForAll( GroupsOfGraphOfGroups( gg ), IsPermGroup );
end );

InstallMethod( IsGraphOfFpGroups, "generic method", [ IsGraphOfGroups ],
function( gg )
    return ForAll( GroupsOfGraphOfGroups( gg ), IsFpGroup );
end );

InstallMethod( IsGraphOfPcGroups, "generic method", [ IsGraphOfGroups ],
function( gg )
    return ForAll( GroupsOfGraphOfGroups( gg ), IsPcGroup );
end );

#############################################################################
##
#M  RightTransversalsOfGraphOfGroups
##
InstallMethod( RightTransversalsOfGraphOfGroups, 
    "generic method for a group graph", true, [ IsGraphOfGroups ], 0,
function( gg )

    local gps, dig, isos, adig, vdig, len, i, g, rc, rep, trans;

    gps := GroupsOfGraphOfGroups( gg );
    dig := DigraphOfGraphOfGroups( gg );
    isos := IsomorphismsOfGraphOfGroups( gg );
    vdig := dig!.vertices;
    adig := dig!.arcs;
    len := Length( adig );
    trans := ListWithIdenticalEntries( len, 0 );
    for i in [1..len] do
        g := gps[ Position( vdig, adig[i][2] ) ];
        rc := RightCosets( g, Source( isos[i] ) );
        rep := List( rc, Representative );
        if IsGraphOfFpGroups( gg ) then
            trans[i] := List( rep, r -> NormalFormKBRWS( g, r ) ); 
        elif IsGraphOfPermGroups( gg ) then
            trans[i] := ShallowCopy( rep );
        else
            Error( "not yet implemented" );
        fi;
    od;
    return trans;
end );

#############################################################################
##
#M  LeftTransversalsOfGraphOfGroups
##
InstallMethod( LeftTransversalsOfGraphOfGroups, 
    "generic method for a group graph", true, [ IsGraphOfGroups ], 0,
function( gg )

    local gps, dig, vdig, adig, len, i, trans, itran, g; 

    gps := GroupsOfGraphOfGroups( gg );
    dig := DigraphOfGraphOfGroups( gg );
    vdig := dig!.vertices;
    adig := dig!.arcs;
    len := Length( adig );
    trans := RightTransversalsOfGraphOfGroups( gg );
    itran := ListWithIdenticalEntries( len, 0 );
    for i in [1..len] do
        g := gps[ Position( vdig, adig[i][2] ) ];
        if IsGraphOfFpGroups( gg ) then
            itran[i] := List( trans[i], t -> NormalFormKBRWS( g, t^-1 ) ); 
        elif IsGraphOfPermGroups( gg ) then
            itran[i] := List( trans[i], t -> t^-1 );
        fi;
    od;
    return itran;
end);

## ------------------------------------------------------------------------##
##                      Graph of Groups Words                              ##
## ------------------------------------------------------------------------##
 
#############################################################################
##
#M  GraphOfGroupsWordNC 
##
InstallMethod( GraphOfGroupsWordNC, "generic method for a word",
    true, [ IsGraphOfGroups, IsInt, IsList ], 0,
function( gg, tv, wL )

    local ggword;
    
    ggword := rec(); 
    ObjectifyWithAttributes( ggword, IsGraphOfGroupsWordType, 
        GraphOfGroupsOfWord, gg, 
        TailOfGraphOfGroupsWord, tv, 
        WordOfGraphOfGroupsWord, wL, 
        IsGraphOfGroupsWord, true ); 
    if ( Length( wL ) = 1 ) then
        SetHeadOfGraphOfGroupsWord( ggword, tv );
    fi;
    return ggword; 
end );

#############################################################################
##
#M  GraphOfGroupsWord 
##
InstallMethod( GraphOfGroupsWord, "for word in graph of groups",
    true, [ IsGraphOfGroups, IsInt, IsList ], 0,
function( gg, tv, wL )

    local gps, dig, adig, enum, vdig, n, i, j, g, v, posv, e, w;

    gps := GroupsOfGraphOfGroups( gg );
    dig := DigraphOfGraphOfGroups( gg );
    vdig := dig!.vertices;
    adig := dig!.arcs;
    enum := Length( adig );
    v := tv;
    posv := Position( vdig, v );
    w := wL[1];
    if not ( w in gps[posv] ) then
        Error( "first group element not in tail group" );
    fi;
    j := 1;
    n := ( Length( wL ) - 1 )/2;
    for i in [1..n] do
        e := wL[j+1];
        if ( e > enum ) then
            Error( "entry ", j+1, " in wL not an edge" );
        else
            e := adig[e];
        fi;
        v := e[3];
        posv := Position( vdig, v );
        g := gps[ posv ];
        j := j+2;
        w := wL[j];
        if not ( w in g ) then
            Error( "entry ", j, " not in group at vertex", v );
        fi;
    od;    
    return GraphOfGroupsWordNC( gg, tv, wL );
end);

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . . . . . . for a graph of groups word 
##
InstallMethod( String, "for a graph of groups word", true, 
    [ IsGraphOfGroupsWord ], 0, 
function( ggword ) 
    return( STRINGIFY( "graph of groups word" ) ); 
end );

InstallMethod( ViewString, "for a graph of groups word", true, 
    [ IsGraphOfGroupsWord ], 0, String ); 

InstallMethod( PrintString, "for a graph of groups word", true, 
    [ IsGraphOfGroupsWord ], 0, String ); 

InstallMethod( ViewObj, "method for a graph of groups word", 
    [ IsGraphOfGroupsWord ],
function( ggword )
    local w, i, gg, adig;

    gg := GraphOfGroupsOfWord( ggword );
    adig := DigraphOfGraphOfGroups( gg )!.arcs;
    w := WordOfGraphOfGroupsWord( ggword );
    Print( "(", TailOfGraphOfGroupsWord( ggword ), ")", w[1] );
    i := 1;
    while ( i < Length(w) ) do
        i := i+2;
        Print( ".", adig[w[i-1]][1], ".", w[i] );
    od;
    Print( "(", HeadOfGraphOfGroupsWord( ggword ), ")" );
end );

InstallMethod( PrintObj, "method for a graph of groups word", 
    [ IsGraphOfGroupsWord ],
function( ggword )

    local w, i, gg, adig;

    gg := GraphOfGroupsOfWord( ggword );
    adig := DigraphOfGraphOfGroups( gg )!.arcs;
    w := WordOfGraphOfGroupsWord( ggword );
    Print( "(", TailOfGraphOfGroupsWord( ggword ), ")", w[1] );
    i := 1;
    while ( i < Length(w) ) do
        i := i+2;
        Print( ".", adig[w[i-1]][1], ".", w[i] );
    od;
    Print( "(", HeadOfGraphOfGroupsWord( ggword ), ")" );
end );

#############################################################################
##
#M  HeadOfGraphOfGroupsWord                                             
##
InstallMethod( HeadOfGraphOfGroupsWord, "generic method for a graph of groups word",
    true, [ IsGraphOfGroupsWordRep ], 0,
function( ggword )

    local w, gg, e;

    w := WordOfGraphOfGroupsWord( ggword ); 
    gg := GraphOfGroupsOfWord( ggword );
    e := w[Length(w)-1];
    return DigraphOfGraphOfGroups( gg )!.arcs[e][3];
end );

#############################################################################
##
#M  IsReducedGraphOfGroupsWord 
##
##  NO LONGER NEEDED  ??  INCORRECT ??
##
InstallMethod( IsReducedGraphOfGroupsWord, "for word in graph of groups",
    true, [ IsGraphOfGroupsWord ], 0,
function( ggw )

    local w, len, gg, dig, gps, adig, vdig, pos, i, e, ie, t, v, g;

    w := WordOfGraphOfGroupsWord( ggw );
    len := Length( w );
    gg := GraphOfGroupsOfWord( ggw );
    dig := DigraphOfGraphOfGroups( gg );
    gps := GroupsOfGraphOfGroups( gg );
    vdig := dig!.vertices;
    adig := dig!.arcs;
    if ( len = 1 ) then
        pos := Position( vdig, TailOfGraphOfGroupsWord( ggw ) );
        return ( w[1] = NormalFormKBRWS( gps[pos], w[1] ) );
    fi;
    i := 1;
    while ( i < len ) do
        e := w[i+1];
        ie := InvolutoryArcs( dig )[e];
        t := RightTransversalsOfGraphOfGroups( gg )[ie];
        if not ( w[i] in t ) then
            return false;
        fi;
        if ( ( i > 1 ) and ( w[i-1] = ie ) ) then
            v := adig[e][2];
            g := gps[ Position( vdig, v ) ];
            if ( w[i] = One( g ) ) then
                return false;
            fi;
        fi;
        i := i+2;
    od;
    pos := Position( vdig, adig[e][3] );
    return ( w[i] = NormalFormKBRWS( gps[pos], w[i] ) );
end);

#############################################################################
##
#M  ReducedGraphOfGroupsWord  
##
InstallMethod( ReducedGraphOfGroupsWord, "for word in graph of groups",
    true, [ IsGraphOfGroupsWordRep ], 0,
function( ggword )

    local w, tw, hw, gg, gps, isos, dig, adig, vdig, lw, len, k, k2, 
          he, rtrans, tran, pos, a, g, h, found, i, nwit, tsp,
          im, sub, u, v, gu, gv, e, ie, ng, rw, lenred, isfp, isid;

    if ( HasIsReducedGraphOfGroupsWord( ggword ) 
         and IsReducedGraphOfGroupsWord( ggword ) ) then
        return ggword;
    fi;
    w := ShallowCopy( WordOfGraphOfGroupsWord( ggword ) );
    tw := TailOfGraphOfGroupsWord( ggword );
    hw := HeadOfGraphOfGroupsWord( ggword );
    lw := Length( w );
    len := (lw-1)/2;
    gg := GraphOfGroupsOfWord( ggword );
    gps := GroupsOfGraphOfGroups( gg );
    isos := IsomorphismsOfGraphOfGroups( gg );
    isfp := IsGraphOfFpGroups( gg );
    dig := DigraphOfGraphOfGroups( gg );
    vdig := dig!.vertices;
    adig := dig!.arcs;
    rtrans := RightTransversalsOfGraphOfGroups( gg );
    if ( len = 0 ) then
        if isfp then 
            ng := NormalFormKBRWS( gps[1], w[1] ); 
        else
            ng := w[1];
        fi;
        return GraphOfGroupsWordNC( gg, tw, [ng] );
    fi;
    k := 1;
    v := tw;
    Info( InfoGroupoids, 3, "initial w = ", w );
    Info( InfoGroupoids, 3, "--------------------------------------------" );
    while ( k <= len ) do
        k2 := k+k;
        ## reduce the subword  w{[k2-1..k2+1]}
        Info( InfoGroupoids, 3, "w{[k2-1..k2+1]} = ", w{[k2-1..k2+1]} );
        e := w[k2];
        he := Position( vdig, adig[e][3] );
        ## factorise group element as pair [ transversal, subgroup element] 
        tran := rtrans[e];
        a := adig[e];
        pos := Position( vdig, a[2] );
        g := gps[pos];
        h := Source( isos[e] );
        found := false;
        i := 0;
        while not found do
            i := i+1;
            nwit := tran[i]*w[k2-1];
            found := ( nwit in h );
        od;
        tsp := [ tran[i]^(-1), nwit ];
        Info( InfoGroupoids, 3, "tsp at i = ", i, " is ", tsp );
        im := ImageElm( isos[e], tsp[2] );
        w[k2-1] := tsp[1];
        if isfp then
            w[k2+1] := NormalFormKBRWS( gps[he], im*w[k2+1] );
            Info( InfoGroupoids, 3, "k = ", k, ", w = ", w );
        else
            w[k2+1] := im*w[k2+1];
        fi;
        Info( InfoGroupoids, 3, "w{[k2-1..k2+1]} = ", w{[k2-1..k2+1]} );
        Info( InfoGroupoids, 3, "------------------------------------------" );
        lenred := ( k > 1 );
        while lenred do
            ## test for a length reduction
            e := w[k2];
            ie := InvolutoryArcs( dig )[e];
            v := adig[e][2];
            gv := gps[ Position( vdig, v ) ];
            if isfp then
                isid := ( ( Length( w[k2-1]![1] ) = 0 ) and ( w[k2-2] = ie ) );
            else
                isid := ( ( w[k2-1] = ( ) ) and ( w[k2-2] = ie ) );
            fi;
            if isid then
                ### perform a length reduction ###
                u := adig[e][3];
                gu := gps[ Position( vdig, u ) ];
                if isfp then
                    ng := NormalFormKBRWS( gu, w[k2-3]*w[k2+1] );
                else
                    ng := w[k2-3]*w[k2+1];
                fi;
                w := Concatenation( w{[1..k2-4]}, [ng], w{[k2+2..lw]} );
                len := len - 2;
                lw := lw - 4;
                Info( InfoGroupoids, 1, "k = ", k, ", shorter w = ", w );
                if ( len = 0 ) then
                     rw := GraphOfGroupsWordNC( gg, u, w );
                     SetTailOfGraphOfGroupsWord( rw, u );
                     SetHeadOfGraphOfGroupsWord( rw, u );
                     return rw;
                else
                     k := k-2;
                     k2 := k2-4;
                     lenred := ( k > 1 );
                fi;
            else
                lenred := false;
            fi;
        od;
        k := k+1;
    od;
    ## put final group element in normal form
    e := w[lw-1];
    u := adig[e][3];
    gu := gps[ Position( vdig, u ) ];
    if isfp then
        w[lw] := NormalFormKBRWS( gu, w[lw] );
    fi;
    rw := GraphOfGroupsWordNC( gg, tw, w );
    SetTailOfGraphOfGroupsWord( rw, tw );
    SetHeadOfGraphOfGroupsWord( rw, hw );
    SetIsReducedGraphOfGroupsWord( rw, true );
    return rw;
end);

#############################################################################
##
#M  IsMappingToGroupWithGGRWS( <map> )  
##
InstallMethod( IsMappingToGroupWithGGRWS, "for a mapping", true, 
    [ IsGroupGeneralMappingByImages ], 0,
function( map ) 
    return HasGraphOfGroupsRewritingSystem( Range( map ) ); 
end ); 

#############################################################################
##
#M  ReducedImageElm( <hom>, <elm> )  
##
InstallMethod( ReducedImageElm, "for word in graph of groups", true, 
    [ IsMappingToGroupWithGGRWS, 
      IsMultiplicativeElementWithInverse ], 0,
function( hom, elm )

    local rng, im, rim;  

    rng := Range( hom ); 
    im := ImageElm( hom, elm ); 
    rim := NormalFormGGRWS( rng, im );
    return ElementOfFpGroup( FamilyObj( rng.1 ), rim ); 
end );

##############################################################################
##
#M  \=( <ggw1>, <ggw2> ) . . . . . test if two graph of group words are equal
##
InstallOtherMethod( \=,
    "generic method for two graph of groups words",
    IsIdenticalObj, [ IsGraphOfGroupsWordRep, IsGraphOfGroupsWordRep ], 0,
function ( w1, w2 )
return ( ( GraphOfGroupsOfWord(w1) = GraphOfGroupsOfWord(w2) )
     and ( TailOfGraphOfGroupsWord( w1 ) = TailOfGraphOfGroupsWord( w2 ) )
     and ( WordOfGraphOfGroupsWord(w1) = WordOfGraphOfGroupsWord(w2) ) );
end );

##############################################################################
##
#M  ProductOp( <wordlist> ) . . . . . product of list of graph of groups words
##
InstallOtherMethod( ProductOp,
    "generic method for graph of groups words",
    true, [ IsHomogeneousList ], 0,
function ( ggwlist ) 

    local ggw1, ggw2, num, w1, w2, h1, len1, len2, w;

    num := Length( ggwlist );
    if not ( num = 2 ) then
        Error( "only works for two words at present" );
    fi;
    if not ForAll( [1..num], i -> IsGraphOfGroupsWordRep( ggwlist[i] ) ) then
        Error( "not a list of graph of groups words" );
    fi;
    ggw1 := ggwlist[1];
    ggw2 := ggwlist[2];
    if not ( HeadOfGraphOfGroupsWord( ggw1 ) 
             = TailOfGraphOfGroupsWord( ggw2 ) ) then
        Info( InfoGroupoids, 1, 
              "Head <> Tail for GraphOfGroupsWord(ggw1), so no composite" );
        return fail;
    fi;
    w1 := WordOfGraphOfGroupsWord( ggw1 );
    w2 := WordOfGraphOfGroupsWord( ggw2 );
    len1 := Length( w1 );
    len2 := Length( w2 );
    w := Concatenation( w1{[1..len1-1]}, [w1[len1]*w2[1]], w2{[2..len2]} );
    return GraphOfGroupsWord( GraphOfGroupsOfWord(ggw1), 
                              TailOfGraphOfGroupsWord(ggw1), w );
end );

##############################################################################
##
#M  \*( <ggw1>, <ggw2> ) . . . . . . . . product of two graph of groups words
##
InstallOtherMethod( \*, "generic method for two graph of groups words",
    IsIdenticalObj, [ IsGraphOfGroupsWordRep, IsGraphOfGroupsWordRep ], 0,
function ( ggw1, ggw2 ) 

    local ggw12;

    ggw12 := ProductOp( [ ggw1, ggw2 ] );
    if ( ggw12 = fail ) then
        return fail;
    else
        return ReducedGraphOfGroupsWord( ggw12 );
    fi;
end );

##############################################################################
##
#M  InverseOp( <ggword> ) . . . . . . . . . inverse of a graph of groups word
##
InstallOtherMethod( InverseOp, "generic method for a graph of groups word",
    true, [ IsGraphOfGroupsWordRep ], 0,
function ( ggw ) 

    local gg, ie, i, j, w, len, iw, iggw;

    w := WordOfGraphOfGroupsWord( ggw ); 
    gg := GraphOfGroupsOfWord( ggw );
    ie := InvolutoryArcs( DigraphOfGraphOfGroups( gg ) );
    len := Length( w );
    iw := ShallowCopy( w );
    i := 1;
    j := len;
    iw[1] := w[len]^-1;
    while ( i < len ) do
        iw[i+1] := ie[ w[j-1] ];
        i := i+2;
        j := j-2;
        iw[i] := w[j]^(-1);
    od;
    iggw := GraphOfGroupsWord( gg, HeadOfGraphOfGroupsWord( ggw ), iw );
    SetHeadOfGraphOfGroupsWord( iggw, TailOfGraphOfGroupsWord( ggw ) );
    if IsReducedGraphOfGroupsWord( ggw ) then
        iggw := ReducedGraphOfGroupsWord( iggw );
    fi;
    return iggw;
end );

##############################################################################
##
#M  \^( <ggw>, <n> ) . . . . . . . . . . . . . power of a graph of groups word
##
InstallOtherMethod( \^,
    "generic method for n-th power of a graph of groups word",
    true, [ IsGraphOfGroupsWordRep, IsInt ], 0,
function ( ggw, n ) 

    local w, tv, ptv, gg, g, k, iggw, ggwn;

    if ( n = 1 ) then
        return ggw;
    elif ( n = -1 ) then
        return InverseOp( ggw );
    fi;
    if not ( HeadOfGraphOfGroupsWord(ggw) = TailOfGraphOfGroupsWord(ggw) ) then
        Info( InfoGroupoids, 1, 
              "Head <> Tail for GraphOfGroupsWord(ggw), so no composite" );
        return fail;
    fi;
    if ( n = 0 ) then
        tv := TailOfGraphOfGroupsWord( ggw );
        gg := GraphOfGroupsOfWord( ggw );
        ptv := Position( DigraphOfGraphOfGroups(gg)!.vertices, tv );
        g := GroupsOfGraphOfGroups( gg )[ptv];
        return GraphOfGroupsWord( gg, tv, [ One(g) ] ); 
    elif ( n > 1 ) then
        ggwn := ggw;
        for k in [2..n] do
            ggwn := ggwn * ggw;
        od;
    elif ( n < -1 ) then
        iggw := InverseOp( ggw );
        ggwn := iggw;
        for k in [2..-n] do
            ggwn := ggwn * iggw;
        od;
    fi;
    return ggwn;
end );

## ------------------------------------------------------------------------##
##                          Rewriting Functions                            ##
## ------------------------------------------------------------------------##
 
#############################################################################
##
#M  FreeSemigroupOfKnuthBendixRewritingSystem(<KB RWS>)
##
##
InstallMethod(FreeSemigroupOfKnuthBendixRewritingSystem, "<KB RWS>", true,
    [IsKnuthBendixRewritingSystem], 0,
function(kbrws)
    return FreeSemigroupOfFpSemigroup( SemigroupOfRewritingSystem( kbrws ) );
end);

#############################################################################
##
#M  NormalFormKBRWS                                             
##
InstallMethod( NormalFormKBRWS, "generic method for normal form",
    true, [ IsFpGroup, IsObject ], 0,
function( gp, w0 )

    local iso, smg, rwsmg, smggen, fsmg, iw, uiw, ruw, fam1, riw, inviso, rw;

    if not ( w0 in gp ) then
        Error( "word not in the group" );
    fi;
    iso := IsomorphismFpSemigroup( gp );
    inviso := InverseGeneralMapping( iso ); 
    smg := Range( iso );
    rwsmg := KnuthBendixRewritingSystem( smg );
    MakeConfluent( rwsmg );  ### this should not be necessary here !! ###
    smggen := GeneratorsOfSemigroup( smg );
    fsmg := FreeSemigroupOfKnuthBendixRewritingSystem( rwsmg );
    iw := ImageElm( iso, w0 );
    uiw := UnderlyingElement( iw );
    ruw := ReducedForm( rwsmg, uiw );
    fam1 := FamilyObj( smggen[1] );
    riw := ElementOfFpSemigroup( fam1, ruw );
    rw :=ImageElm( inviso, riw );
    return rw;
end);

###############################################################################
##
#F  FreeProductWithAmalgamation( group, group, isomorphism between subgroups ) 
##
InstallMethod( FreeProductWithAmalgamation, "for 2 groups and an isomorphism", 
    true, [ IsGroup, IsGroup, IsGroupHomomorphism ], 0, 
function( G, H, hom )

    local SG, SH; 

    ## Checks to verify the arguments 
    SG := Source( hom ); 
    SH := Range( hom ); 
    if not ( IsSubgroup( G, SG ) and IsSubgroup( H, SH ) ) then 
        Error( "source and range of hom not subgroups of G,H" ); 
    fi; 
    if not IsBijective( hom ) then 
        Error( "hom : SG -> SH not an isomorphism" ); 
    fi; 
    ## Delegate the construction to FreeProductWithAmalgamationOp
    return FreeProductWithAmalgamationOp( G, H, hom ); 
end );

############################################################################
##
#O  FreeProductWithAmalgamationOp( group, group, isomorphism ) 
##
InstallMethod( FreeProductWithAmalgamationOp, "for 2 groups and an isomorphism",
    true, [ IsGroup, IsGroup, IsGroupHomomorphism ], 0,
function( G, H, iso ) 

    local gG, gH,       # generating sets for G,H 
          ngG, ngH,     # lengths of these generating sets 
          Giso, Hiso,   # isomorphisms from G,H to fp-groups 
          fpG, fpH,     # images of G,H under these isomorphisms 
          fG, fH,       # free groups of fpG,fpH 
          gfG, gfH,     # generators of the groups fG,fH 
          gfpG, gfpH,   # generators of the groups fpG,fpH 
          SG, SH,       # subgroups of G,H which are isomorphic  
          fpSG, fpSH,   # images of SG,SH under Giso,Hiso 
          ngF,          # ngG + ngH = total number of free generators 
          F, gF,        # free group of the fpa and its generating set 
          gFG, gFH,     # subsets of gF given by the embeddings of G,H 
          rels, r,      # set of relators for the fpa; relations index 
          mgi,          # mapping generators images for iso 
          gSG, gSH,     # generating sets for SG,SH 
          gfpSG, gfpSH, # images of gSG,gSH under Giso,Hiso
          i,            # index of generator range
          g, h,         # elements in the two subgroups 
          wg, wh,       # mapped words for g,h 
          FPA, gFPA,    # the free product with amalgamation; its generators 
          gFPAG, gFPAH, # images for the two embeddings 
          eG, eH,       # embeddings of G,H in FPA 
          embeddings,   # monomorphisms of base groups into free product 
          rws;          # graph of groups rewriting system 
        
    ## no need to check the arguments - this is the NC version 
    ## create isomorphisms from the given groups list to fp-groups
    gG := SmallGeneratingSet( G );
    ngG := Length( gG ); 
    Giso := IsomorphismFpGroupByGenerators( G, gG ); 
    fpG := ImagesSource( Giso ); 
    gfpG := List( gG, g -> ImageElm( Giso, g ) ); 
    gH := SmallGeneratingSet( H );
    ngH := Length( gH ); 
    Hiso := IsomorphismFpGroupByGenerators( H, gH );
    fpH := Image( Hiso ); 
    gfpH := List( gH, h -> ImageElm( Hiso, h ) ); 
    fG := FreeGroupOfFpGroup( fpG ); 
    gfG := GeneratorsOfGroup( fG );
    fH := FreeGroupOfFpGroup( fpH ); 
    gfH := GeneratorsOfGroup( fH );
    ngF := ngG + ngH; 
    SG := Source( iso ); 
    SH := Range( iso ); 
    fpSG := Image( Giso, SG ); 
    fpSH := Image( Hiso, SH ); 
    ## Create the free group of the fpa
    F := FreeGroup( ngF );
    gF := GeneratorsOfGroup( F ); 
    gFG := gF{[1..ngG]}; 
    gFH := gF{[ngG+1..ngF]}; 
    ## create the G,H relations for the fpa 
    rels := [];
    for r in RelatorsOfFpGroup( fpG ) do 
        Add( rels, MappedWord( UnderlyingElement(r), gfG, gFG ) ); 
    od; 
    for r in RelatorsOfFpGroup( fpH ) do 
        Add( rels, MappedWord( UnderlyingElement(r), gfH, gFH ) ); 
    od; 
    ## create the subgroup relations 
    mgi := MappingGeneratorsImages( iso ); 
    gSG := mgi[1]; 
    gfpSG := List( gSG, g -> Image( Giso, g ) ); 
    gSH := mgi[2];
    gfpSH := List( gSH, h -> Image( Hiso, h ) ); 
    for i in [1..Length(gSG)] do 
        g := UnderlyingElement( gfpSG[i] ); 
        wg := MappedWord( g, gfG, gFG ); 
        h := UnderlyingElement( gfpSH[i] ); 
        wh := MappedWord( h, gfH, gFH ); 
        Add( rels, wg * wh^-1 ); 
    od;
    ## create the fpa
    FPA := F/rels; 
    gFPA := GeneratorsOfGroup( FPA ); 
    gFPAG := gFPA{[1..ngG]}; 
    gFPAH := gFPA{[ngG+1..ngF]}; 
    SetIsFreeProductWithAmalgamation( FPA, true ); 
    ## create the two embeddings into the fpa 
    eG := GroupHomomorphismByImagesNC( G, FPA, gG, gFPAG );
    SetIsInjective( eG, true ); 
    SetIsMappingToGroupWithGGRWS( eG, true ); 
    eH := GroupHomomorphismByImagesNC( H, FPA, gH, gFPAH );
    SetIsInjective( eH, true ); 
    SetIsMappingToGroupWithGGRWS( eH, true ); 
    embeddings := [ eG, eH ]; 
    ## Save the embedding information for possible use later.
    SetFreeProductWithAmalgamationInfo( FPA, 
        rec( embeddings := embeddings, 
             groups := [ G, H ], 
             isomorphism := iso, 
             positions := [ [1..ngG], [ngG+1..ngF] ], 
             subgroups := [ SG, SH ] ) );
    rws := GraphOfGroupsRewritingSystem( FPA );
    return FPA; 
end );

#############################################################################
##
#M  Embedding (for free product with amalgamation and hnn extension) 
##
InstallMethod( Embedding, "free product with amalgamation", true, 
    [ IsGroup and HasFreeProductWithAmalgamationInfo, IsPosInt ], 0,
    function( G, i )
        if i > Length(FreeProductWithAmalgamationInfo(G).embeddings) then
            Error("Base group with index ",i, " does not exist");
        else
            return FreeProductWithAmalgamationInfo(G).embeddings[i];
        fi;
    end
);
   
InstallMethod( Embedding, "hnn extension", true, 
    [ IsGroup and HasHnnExtensionInfo, IsPosInt ], 0,
    function( G, i )
        if ( i <> 1 ) then
            Error("Base group with index 1 does not exist");
        else
            return HnnExtensionInfo(G).embeddings[1];
        fi;
    end
);
   
#############################################################################
##
#M  GraphOfGroupsRewritingSystem
##
InstallMethod( GraphOfGroupsRewritingSystem, "generic method for an fpa",
    true, [ IsFreeProductWithAmalgamation ], 0,
function( fpa )

    local fy, y, verts, arcs, dig, info, f1, f2, iso, inv;

    fy := FreeGroup( "y" );
    y := fy.1; 
    verts := [5,6];
    arcs := [ [y,verts[1],verts[2]], [y^-1,verts[2],verts[1]]];
    dig := FpWeightedDigraph( fy, verts, arcs );
    info := FreeProductWithAmalgamationInfo( fpa );
    f1 := info!.groups[1];
    f2 := info!.groups[2];
    iso := info!.isomorphism;
    inv := InverseGeneralMapping( iso );
    return GraphOfGroups( dig, [f1,f2], [iso,inv] );
end );

#############################################################################
##
#M  NormalFormGGRWS
##
InstallMethod( NormalFormGGRWS, "generic method for fpa normal form",
    true, [ IsFreeProductWithAmalgamation, IsObject ], 0,
function( fpa, w )

    local iso, gg, dig, verts, ew, len, ff, idff, famff, wL, info, pos, p,
          ng1, tv, j, k, gff, gff12, gps, gen12, s, es, ggw, rgw, trgw, wrgw,
          i, e, rw;

    if not ( w in fpa ) then
        Error( "word not in the group" );
    fi;
    gg := GraphOfGroupsRewritingSystem( fpa );
    dig := DigraphOfGraphOfGroups( gg );
    verts := dig!.vertices;
    ew := ShallowCopy( ExtRepOfObj( w ) );
    Info( InfoGroupoids, 2, "ew = ", ew ); 
    if ( ew = [ ] ) then
        return One( fpa );
    fi;
    ff := FreeGroupOfFpGroup( fpa );
    idff := One( ff );
    famff := FamilyObj( idff );
    len := Length( ew );
    info := FreeProductWithAmalgamationInfo( fpa );
    pos := info!.positions;
    ng1 := Length( pos[1] );
    gff := GeneratorsOfGroup( ff );
    gff12 := [ gff{pos[1]}, gff{pos[2]} ];
    gps := info!.groups;
    gen12 := List( gps, GeneratorsOfGroup );
    ## (08/06/15) make the word start at the first vertex 
    tv := verts[1]; 
    if ( ew[1] in pos[1] ) then
        wL := [ ];
        p := 1;
    elif ( ew[1] in pos[2] ) then 
        wL := [ One( gps[1] ), 1 ]; 
        p := 2;
    else
        Error( "first vertex not found" );
    fi;
    Info( InfoGroupoids, 2, "wL = ", wL ); 
    j := 0;
    while ( j < len ) do
        k := j+2;
        while ( ( k < len ) and ( ew[k+1] in pos[p] ) ) do
            k := k+2;
        od;
        es := ew{[j+1..k]};
        s := MappedWord( ObjByExtRep( famff, es ), gff12[p], gen12[p] );
        Info( InfoGroupoids, 2, "es = ", es, ", s = ", s ); 
        Append( wL, [ s, p ] );
        Info( InfoGroupoids, 2, "wL = ", wL ); 
        p := 3-p;
        j := k;
    od;
    ## (08/06/15) make the word finish at the second vertex 
    if ( p = 2 ) then 
        Append( wL, [ One( gps[2] ), 2 ] ); 
    fi; 
    Info( InfoGroupoids, 2, "wL = ", wL ); 
    wL := wL{[1..(Length(wL)-1)]};
    Info( InfoGroupoids, 2, "wL = ", wL ); 
    ##  now have w in the form of a graph of groups word
    ggw := GraphOfGroupsWord( gg, tv, wL );
    Info( InfoGroupoids, 2, "ggw = ", ggw );
    rgw := ReducedGraphOfGroupsWord( ggw );
    Info( InfoGroupoids, 2, "rgw = ", rgw );
    ##  now convert the reduced graph of groups word back to fpa
    trgw := TailOfGraphOfGroupsWord( rgw );
    wrgw := WordOfGraphOfGroupsWord( rgw );
    if ( trgw = verts[1] ) then
        p := 1;
    else
        p := 2;
    fi;
    len := ( Length(wrgw) + 1 )/2;
    rw := idff;
    for j in [1..len] do
        k := j+j-1;
        e := ShallowCopy( ExtRepOfObj( wrgw[k] ) );
        if ( p=2 ) then
            for i in [1..(Length(e)/2)] do
                e[i+i-1] := e[i+i-1]+ng1;
            od;
        fi;
        rw := rw*ObjByExtRep( famff, e );
        p := 3-p;
    od;
    return rw;
end);

#############################################################################
##
#M  HnnExtension
##
InstallMethod( HnnExtension,
    "for an fp-groups and an isomorphism of subgroups", true,
    [ IsFpGroup, IsGroupHomomorphism ], 0, 
function( fp, iso )

    local H1, H2, gfp, ng, fe, gfe, gfe1, ffp, gffp, z,
          rel, rele, gH1, igH1, relH, hnn, ghnn, emb, rws;

    H1 := Source( iso );
    H2 := Range( iso );
    if not ( IsSubgroup( fp, H1 ) and IsSubgroup( fp, H2 ) 
             and IsTotal( iso ) and IsSingleValued( iso ) ) then
        Error( "iso not an isomorphism of subgroups" );
    fi;
    gfp := GeneratorsOfGroup( fp );
    ng := Length( gfp );
    fe := FreeGroup( ng+1, "fe" );
    gfe := GeneratorsOfGroup( fe );
    gfe1 := gfe{[1..ng]};
    z := gfe[ng+1];
    ffp := FreeGroupOfFpGroup( fp );
    gffp := GeneratorsOfGroup( ffp );
    rel := RelatorsOfFpGroup( fp );
    gH1 := GeneratorsOfGroup( H1 );
    igH1 := List( gH1, h -> ImageElm( iso, h ) );
    relH := List( [1..Length(gH1)], i -> z^-1 * MappedWord( gH1[i], gfp, gfe1 )
                * z * MappedWord( igH1[i], gfp, gfe1 )^(-1) );
    rele := Concatenation( List( rel, w -> MappedWord(w, gffp, gfe1) ), relH );
    hnn := fe/rele;
    SetIsHnnExtension( hnn, true );
    ghnn := GeneratorsOfGroup( hnn );
    emb := GroupHomomorphismByImages( fp, hnn, gfp, ghnn{[1..ng]} );
    SetIsInjective( emb, true ); 
    SetIsMappingToGroupWithGGRWS( emb, true ); 
    SetHnnExtensionInfo( hnn, rec( group := fp,
                                   subgroups := [ H1, H2 ], 
                                   embeddings := [ emb ],
                                   isomorphism := iso ) );
    rws := GraphOfGroupsRewritingSystem( hnn );
    return hnn;
end );

#############################################################################
##
#M  GraphOfGroupsRewritingSystem
##
InstallMethod( GraphOfGroupsRewritingSystem, "generic method for an hnn",
    true, [ IsHnnExtension ], 0,
function( hnn )

    local fz, z, verts, arcs, dig, inva, info, fp, iso, inv;

    fz := FreeGroup("z");
    z := fz.1;
    verts := [7];
    arcs := [ [z,7,7], [z^-1,7,7]];
    dig := FpWeightedDigraph( fz, verts, arcs );
    inva := InvolutoryArcs( dig );
    info := HnnExtensionInfo( hnn );
    fp := info!.group;
    iso := info!.isomorphism;
    inv := InverseGeneralMapping( iso );
    return GraphOfGroups( dig, [fp], [iso,inv] );
end );

#############################################################################
##
#M  NormalFormGGRWS
##
InstallMethod( NormalFormGGRWS, "generic method for hnn normal form",
    true, [ IsHnnExtension, IsObject ], 0,
function( hnn, w )

    local iso, gg, dig, v, ew, len, ff, idff, famff, wL, info, z, p, q,
          ng, j, k, gff, fp, gfp, idfp, s, es, ggw, rgw, trgw, wrgw,
          idhnn, famhnn, i, e, rw; 

    if not ( w in hnn ) then
        Error( "word not in the group" );
    fi;
    gg := GraphOfGroupsRewritingSystem( hnn );
    dig := DigraphOfGraphOfGroups( gg );
    Info( InfoGroupoids, 2, "graph of groups has left transversals" );
    Info( InfoGroupoids, 2, LeftTransversalsOfGraphOfGroups( gg ) );
    v := dig!.vertices[1];
    ew := ShallowCopy( ExtRepOfObj( w ) );
    if ( ew = [ ] ) then
        return One( hnn );
    fi;
    ff := FreeGroupOfFpGroup( hnn );
    idff := One( ff );
    famff := FamilyObj( idff );
    len := Length( ew );
    info := HnnExtensionInfo( hnn );
    gff := GeneratorsOfGroup( ff );
    p := Length( gff );
    z := gff[p];
    ng := p - 1;
    fp := info!.group;
    idfp := One( fp );
    gfp := GeneratorsOfGroup( fp );
    j := 0; 
    if ( ew[1] = 3 ) then 
        wL := [ idfp ];
    else 
        wL := [ ]; 
    fi; 
    while ( j < len ) do 
        k := j; 
        if ( ew[j+1] = p) then 
            j := j+2; 
            if ( ew[j] > 0 ) then 
                Add( wL, 1 ); 
                for i in [2..ew[j]] do 
                    Append( wL, [ idfp, 1 ] ); 
                od; 
            else 
                Add( wL, 2 );
                for i in [2..-ew[j]] do 
                    Append( wL, [ idfp, 2 ] ); 
                od; 
            fi;
        else 
            while ( ( k < len ) and ( ew[k+1] <> p ) ) do
                k := k+2;
            od;
            es := ew{[j+1..k]};
            s := MappedWord( ObjByExtRep( famff, es ), gff, gfp );
            Add( wL, s );
            j := k;
        fi; 
    od;
    if ( RemInt( Length( wL ), 2 ) = 0 ) then 
        Add( wL, idfp );
    fi;
    ##  now have w in the form of a graph of groups word
    ggw := GraphOfGroupsWord( gg, v, wL );
    Info( InfoGroupoids, 2, "ggw = ", ggw );
    rgw := ReducedGraphOfGroupsWord( ggw );
    Info( InfoGroupoids, 2, "rgw = ", rgw );
    ##  now convert the reduced graph of groups word back to hnn
    trgw := TailOfGraphOfGroupsWord( rgw );
    wrgw := WordOfGraphOfGroupsWord( rgw );
    idhnn := One( hnn );
    len := ( Length(wrgw) + 1 )/2;
    rw := idff;
    for j in [1..len] do
        k := j+j-1;
        e := ShallowCopy( ExtRepOfObj( wrgw[k] ) );
        rw := rw*ObjByExtRep( famff, e );
        if ( j < len ) then
            if ( wrgw[k+1] = 1 ) then
                rw := rw*z;
            else
                rw := rw*(z^-1);
            fi;
        fi;
    od;
    return rw;
end);
