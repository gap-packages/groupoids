##############################################################################
##
#W  grpgraph.gi                GAP4 package `Gpd'                Chris Wensley
#W                                                                & Emma Moore
##  version 1.34, 05/06/2015 
##
#Y  Copyright (C) 2000-2015, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file contains generic methods for FpWeightedDigraphs and 
##  FpWeightedDigraphs of groups 
##

#############################################################################
##
#M  ArcsIsosFromMatrices
##
InstallMethod( ArcsIsosFromMatrices, "generic method for a digraph",
    true, [ IsHomogeneousList, IsHomogeneousList, IsHomogeneousList ], 0,
function( V, A, H ) 

    local  n, i, j, arcs, isos; 

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

    local  fam, filter, dig;
     
    fam := FamilyObj( [ v, e ] );
    filter := IsFpWeightedDigraph;
    dig := Objectify( NewType( fam, filter ), rec () );
    ## changed from attributes to components 04/04/06 
    ## and changed back again 25/10/12 (but conflicts with Grape!) 
    ## and changed back to components again 12/01/13 
    dig!.group := gp; 
    dig!.vertices := v;
    dig!.arcs := e; 
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
#M  WeightedSpanningTree                                              
##
InstallMethod( WeightedSpanningTree, "generic method for a FpWeightedDigraph",
    true, [ IsFpWeightedDigraph ], 0,
function( dig )

    local  gp, verts, arcs, lenv, lena, paths, adjmx, i, j, p, t, w;

    gp := dig!.group; 
    verts := dig!.vertices; 
    arcs := dig!.arcs; 
    adjmx := WeightedAdjacencyMatrix( dig ); 
    lenv := Length( verts );
    lena := Length( arcs ); 
    paths := ListWithIdenticalEntries( lenv, 0 ); 
    t := 0; 
    paths[1] := GraphOfGroupsWord( dig, 1, [ One(gp) ] ); 
    for i in [1..lenv] do 
        for j in [1..lenv] do 
            w := adjmx[i][j]; 
            if ( w <> 0 ) then 
                p := p+1; 

            fi; 
        od; 
    od; 
    return paths;
end);

#############################################################################
##
#M  PrintObj( <dig> ) . . . . . . . . . . . . . . . print a weighted digraph
##
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

#############################################################################
##
#M  ViewObj( <dig> ) . . . . . . . . . . . . . . . . view a weighted digraph
##
InstallMethod( ViewObj, "method for a weighted digraph",
    [ IsFpWeightedDigraph ],
function( dig )
    if HasName( dig ) then
        Print( Name( dig ), "\n" );
    else
        Print("weighted digraph with vertices: ", dig!.vertices, "\n");
        Print("and arcs: ", dig!.arcs, "\n" );
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
            Info( InfoGpd, 1, "involutory arc not available" );
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

    local  fam, filter, gg, ok; 
     
    fam := GraphOfGroupsFamily; 
    filter := IsGraphOfGroupsRep;
    gg := Objectify( NewType( fam, filter ), rec () );
    SetDigraphOfGraphOfGroups( gg, dig );
    SetGroupsOfGraphOfGroups( gg, gps );
    SetIsomorphismsOfGraphOfGroups( gg, isos );
    if ForAll( gps, g -> IsPermGroup( g ) ) then
        SetIsGraphOfPermGroups( gg, true );
    elif ForAll( gps, g -> IsFpGroup( g ) ) then
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

    local  g, i, v, e, lenV, lenE, tgtL, pos, inv, einvpos;

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
    ### THIS LINE DOES NOT MAKE SENSE :-
    ###        einvpos := Position( e, e[inv[i]] );
        for g in GeneratorsOfGroup( Source( isos[i] ) ) do
            if not ( Image( isos[inv[i]], Image( isos[i], g ) ) = g ) then
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

###############################################################################
##
#M  PrintObj( <gg> ) . . . . . . . . . . . . . . . . print a graph of groups
##
InstallMethod( PrintObj, "method for a graph of groups", [ IsGraphOfGroups ],
function( gg )
    
    local  dig;

    dig := DigraphOfGraphOfGroups( gg );
    Print( "Graph of Groups: " );
    Print( Length( dig!.vertices ), " vertices; " );
    Print( Length( dig!.arcs ), " arcs; " );
    Print( "groups ", GroupsOfGraphOfGroups( gg ) );
end );

###############################################################################
##
#M  ViewObj( <gg> ) . . . . . . . . . . . . . . . . . view a graph of groups
##
InstallMethod( ViewObj, "method for a graph of groups", [ IsGraphOfGroups ],
function( gg )
    
    local  dig;

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
    
    local  dig;

    dig := DigraphOfGraphOfGroups( gg );
    Print( "Graph of Groups with :- \n" );
    Print( "    vertices: ", dig!.vertices, "\n" );
    Print( "        arcs: ", dig!.arcs, "\n" );
    Print( "      groups: ", GroupsOfGraphOfGroups( gg ), "\n" );
    Print( "isomorphisms: ", List( IsomorphismsOfGraphOfGroups( gg ), 
                                   m -> MappingGeneratorsImages(m) ), "\n" );
end );

##############################################################################
##
#M  IsGraphOfPermGroups( <gg> ) . . . . . . . . . . . .  for a graph of groups
#M  IsGraphOfFpGroups( <gg> ) . . . . . . . . . . . . .  for a graph of groups
#M  IsGraphOfPcGroups( <gg> ) . . . . . . . . . . . . .  for a graph of groups
##
InstallMethod( IsGraphOfPermGroups, "generic method", [ IsGraphOfGroups ],
function( gg )
    return ForAll( GroupsOfGraphOfGroups( gg ), g -> IsPermGroup( g ) );
end );

InstallMethod( IsGraphOfFpGroups, "generic method", [ IsGraphOfGroups ],
function( gg )
    return ForAll( GroupsOfGraphOfGroups( gg ), g -> IsFpGroup( g ) );
end );

InstallMethod( IsGraphOfPcGroups, "generic method", [ IsGraphOfGroups ],
function( gg )
    return ForAll( GroupsOfGraphOfGroups( gg ), g -> IsPcGroup( g ) );
end );

#############################################################################
##
#M  RightTransversalsOfGraphOfGroups
##
InstallMethod( RightTransversalsOfGraphOfGroups, 
    "generic method for a group graph", true, [ IsGraphOfGroups ], 0,
function( gg )

    local  gps, dig, isos, adig, vdig, len, i, g, rc, rep, trans;

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
        rep := List( rc, r -> Representative( r ) );
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

    local  gps, dig, vdig, adig, len, i, trans, itran, g; 

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

    local  fam, filter, ggword;
     
    fam := FamilyObj( [ gg, wL] );
    filter := IsGraphOfGroupsWordRep;
    ggword := Objectify( NewType( fam, filter ), rec () );
    SetIsGraphOfGroupsWord( ggword, true );
    SetGraphOfGroupsOfWord( ggword, gg );
    SetGGTail( ggword, tv );
    if ( Length( wL ) = 1 ) then
        SetGGHead( ggword, tv );
    fi;
    SetWordOfGraphOfGroupsWord( ggword, wL );
    return ggword; 
end );

#############################################################################
##
#M  GraphOfGroupsWord 
##
InstallMethod( GraphOfGroupsWord, "for word in graph of groups",
    true, [ IsGraphOfGroups, IsInt, IsList ], 0,
function( gg, tv, wL )

    local  gps, dig, adig, enum, vdig, n, i, j, g, v, posv, e, w;

    gps := GroupsOfGraphOfGroups( gg );
    dig := DigraphOfGraphOfGroups( gg );
    vdig := dig!.vertices;
    adig := dig!.arcs;
    enum := Length( adig );
    v := tv;
    posv := Position( vdig, v );
    g := gps[ posv ];
    w := wL[1];
    if not ( w in g ) then
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
            Error( "entry ", j, "not in group at vertex", v );
        fi;
    od;    
    return GraphOfGroupsWordNC( gg, tv, wL );
end);

##############################################################################
##
#M  ViewObj( <ggword> ) . . . . . . . . . . . . . view a graph of groups word
##
InstallMethod( ViewObj, "method for a graph of groups word", 
    [ IsGraphOfGroupsWord ],
function( ggword )
    local  w, i, gg, adig;

    gg := GraphOfGroupsOfWord( ggword );
    adig := DigraphOfGraphOfGroups( gg )!.arcs;
    w := WordOfGraphOfGroupsWord( ggword );
    Print( "(", GGTail( ggword ), ")", w[1] );
    i := 1;
    while ( i < Length(w) ) do
        i := i+2;
        Print( ".", adig[w[i-1]][1], ".", w[i] );
    od;
    Print( "(", GGHead( ggword ), ")" );
end );

##############################################################################
##
#M  PrintObj( <ggword> ) . . . . . . . . . . . .  print a graph of groups word
##
InstallMethod( PrintObj, "method for a graph of groups word", 
    [ IsGraphOfGroupsWord ],
function( ggword )
    local  w, i, gg, adig;

    gg := GraphOfGroupsOfWord( ggword );
    adig := DigraphOfGraphOfGroups( gg )!.arcs;
    w := WordOfGraphOfGroupsWord( ggword );
    Print( "(", GGTail( ggword ), ")", w[1] );
    i := 1;
    while ( i < Length(w) ) do
        i := i+2;
        Print( ".", adig[w[i-1]][1], ".", w[i] );
    od;
    Print( "(", GGHead( ggword ), ")" );
end );

#############################################################################
##
#M  GGHead                                             
##
InstallMethod( GGHead, "generic method for a graph of groups word",
    true, [ IsGraphOfGroupsWordRep ], 0,
function( ggword )

    local  w, gg, e;

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

    local  w, len, gg, dig, gps, adig, vdig, pos, i, e, ie, t, v, g;

    w := WordOfGraphOfGroupsWord( ggw );
    len := Length( w );
    gg := GraphOfGroupsOfWord( ggw );
    dig := DigraphOfGraphOfGroups( gg );
    gps := GroupsOfGraphOfGroups( gg );
    vdig := dig!.vertices;
    adig := dig!.arcs;
    if ( len = 1 ) then
        pos := Position( vdig, GGTail( ggw ) );
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

    local  w, tw, hw, gg, gps, isos, dig, adig, vdig, lw, len, k, k2, 
           he, rtrans, tran, pos, a, g, h, found, i, nwit, tsp,
           im, sub, u, v, gu, gv, e, ie, ng, rw, lenred, isfp, isid;

    if ( HasIsReducedGraphOfGroupsWord( ggword ) 
         and IsReducedGraphOfGroupsWord( ggword ) ) then
        return ggword;
    fi;
    w := ShallowCopy( WordOfGraphOfGroupsWord( ggword ) );
    tw := GGTail( ggword );
    hw := GGHead( ggword );
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
            ng := NormalFormKBRWS( gps[tw], w[1] ); 
        else
            ng := w[1];
        fi;
        return GraphOfGroupsWordNC( gg, tw, ng );
    fi;
    k := 1;
    v := tw;
    Info( InfoGpd, 3, "initial w = ", w );
    Info( InfoGpd, 3, "--------------------------------------------" );
    while ( k <= len ) do
        k2 := k+k;
        ## reduce the subword  w{[k2-1..k2+1]}
        Info( InfoGpd, 3, "w{[k2-1..k2+1]} = ", w{[k2-1..k2+1]} );
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
        Info( InfoGpd, 3, "tsp at i = ", i, " is ", tsp );
        im := Image( isos[e], tsp[2] );
        w[k2-1] := tsp[1];
        if isfp then
            w[k2+1] := NormalFormKBRWS( gps[he], im*w[k2+1] );
            Info( InfoGpd, 3, "k = ", k, ", w = ", w );
        else
            w[k2+1] := im*w[k2+1];
        fi;
        Info( InfoGpd, 3, "w{[k2-1..k2+1]} = ", w{[k2-1..k2+1]} );
        Info( InfoGpd, 3, "--------------------------------------------" );
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
                Info( InfoGpd, 1, "k = ", k, ", shorter w = ", w );
                if ( len = 0 ) then
                     rw := GraphOfGroupsWordNC( gg, u, w );
                     SetGGTail( rw, u );
                     SetGGHead( rw, u );
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
    SetGGTail( rw, tw );
    SetGGHead( rw, hw );
    SetIsReducedGraphOfGroupsWord( rw, true );
    return rw;
end);
 
##############################################################################
##
#M  \=( <ggw1>, <ggw2> ) . . . . . test if two graph of group words are equal
##
InstallOtherMethod( \=,
    "generic method for two graph of groups words",
    IsIdenticalObj, [ IsGraphOfGroupsWordRep, IsGraphOfGroupsWordRep ], 0,
function ( w1, w2 )
return ( ( GraphOfGroupsOfWord(w1) = GraphOfGroupsOfWord(w2) )
     and ( GGTail( w1 ) = GGTail( w2 ) )
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
    local  ggw1, ggw2, num, w1, w2, h1, len1, len2, w;

    num := Length( ggwlist );
    if not ( num = 2 ) then
        Error( "only works for two words at present" );
    fi;
    if not ForAll( [1..num], i -> IsGraphOfGroupsWordRep( ggwlist[i] ) ) then
        Error( "not a list of graph of groups words" );
    fi;
    ggw1 := ggwlist[1];
    ggw2 := ggwlist[2];
    if not ( GGHead( ggw1 ) = GGTail( ggw2 ) ) then
        Info( InfoGpd, 1, "GGHead(ggw1) <> GGTail(ggw2), so no composite" );
        return fail;
    fi;
    w1 := WordOfGraphOfGroupsWord( ggw1 );
    w2 := WordOfGraphOfGroupsWord( ggw2 );
    len1 := Length( w1 );
    len2 := Length( w2 );
    w := Concatenation( w1{[1..len1-1]}, [w1[len1]*w2[1]], w2{[2..len2]} );
    return GraphOfGroupsWord( GraphOfGroupsOfWord( ggw1 ),
                              GGTail( ggw1 ), w );
end );

##############################################################################
##
#M  \*( <ggw1>, <ggw2> ) . . . . . . . . product of two graph of groups words
##
InstallOtherMethod( \*, "generic method for two graph of groups words",
    IsIdenticalObj, [ IsGraphOfGroupsWordRep, IsGraphOfGroupsWordRep ], 0,
function ( ggw1, ggw2 )
    local  ggw12;

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
    local  gg, ie, i, j, w, len, iw, iggw;

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
    iggw := GraphOfGroupsWord( gg, GGHead( ggw ), iw );
    SetGGHead( iggw, GGTail( ggw ) );
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
    local  w, tv, ptv, gg, g, k, iggw, ggwn;

    if ( n = 1 ) then
        return ggw;
    elif ( n = -1 ) then
        return InverseOp( ggw );
    fi;
    if not ( GGHead( ggw ) = GGTail( ggw ) ) then
        Info( InfoGpd, 1, "GGHead(ggw) <> GGTail(ggw), so no composite" );
        return fail;
    fi;
    if ( n = 0 ) then
        tv := GGTail( ggw );
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
#M  InverseOfIsomorphismFpSemigroup
##
InstallMethod( InverseOfIsomorphismFpSemigroup, "for iso to fp-semigroup",
    true, [ IsNonSPMappingByFunctionRep ], 0,
function( iso )

    local  gp, smg, fgp, id, i, nat, fun, smgword2gpword;

    smgword2gpword := function( id, w )
        local  wlist, i;
        wlist := ShallowCopy( ExtRepOfObj( w ) );
        if ( ( wlist = [ 1, 1 ] ) or ( Length( wlist ) = 0 ) ) then
            return id;
        fi;
        for i in [1..Length(wlist)/2] do
            if ( RemInt( wlist[2*i-1], 2 ) = 0 ) then
                wlist[2*i] := - wlist[2*i];
            fi;
            wlist[2*i-1] := QuoInt( wlist[2*i-1], 2 );
        od;
        return ObjByExtRep( FamilyObj( id ), wlist );
    end;

    gp := Source( iso );
    smg := Range( iso );
    fgp := FreeGroupOfFpGroup( gp );
    id := One( fgp );
    nat := GroupHomomorphismByImages( fgp, gp, GeneratorsOfGroup( fgp ),
                                               GeneratorsOfGroup( gp ) );
    fun := x -> Image( nat, smgword2gpword( id, UnderlyingElement( x ) ) );
    return MagmaHomomorphismByFunctionNC( smg, gp, fun );
end);

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

    local iso, smg, rwsmg, smggen, fsmg, iw, uiw, ruw, fam1, riw, 
          inviso, rw;

    if not ( w0 in gp ) then
        Error( "word not in the group" );
    fi;
    iso := IsomorphismFpSemigroup( gp );
    inviso := InverseOfIsomorphismFpSemigroup( iso );
    smg := Range( iso );
    rwsmg := KnuthBendixRewritingSystem( smg );
    MakeConfluent( rwsmg );  ### this should not be necessary here !! ###
    smggen := GeneratorsOfSemigroup( smg );
    fsmg := FreeSemigroupOfKnuthBendixRewritingSystem( rwsmg );
    iw := Image( iso, w0 );
    uiw := UnderlyingElement( iw );
    ruw := ReducedForm( rwsmg, uiw );
    fam1 := FamilyObj( smggen[1] );
    riw := ElementOfFpSemigroup( fam1, ruw );
    rw :=Image( inviso, riw );
    return rw;
end);

#############################################################################
##
#M  FreeProductWithAmalgamation
##
InstallMethod( FreeProductWithAmalgamation,
    "for two fp-groups and an isomorphism of subgroups", true,
    [ IsFpGroup, IsFpGroup, IsGroupHomomorphism ], 0,
function( fp1, fp2, iso )
    local  H1, H2, gfp1, gfp2, ng1, ng2, num, fa, gfa, f1, f2, gf1, gf2,
           gfa1, gfa2, rel1, rel2, rela, gH1, igH1, relH, fpa, gfpa, e1, e2;

    H1 := Source( iso );
    H2 := Range( iso );
    if not ( IsSubgroup( fp1, H1 ) and IsSubgroup( fp2, H2 ) 
             and IsTotal( iso ) and IsSingleValued( iso ) ) then
        Error( "iso not an isomorphism of subgroups" );
    fi;
    gfp1 := GeneratorsOfGroup( fp1 );
    gfp2 := GeneratorsOfGroup( fp2 );
    ng1 := Length( gfp1 );
    ng2 := Length( gfp2 );
    num := ng1 + ng2;
    fa := FreeGroup( num, "fa" );
    gfa := GeneratorsOfGroup( fa );
    f1 := FreeGroupOfFpGroup( fp1 );
    gf1 := GeneratorsOfGroup( f1 );
    f2 := FreeGroupOfFpGroup( fp2 );
    gf2 := GeneratorsOfGroup( f2 );
    rel1 := RelatorsOfFpGroup( fp1 );
    rel2 := RelatorsOfFpGroup( fp2 );
    gfa1 := gfa{[1..ng1]};
    gfa2 := gfa{[ng1+1..num]};
    gH1 := GeneratorsOfGroup( H1 );
    igH1 := List( gH1, h -> Image( iso, h ) );
    relH := List( [1..Length(gH1)], i -> MappedWord( gH1[i], gfp1, gfa1 )
                * MappedWord( igH1[i], gfp2, gfa2 )^(-1) );
    rela := Concatenation( List( rel1, w -> MappedWord( w, gf1, gfa1 ) ),
                List( rel2, w -> MappedWord( w, gf2, gfa2 ) ), relH );
    fpa := fa/rela;
    SetIsFpaGroup( fpa, true );
    gfpa := GeneratorsOfGroup( fpa );
#    e1 := GroupHomomorphismByImages( fp1, fpa, gfp1, gfpa{[1..ng1]} );
#    e2 := GroupHomomorphismByImages( fp2, fpa, gfp2, gfpa{[ng1+1..num]} );
    SetFpaInfo( fpa, rec( groups := [ fp1, fp2 ],
#                          embeddings := [ e1, e2 ],
                          positions := [ [1..ng1], [ng1+1..num] ],
                          isomorphism := iso ) );
    return fpa;
end );

InstallMethod( FreeProductWithAmalgamation,
    "for two perm groups and an isomorphism of subgroups", true,
    [ IsPermGroup, IsPermGroup, IsGroupHomomorphism ], 0,
function( p1, p2, iso )

    local  p2f1, fp1, sub1, gen1, fgen1, fsub1, 
           p2f2, fp2, sub2, gen2, fgen2, fsub2, 
           fiso;

    p2f1 := IsomorphismFpGroup( p1 );
    fp1 := Image( p2f1 );
    sub1 := Source( iso );
    gen1 := GeneratorsOfGroup( sub1 );
    fgen1 := Image( p2f1, gen1 );
    fsub1 := Subgroup( fp1, fgen1 );
    p2f2 := IsomorphismFpGroup( p2 );
    fp2 := Image( p2f2 );
    sub2 := Range( iso );
    gen2 := Image( iso, gen1 );
    fgen2 := Image( p2f2, gen2 );
    fsub2 := Subgroup( fp2, fgen2 );
    fiso := GroupHomomorphismByImages( fsub1, fsub2, fgen1, fgen2 );
    return FreeProductWithAmalgamation( fp1, fp2, fiso );
end );

#############################################################################
##
#M  GraphOfGroupsRewritingSystem
##
InstallMethod( GraphOfGroupsRewritingSystem, "generic method for an fpa",
    true, [ IsFpaGroup ], 0,
function( fpa )

    local  fy, y, verts, arcs, dig, inva, info, f1, f2, e1, e2, pos1, pos2,
           iso, inv, h1, h2, gg;

    fy := FreeGroup("y");
    y := fy.1;
    verts := [5,6];
    arcs := [ [y,5,6], [y^-1,6,5]];
    dig := FpWeightedDigraph( fy, verts, arcs );
    inva := InvolutoryArcs( dig );
    info := FpaInfo( fpa );
    f1 := info!.groups[1];
    f2 := info!.groups[2];
#    e1 := info!.embeddings[1];
#    e2 := info!.embeddings[2];
    pos1 := info!.positions[1];
    pos2 := info!.positions[2];
    iso := info!.isomorphism;
    h1 := Source( iso );
    h2 := Range( iso );
    inv := InverseGeneralMapping( iso );
    gg := GraphOfGroups( dig, [f1,f2], [iso,inv] );
    return gg;
end );

#############################################################################
##
#M  NormalFormGGRWS
##
InstallMethod( NormalFormGGRWS, "generic method for fpa normal form",
    true, [ IsFpaGroup, IsObject ], 0,
function( fpa, w )

    local  iso, gg, dig, verts, ew, len, ff, idff, famff, wL, info, pos, p,
           ng1, tv, j, k, gff, gff12, gps, gen12, s, es, ggw, rgw, trgw, wrgw,
           i, e, rw;

    if not ( w in fpa ) then
        Error( "word not in the group" );
    fi;
    gg := GraphOfGroupsRewritingSystem( fpa );
    dig := DigraphOfGraphOfGroups( gg );
    verts := dig!.vertices;
    ew := ExtRepOfObj( w );
    if ( ew = [ ] ) then
        return One( fpa );
    fi;
## Print( "+++++ ew = ", ew, "\n" );
    ff := FreeGroupOfFpGroup( fpa );
    idff := One( ff );
    famff := FamilyObj( idff );
    len := Length( ew );
    wL := [ ];
    info := FpaInfo( fpa );
    pos := info!.positions;
## Print( "+++++ pos = ", pos, "\n" );
    ng1 := Length( pos[1] );
    gff := GeneratorsOfGroup( ff );
    gff12 := [ gff{pos[1]}, gff{pos[2]} ];
    gps := info!.groups;
    gen12 := List( gps, g -> GeneratorsOfGroup( g ) );
    if ( ew[1] in pos[1] ) then
        p := 1;
        tv := verts[1];
    elif ( ew[1] in pos[2] ) then
        p := 2;
        tv := verts[2];
    else
        Error( "first vertex not found" );
    fi;
## Print( "+++++ [p,tv,len] = ", [p,tv,len], "\n" ); 
    j := 0;
    while ( j < len ) do
        k := j+2;
        while ( ( k < len ) and ( ew[k+1] in pos[p] ) ) do
            k := k+2;
        od;
        es := ew{[j+1..k]};
        s := MappedWord( ObjByExtRep( famff, es ), gff12[p], gen12[p] );
## Print( "+++++ [es,s] = ", [es,s], "\n" ); 
        Append( wL, [ s, p ] );
        p := 3-p;
        j := k;
    od;
## Print( "++++ wL = ", wL, "\n" ); 
    wL := wL{[1..(Length(wL)-1)]};
    ##  now have w in the form of a graph of groups word
    ggw := GraphOfGroupsWord( gg, tv, wL );
## Print( "+++++ ggw = ", ggw, "\n" );
    Info( InfoGpd, 2, "ggw = ", ggw );
    rgw := ReducedGraphOfGroupsWord( ggw );
    Info( InfoGpd, 2, "rgw = ", rgw );
    ##  now convert the reduced graph of groups word back to fpa
    trgw := GGTail( rgw );
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
        e := ExtRepOfObj( wrgw[k] );
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
    local  H1, H2, gfp, ng, fe, gfe, gfe1, ffp, gffp, z,
           rel, rele, gH1, igH1, relH, hnn, ghnn, e1, e2;

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
    igH1 := List( gH1, h -> Image( iso, h ) );
    relH := List( [1..Length(gH1)], i -> z^-1 * MappedWord( gH1[i], gfp, gfe1 )
                * z * MappedWord( igH1[i], gfp, gfe1 )^(-1) );
    rele := Concatenation( List( rel, w -> MappedWord(w, gffp, gfe1) ), relH );
    hnn := fe/rele;
    SetIsHnnGroup( hnn, true );
    ghnn := GeneratorsOfGroup( hnn );
#    e1 := GroupHomomorphismByImages( fp1, hnn, gfp1, ghnn{[1..ng1]} );
#    e2 := GroupHomomorphismByImages( fp2, hnn, gfp2, ghnn{[ng1+1..num]} );
    SetHnnInfo( hnn, rec( group := fp,
#                          embeddings := [ e1, e2 ],
                          isomorphism := iso ) );
    return hnn;
end );

#############################################################################
##
#M  GraphOfGroupsRewritingSystem
##
InstallMethod( GraphOfGroupsRewritingSystem, "generic method for an hnn",
    true, [ IsHnnGroup ], 0,
function( hnn )

    local  fz, z, verts, arcs, dig, inva, info, fp, e1, e2, pos1, pos2,
           iso, inv, h1, h2, gg;

    fz := FreeGroup("z");
    z := fz.1;
    verts := [7];
    arcs := [ [z,7,7], [z^-1,7,7]];
    dig := FpWeightedDigraph( fz, verts, arcs );
    inva := InvolutoryArcs( dig );
    info := HnnInfo( hnn );
    fp := info!.group;
#    e1 := info!.embeddings[1];
#    e2 := info!.embeddings[2];
    iso := info!.isomorphism;
    h1 := Source( iso );
    h2 := Range( iso );
    inv := InverseGeneralMapping( iso );
    gg := GraphOfGroups( dig, [fp], [iso,inv] );
    return gg;
end );

#############################################################################
##
#M  NormalFormGGRWS
##
InstallMethod( NormalFormGGRWS, "generic method for hnn normal form",
    true, [ IsHnnGroup, IsObject ], 0,
function( hnn, w )

    local  iso, gg, dig, v, ew, len, ff, idff, famff, wL, info, z, p, q,
           ng, j, k, gff, fp, gfp, s, es, ggw, rgw, trgw, wrgw,
           idhnn, famhnn, i, e, rw;

    if not ( w in hnn ) then
        Error( "word not in the group" );
    fi;
    gg := GraphOfGroupsRewritingSystem( hnn );
    dig := DigraphOfGraphOfGroups( gg );
    Info( InfoGpd, 2, "graph of groups has left transversals" );
    Info( InfoGpd, 2, LeftTransversalsOfGraphOfGroups( gg ) );
    v := dig!.vertices[1];
    ew := ExtRepOfObj( w );
    if ( ew = [ ] ) then
        return One( hnn );
    fi;
    ff := FreeGroupOfFpGroup( hnn );
    idff := One( ff );
    famff := FamilyObj( idff );
    len := Length( ew );
    wL := [ ];
    info := HnnInfo( hnn );
    gff := GeneratorsOfGroup( ff );
    p := Length( gff );
    z := gff[p];
    ng := p - 1;
    fp := info!.group;
    gfp := GeneratorsOfGroup( fp );
    j := 2;
    while ( j <= len ) do
        k := j;
        while ( ( k < len ) and ( ew[k+1] <> p ) ) do
            k := k+2;
        od;
        es := ew{[j-1..k]};
        s := MappedWord( ObjByExtRep( famff, es ), gff, gfp );
        Add( wL, s );
        if ( k < len ) then
            q := ( 3 - ew[k+2] )/2;
            Add( wL, q );
            j := k+2;
        else
            j := k;
        fi;
        j := j+2;
    od;
    if ( RemInt( Length( wL ), 2 ) = 0 ) then
        Add( wL, idff );
    fi;
    ##  now have w in the form of a graph of groups word
    ggw := GraphOfGroupsWord( gg, v, wL );
    Info( InfoGpd, 2, "ggw = ", ggw );
    rgw := ReducedGraphOfGroupsWord( ggw );
    Info( InfoGpd, 2, "rgw = ", rgw );
    ##  now convert the reduced graph of groups word back to hnn
    trgw := GGTail( rgw );
    wrgw := WordOfGraphOfGroupsWord( rgw );
    idhnn := One( hnn );
    len := ( Length(wrgw) + 1 )/2;
    rw := idff;
    for j in [1..len] do
        k := j+j-1;
        e := ExtRepOfObj( wrgw[k] );
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

## ------------------------------------------------------------------------##
##            Graph of Groups Groupoid  - still to be implemented          ##
## ------------------------------------------------------------------------##
 
#############################################################################
##
## #M  GraphOfGroupsGroupoid
##
## InstallMethod( GraphOfGroupsGroupoid, "for a graph of groups",
##     true, [ IsGraphOfGroups ], 0,
## function( gg )
##
##     local  gpd;
##     return fail;
## end );

#############################################################################
##
#E  grpgraph.gi . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##
