#############################################################################
##
##  PackageInfo.g  file for the package Gpd version 1.42 
##  Emma Moore and Chris Wensley 
##

SetPackageInfo( rec(
PackageName := "gpd",
Subtitle := "Groupoids, graphs of groups, and graphs of groupoids",

Version := "1.42",
Date := "15/02/2016", 

##  duplicate these values for inclusion in the manual: 
##  <#GAPDoc Label="PKGVERSIONDATA">
##  <!ENTITY GPDVERSION "1.42">
##  <!ENTITY GPDRELEASEDATE "15/02/2016">
##  <!ENTITY GPDTARFILENAME "gpd-1.42.tar.gz">
##  <!ENTITY GPDHTMLFILENAME "gpd.html">
##  <!ENTITY GPDLONGRELEASEDATE "15th February 2016">
##  <!ENTITY GPDCOPYRIGHTYEARS "2000-2016">
##  <#/GAPDoc>

PackageWWWHome := 
 "http://pages.bangor.ac.uk/~mas023/chda/gpd/",

ArchiveURL := "http://pages.bangor.ac.uk/~mas023/chda/gpd/gpd-1.42", 
ArchiveFormats := ".tar.gz",

Persons := [
  rec(
    LastName      := "Moore",
    FirstNames    := "Emma J.",
    IsAuthor      := true,
    IsMaintainer  := false,
    ## Email         := "",
    ## WWWHome       := "",
    ## PostalAddress := Concatenation( ["\n", "UK"] ),
    ## Place         := "",
    ## Institution   := ""
  ),
  rec(
    LastName      := "Wensley",
    FirstNames    := "Christopher D.",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "c.d.wensley@bangor.ac.uk",
    WWWHome       := "http://pages.bangor.ac.uk/~mas023/",
    PostalAddress := Concatenation( [
                       "Dr. C.D. Wensley\n",
                       "School of Computer Science\n",
                       "Bangor University\n",
                       "Dean Street\n",
                       "Bangor\n",
                       "Gwynedd LL57 1UT\n",
                       "UK"] ),
    Place         := "Bangor",
    Institution   := "University of Wales, Bangor"
  )
],

Status := "accepted",
CommunicatedBy := "Derek Holt (Warwick)",
AcceptDate := "05/2015",

README_URL := 
  Concatenation( ~.PackageWWWHome, "README" ),
PackageInfoURL := 
  Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),

##  Optional:
##    - Type and the URL of the source code repository
##    - URL of the public issue tracker
##    - Support email address
SourceRepository :=
  rec( Type := "git", # must be one of "git", "hg", "svn", "cvs"
       URL  := "http://github.com/gap-packages/gpd"),
IssueTrackerURL := "http://github.com/gap-packages/gpd/issues",
SupportEmail := "c.d.wensley@bangor.ac.uk",

AbstractHTML :=
"The Gpd package provides a collection of functions for computing with \
finite groupoids, graph of groups, and graphs of groupoids. \
These are based on the more basic structures of magmas with objects \
and their mappings. \
It provides functions for normal forms of elements in Free Products with \
Amalgamation and in HNN extensions.",

PackageDoc := rec(
  BookName  := "Gpd",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Finite Groupoids and Graphs of Groups",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.8",
  NeededOtherPackages := [ [ "GAPDoc", ">= 1.5.1" ], 
                           [ "fga", ">= 1.3.0" ] ],
  SuggestedOtherPackages := [ [ "semigroups", ">= 2.7.2"] ],
  ExternalConditions := [ ]
),

AvailabilityTest := ReturnTrue,

BannerString := Concatenation(
    "Loading Gpd ", String( ~.Version ), " for GAP 4.8", 
    " - Emma Moore and Chris Wensley ...\n" ),

TestFile := "tst/testall.g", 

Keywords := [ "magma with objects", "groupoid", "graph of groups", 
              "free product with amalgamation", "HNN extension", 
              "automorphisms" ]

));
