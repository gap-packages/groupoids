#############################################################################
##
##  PackageInfo.g  file for the package groupoids 
##  Emma Moore and Chris Wensley 
##

SetPackageInfo( rec(
PackageName := "groupoids",
Subtitle := "Groupoids, graphs of groups, and graphs of groupoids",

Version := "1.51",
Date := "06/08/2017", 

##  duplicate these values for inclusion in the manual: 
##  <#GAPDoc Label="PKGVERSIONDATA">
##  <!ENTITY GROUPOIDSVERSION "1.51">
##  <!ENTITY GROUPOIDSRELEASEDATE "06/08/2017">
##  <!ENTITY GROUPOIDSTARFILENAME "groupoids-1.51.tar.gz">
##  <!ENTITY GROUPOIDSHTMLFILENAME "groupoids.html">
##  <!ENTITY GROUPOIDSLONGRELEASEDATE "6th August 2017">
##  <!ENTITY GROUPOIDSCOPYRIGHTYEARS "2000-2017">
##  <#/GAPDoc>

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
    Place         := "Llanfairfechan",
    Institution   := "University of Wales, Bangor"
  )
],

Status := "accepted",
CommunicatedBy := "Derek Holt (Warwick)",
AcceptDate := "05/2015",

SourceRepository := rec( 
  Type := "git", 
  URL := "https://github.com/gap-packages/groupoids"
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := "https://gap-packages.github.io/groupoids/",
README_URL      := Concatenation( ~.PackageWWWHome, "README.md" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL, 
                                  "/releases/download/v", ~.Version, 
                                  "/", ~.PackageName, "-", ~.Version ), 
SupportEmail := "c.d.wensley@bangor.ac.uk",
ArchiveFormats  := ".tar.gz",

AbstractHTML :=
"The groupoids package provides a collection of functions for computing with \
finite groupoids, graph of groups, and graphs of groupoids. \
These are based on the more basic structures of magmas with objects \
and their mappings. \
It provides functions for normal forms of elements in Free Products with \
Amalgamation and in HNN extensions. \
Up until April 2017 this package was named Gpd.",

PackageDoc := rec(
  BookName  := "groupoids",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Finite Groupoids and Graphs of Groups",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.8.7",
  NeededOtherPackages := [ [ "GAPDoc", ">= 1.5.1" ], 
                           [ "fga", ">= 1.3.1" ] ],
  SuggestedOtherPackages := [ [ "semigroups", ">= 2.8.0"] ],
  ExternalConditions := [ ]
),

AvailabilityTest := ReturnTrue,

BannerString := Concatenation(
    "Loading groupoids ", String( ~.Version ), 
    " (algorithms for finite groupoids)\n", 
    "by Emma Moore and Chris Wensley (http://pages.bangor.ac.uk/~mas023/)\n",
    "--------------------------------------------------------------------\n" ),

TestFile := "tst/testall.g", 

Keywords := [ "magma with objects", "groupoid", "graph of groups", 
              "free product with amalgamation", "HNN extension", 
              "automorphisms" ]

));
