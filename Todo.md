REST
servant

Cache:
  
  persistence
  
  - serialization to file(maybe something else later)
	persistent-cereal
	cereal

Todo:

  - serialization to file(maybe something else later)
  - Settings: read from file (see gitit2 src/Config.hs)
  - FileStore
  - gitit plugins
  - wikilink converter: convert link from subsite to link of site
  - look at exception
  - look at return type (either Monad or IO when fetching / looking config)
  - look at String/Text lazyText Text.char8 Text.UTF8
  - append a prefix to link?

look at error/exception handling

Rest endpoints:

- view
/_index IndexBaseR GET
!/_index/*Page  IndexR GET
/_random RandomR GET
/_raw/*Page RawR GET
/_edit/*Page  EditR GET
/_revision/#RevisionId/*Page RevisionR GET
/_revert/#RevisionId/*Page RevertR GET
/_update/#RevisionId/*Page UpdateR POST
/_create/*Page CreateR POST
/_delete/*Page DeleteR GET POST
/_search SearchR POST
/_go GoR POST
/_upload UploadR GET POST
/_diff/#RevisionId/#RevisionId/*Page DiffR GET
/_history/#Int/*Page HistoryR GET
/_activity/#Int ActivityR GET
/_atom AtomSiteR GET
!/_atom/*Page AtomPageR GET
/_export/#Text/*Page ExportR GET
/_expire/*Page ExpireR POST
!/_expire ExpireHomeR POST
/_categories CategoriesR GET
/_category/#Text CategoryR GET
/_preview/*Page PreviewR POST
!/*Page     ViewR GET


