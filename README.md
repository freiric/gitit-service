Gitit-service aims to provide a wiki system based on pandoc and git like Gitit. 
Unlike Gitit however, Gitit-service is designed to be used as a REST web service, thus bringing the power of pandoc to mere mortal (javascript dev).

* GOALS

** Because it is designed as a web service, 
 - it will enable to use a pandoc powered wiki in any web application, not only application based on haskell, as with happstack (gitit) or yesod (gitit2) one.
 - encourage own styling and templating, as the client will be truely independent from the application.
** As an orthogonal goal we want to develop features for technical documentation:
 - table of content/site map extending to sub-pages
 - ... 

* Feature:
 - api to access metadata including list of content, keywords.

* Run

```
stack build
stack exec gitit-service
```

Then hit http://localhost:8080/Help


* Development

Depends on patched version of pandoc to make some method public.
