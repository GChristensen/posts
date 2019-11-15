---
layout: post
title: "8 Years of Clojure and Google App Engine: it's like a candy. Although, a bittersweet one."
categories: [Clojure, Web, Google App Engine]
---

Below are some highligths from my eight-years-long experience of 
the maintaining of the [feedxcavator](https://gchristensen.github.io/#feedxcavator2)
application.

### Why Clojure? Why on Google App Engine?

[Clojure](https://clojure.org/) is a dynamically-typed language with almost 
ideal blend of functional and object-oriented primitives. This allows to create
quality software in almost no-time. With Clojure you can concentrate on the logic and 
leave the troubles about which functional interface to use with this particular
lambda for Java programmers. Of course, such approach has its well-known disadvantages, 
because the interrelations and specifications of the created interfaces are left 
mostly in the mind of the author (although there are [some](https://clojure.org/guides/spec)
improvements on this part). 

But an experienced developer should not fear such things.
Being guided by her vision as her best tool, she should be able to successfully manage
complexity, creating hierarchical layers of abstractions which are optimally balanced 
in the domains of clarity and extensibility. While  in the object-oriented world 
[design patters](https://en.wikipedia.org/wiki/Design_pattern)
are a well established set of complexity management paradigms, 
in the realm of dynamic languages [DSLs](https://en.wikipedia.org/wiki/Domain-specific_language)
are at the pinnacle of the art, which still remains largely a Terra Incognita. 
But because this post is about Clojure and App Engine, we avoid meditations on these matters here.
It is enough to say, that [GAE](https://cloud.google.com/appengine/) is
the most affordable, feature-rich and permissive web-hosting platform you can find for free.
 
### The sweet part
 
#### Where to begin

If you ever touched the develompment of web-applications, particularly in App Engine, 
you know that this may be a tedious experience. But if you use appropriate Clojure libraries, 
a basic working skeleton of a web-app may look like the following:  

```clojure
(ns clojure-gae.app
  (:require [clojure-gae.backend :as backend]
            [appengine-magic.core :as ae])
  (:use compojure.core))

(defroutes clojure-gae-app-routes
  (GET "/" [] (backend/front-page))
  (GET "/some-request" [parameter] (backend/some-request parameter)))

(ae/def-appengine-app clojure-gae-app #'clojure-gae-app-routes)
```

Here we use [compojure](https://github.com/weavejester/compojure/wiki) 
and the prominent [appengine-magic](https://github.com/GChristensen/appengine-magic)
library, which performs all the App Engine related stuff at the backstage.
A fork of it, to be precise, the one that still works in 2019. Just 
set up your project with the appengine-magic-provided [leiningnen](https://leiningen.org/)   
plugins as it is [described](https://github.com/GChristensen/appengine-magic#project-setup)
in the documentation, and you are ready to go. But you should use [Google Cloud SDK](https://cloud.google.com/sdk/)
at the modern times instead of the legacy App Engine SDK, support of which will be 
terminated soon.

#### REPL-based development

Because you are using Clojure, you do not need to recompile the project and restart
the server each time you have made a modification. This probably is the main 
reason why it is such a fun to use Clojure for web development. Just load the modified files 
into leiningen [REPL](https://github.com/technomancy/leiningen/blob/stable/doc/TUTORIAL.md#running-code)
from your favorite Clojure IDE. But the REPL should run an *App Engine development server*
to make your application to be able to process HTTP requests using App Engine facilities. 

There are good news: you can start appengine-magic App Engine development server 
at the default 8080 port with the following code: 

```clojure
(load-file "src/main/clj/clojure-gae/app.clj")
(eval '(do
  (in-ns 'clojure-gae.app)
  (appengine-magic.core/start clojure-gae-app)))
```

If you put this code into a file named `start-local-server.clj` at the project root directory
and add the following line into your `project.clj`:

```clojure
:repl-options {:init (load-file "start-local-server.clj")}
```

the server will start automatically each time you run REPL with leiningen.

#### Deploying the application

Before the deployment, build the application WAR folder with the `lein appengine-prepare`
command. Then execute the following line from the project root folder: 

```shell
gcloud app deploy war/WEB-INF/appengine-web.xml --version=gae_app_version --project=your_gae_project_name 
```

If you have `cron.yaml` or `queue.yaml` files which define cron jobs or queue properties, 
each of them should be deployed separately by using the same command as above, but where
`war/WEB-INF/appengine-web.xml` is substituted to the appropriate file path.

### The bitter part

#### Clojure startup time is a disaster

Startup time is the eternal problem of Clojure.
If [manual instance scaling](https://cloud.google.com/appengine/docs/standard/python/how-instances-are-managed)
is not an option for you, you will often experience a startup lag, because
automatically scaled Google App Engine instances are deleted by the environment after some 
idle time. To address this problem you may periodically send warm-up requests to the instance,
for example, from a cron job. To limit the number of simultaneously running instances
you may use the following section in `appengine-web.xml`, which sets the maximum number 
of frontend instances to one:

```xml
  <service>default</service>
  <instance-class>F1</instance-class>
  <automatic-scaling>
    <min-idle-instances>1</min-idle-instances>
    <max-idle-instances>1</max-idle-instances>
    <max-instances>1</max-instances>
    <min-pending-latency>automatic</min-pending-latency>
    <max-pending-latency>400ms</max-pending-latency>
    <max-concurrent-requests>50</max-concurrent-requests>
  </automatic-scaling>
```

#### GAE frontend request processing time is limited to one minute

To perform some time-demanding work you need to redirect requests to the backend instances.
The good news are that you can deploy the same code as you use at your frontend
to the backend servies (for example, through queues with a backend target).
To accomplish this, before deployment comment out the configuration of the default service
(which is shown above) and add the following section to your `appengine-web.xml`: 

```xml
  <service>backend</service>
  <instance-class>B1</instance-class>
  <basic-scaling>
    <max-instances>1</max-instances>
    <idle-timeout>1m</idle-timeout>
  </basic-scaling>
```

This will work, although the [default](https://cloud.google.com/appengine/docs/standard/java/configuration-files)
manual requires a dedicated directory for each service in the application.

#### appengine-magic local development server is a phony

The real local development server which you can invoke with Cloud SDK uses complex
preliminary initialization, that is absent in appengine-magic (it should be
reverse-engineered to appear there). So, there are some missing features, such as
services or BlobStore ([Cloud Storage](https://cloud.google.com/storage/) is 
recommended by Google instead of it, anyway). Use the local server from Cloud SDK for
production-grade testing.

#### appengine-magic lacks modern Google APIs

appengine-magic is a quite old unmaintained library which lacks the new additions to
Google Cloud APIs such as Cloud Storage or Cloud Tasks. Because the usage of an 
unmaintained library is like a walk on a minefield (the next version of Clojure
may break something in an unexpected place), the decision to use it in production
may be a tough one.