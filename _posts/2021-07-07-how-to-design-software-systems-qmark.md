---
layout: post
title: How to Design Software Systems?
categories: [Software, Systems, Design]
---

The question in the title implies many underlying aspects, for example, the linguistic clarity of the domain model or 
the intricacies of project management. But if you are asking yourself more mundane questions, like:
"How should I partition these classes?" or "How to implement API of the business layer?" this post is for you.
We begin with well-known basic concepts that you may have encountered elsewhere, but not suspected that they are all
parts of the larger picture, and finish with the rarely-read graduate-level literature. Maybe at the end you will find 
your own answer to the question how to design software systems?

Essentially, software engineering is the art of creating abstractions. Quality and elaboration of the used
abstractions separate failed systems from successfully designed ones, which are comprehensible, maintainable and have
minimum amount of defects. So, how to design quality abstractions, you may ask? We can try to answer this question from 
the several following perspectives:

* Software modeling.
* Immunity to variation.
* Complexity management.
* Structural considerations.
* Software quality attributes.
* Software architecture.
* Domain-driven design.

And as the introductory paragraph suggests, there may be more different approaches to the problem. By taking all the
discussed principles seriously enough, you will surely be able to design successful systems.

NOTE: some terminology here is borrowed from the recommended literature below. Because authors
often use [ad-hoc](https://en.wikipedia.org/wiki/Ad_hoc) terms in the vein of
[Martin Heidegger](https://en.wikipedia.org/wiki/Martin_Heidegger), it may differ from what you accustomed
to read in specialized computer journals.

### Software Modelling

To build quality abstractions it is customary to model software systems before their implementation.
Modeling is usually performed in [UML](https://en.wikipedia.org/wiki/Unified_Modeling_Language)
by decomposing systems into components or modules and defining detailed component interfaces by thoroughly examining
interactions and dependencies between them, possibly creating a throwaway prototype.
The developed model becomes a project artifact, changes in which may drive the changes in the system implementation.

In reality, though, under [agile development methodologies](https://en.wikipedia.org/wiki/Agile_software_development)
modeling often takes place at the first iterations of the development cycle, in close
collaboration with the customer (or without it). This helps to define the requirements more precisely, which may be
too abstract initially. Also, no code is usually thrown away, confirming the proverb that the best permanent solution
is a temporary one. Instead of prototyping, some authors (Andy Hunt and Dave Thomas, "The Pragmatic Programmer")
recommend writing "tracer code", in which the most important aspects of the system are implemented and integrated
to test the validity of the developed model.

It is recommended to initially select the riskiest aspects of the system for the modeling and implementation.
Modeling also should be performed with the vision of the future evolution of the system (this may also help to
detect possible sources of variation), for which a good domain knowledge may be necessary.

Many excellent books are written on this topic. Please check the recommended literature below.

### Immunity to Variation

Immunity to variation implies that changes in some parts of the system produce minimal impact on its other parts.
Several principles help to write code that is immune to changes:

* Do not repeat yourself (also known as [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself)). This principle is
  as simple as it is powerful. By not duplicating code you minimize space for potential changes and errors.
  
* [Separation of concerns](https://en.wikipedia.org/wiki/Separation_of_concerns) - by keeping each part of the
  functionality in a dedicated module you make the possible changes easier, increasing maintainability of the code. 
  Two software quality metrics often used
  in this context: [cohesion](https://en.wikipedia.org/wiki/Cohesion_(computer_science)) - the degree to which the
  elements inside a module belong together and [coupling](https://en.wikipedia.org/wiki/Coupling_(computer_programming)) - 
  the degree of interdependence between modules. A designer should strive to maximize cohesion and minimize coupling.
    
* [Layers of abstraction](https://en.wikipedia.org/wiki/Abstraction_layer) - by isolating functionality into layers
  and by using lower-level layers only from the higher-level ones, you minimize the impact of the changes in any given layer
  on its client layers. Although a good horizontal layered architecture makes it possible to easily swap underlying
  libraries and even technologies, it is not always optimal, so several layering schemes exist, such
  as [hexagonal](https://en.wikipedia.org/wiki/Hexagonal_architecture_(software)) and onion layering.

#### Anti-patterns

* "Smart UI" - a tendency to implement all application logic in UI event handlers, which may violate all the
  principles listed above. This inevitably results in unmaintainable error-prone code. 
  [MVC](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller) design pattern
  or its variations should be used instead.

### Complexity Management

The quality of the used abstractions is very important in managing complexity. The complexity is characterized by the cognitive
effort necessary to understand the program. You can differ a good abstraction
from a bad one by its depth (this is a technical term: John Ousterhout, "A Philosophy of Software Design"). The depth
is measured by the ratio of elaboration of the abstraction interface to its functionality. A good abstraction has a succinct
well-defined interface with meaningful names and covers wast functionality. The opposite is true for a bad abstraction.
A good abstraction also hides unimportant details and reveals only important ones, minimizing the associated
cognitive noise.

A yet another simple and powerful principle also helps to keep complexity at bay: Occam's razor (also known as
[KISS](https://en.wikipedia.org/wiki/KISS_principle)). By not multiplying entities without necessity you are not making
system less comprehensible. We survey other related principles in the "Structural Considerations" section.
Below we examine what the existing programming tools and approaches offer in this field. Generally, they
allow managing complexity to the extent of what you can and can not do with them.

#### A Brief History of Programming Paradigms

##### Before E.D. (Edsger W. Dijkstra)

If you never saw a program in FORTRAN 66, please [take a look](https://en.wikibooks.org/wiki/Fortran/Fortran_examples).
There you may notice the extensive use of the "GO TO" operator without any hesitation.

In his book "Structured Computer Organization" Andrew S. Tanenbaum also offers a good description of what
programming looked like at the dawn of the computer era:

<blockquote>

In these early years, most computers were "open shop," which meant that the programmer had to operate the machine
personally. Next to each machine was a sign-up sheet. A programmer wanting to run a program signed up for a block of
time, say Wednesday morning 3 to 5 A.M. (many programmers liked to work when it was quiet in the machine room).
When the time arrived, the programmer headed for the machine room with a deck of 80-column punched cards
(an early input medium) in one hand and a sharpened pencil in the other. Upon arriving in the computer room, he or she
gently nudged the previous programmer toward the door and took over the computer.

If the programmer wanted to run a FORTRAN program, the following steps were necessary:

1. He went over to the cabinet where the program library was kept,
   took out the big green deck labeled FORTRAN compiler, put it in the card reader, and pushed the START button.
2. He put his FORTRAN program in the card reader and pushed the CONTINUE button. The program was read in.
3. When the computer stopped, he read his FORTRAN program in a second time. Although some compilers required only one
   pass over the input, many required two or more. For each pass, a large card deck had to be read in.
4. Finally, the translation neared completion. The programmer often became nervous near the end because if the compiler
   found an error in the program, he had to correct it and start the entire process all over again. If there were no
   errors, the compiler punched out the translated machine language program on cards.
5. The programmer then put the machine language program in the card reader along with the subroutine library deck
   and read them both in.
6. The program began executing. More often than not it did not work and unexpectedly stopped in the middle.
   Generally, the programmer fiddled with the console switches and looked at the console lights for a while.
   If lucky, he figured out the problem, corrected the error, and went back to the cabinet containing the big green
   FORTRAN compiler to start over again. If less fortunate, he made a printout of the contents of memory,
   called a core dump<sup>†</sup>, and took it home to study.

This procedure, with minor variations, was normal at many computer centers for years. It forced the programmers to learn
how to operate the machine and to know what to do when it broke down, which was often. The machine was frequently idle
while people were carrying cards around the room or scratching their heads trying to find out why their programs were
not working properly.

</blockquote>

† Reference to [magnetic-core memory](https://en.wikipedia.org/wiki/Magnetic-core_memory)

In other words, at least in the early versions of FORTRAN, you were not too restricted and could do anything in 
an arbitrary way by any existing means. There also were not so many of them.
This resulted in barely intelligible code and frequent errors.

##### Structured programming: GOTO considered harmful

To help this gloomy state of affairs [Edsger Dijkstra](https://en.wikipedia.org/wiki/Edsger_W._Dijkstra) promoted
[structured programming](https://en.wikipedia.org/wiki/Structured_programming) as a discipline to adhere only to
the structured control flow constructs. This was the first major step in the taming of complexity, but it had its own major
drawback: the global program state was exposed, which does not help to minimize coupling. Because this is  
undoubtedly harmful, a new technique that addresses this problem was necessary.

##### Object-Oriented Programming

Object-oriented programming introduced three new concepts that allow to minimize coupling:

* [Encapsulation](https://en.wikipedia.org/wiki/Encapsulation_(computer_programming)) - also known under the more
  general name: [information hiding](https://en.wikipedia.org/wiki/Information_hiding).
* [Subclassing](https://en.wikipedia.org/wiki/Inheritance_(object-oriented_programming)) - class inheritance. Subclassing
  facilitates code reuse while maintaining encapsulation.
* [Polymorphism](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)) - ability to call methods of a derived
  class through the reference of a superclass type, which, for example, allows
  [dependency inversion](https://en.wikipedia.org/wiki/Dependency_inversion_principle).

Although OOP had tremendous success and remains a dominant programming paradigm to this day, it is not free from
its own problems inherited from the procedural style, such as side effects, which, for example, make multithreading
a non-trivial task under this approach. Because of that, adepts of functional programming consider object-oriented
programming harmful, probably, not without a reason. Being a great and simple tool for managing complexity, OOP requires
equally great experience and discipline for its proper use.

##### Generic Programming

In [strongly-typed](https://en.wikipedia.org/wiki/Strong_and_weak_typing) languages, generic programming allows generating
multiple type-parametrized versions of the code from a single template. This helps to observe the DRY principle.
Being a simple concept by itself, generic programming requires mastering of several related concepts to unleash 
its full potential:

* [Covariance and contravariance](https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science))
  in Java-based languages.
* [Template specialization](https://en.wikipedia.org/wiki/Partial_template_specialization) and template type deduction in
  C++ (which, for example, includes universal reference-collapsing).
* [SFINAE](https://en.wikipedia.org/wiki/Substitution_failure_is_not_an_error) in C++, which is utilized in the
  [template metaprogramming](https://en.wikipedia.org/wiki/Template_metaprogramming).

Because extensive use of generic programming may result in uncontrollable binary code bloat, it is considered
harmful by byte-counting programmers.

##### Aspect-Oriented Programming

Something is still rotten in your kingdom, say proponents of [aspect-oriented programming](https://en.wikipedia.org/wiki/Aspect-oriented_programming).
You have cross-cutting concerns (this is, obviously, a technical term), such as logging, that dangle from here and there,
and pollute your code. Let's take them into a single place using [advices](https://en.wikipedia.org/wiki/Advice_(programming))
(this is a technical term familiar to Lisp programmers), which will result in a better separation
of concerns. Other programmers are shrugging, and consider this paradigm if not harmful, but of limited use,
since pointcuts (this is yet another technical term, denoting places where advices are applied) do not provide 
the same granularity as the direct use of cross-cutting concerns.

##### Metaprogramming

Metaprogramming, which allows to automatically generate any code, be it template-based, dynamically evaluated, or implemented through
[syntactic macros](https://en.wikipedia.org/wiki/Macro_(computer_science)#Syntactic_macros),
produces abstractions of such depth and eloquence, that it often results in the apparent elimination of complexity
through the introduction of obscurity (opacity of which is limited only by the creativity of the author), both in the places
of abstraction use and implementation. Because of this, metaprogramming usually considered harmful by the programmers
that are not accustomed to it.

##### Functional Programming

Although functional programming was not developed as a direct response to the problems of OOP and takes its roots in
[category-theory](https://en.wikipedia.org/wiki/Category_theory) and [lambda-calculus](https://en.wikipedia.org/wiki/Lambda_calculus)
of Alonso Church, it can successfully eliminate some hurdles of OOP by severely restricting programmers at the level of the language.
For example, side effects are prohibited in most places, and looping constructs are replaced by recursion, which is usually done through folding.
Abstractions are built through higher-order functions.
To successfully use the functional approach, a programmer needs to learn how to bypass these restrictions (often referenced
by the technical term "purity") by utilizing and [accustoming](https://hub.packtpub.com/a-five-level-learning-roadmap-for-functional-programmers) to
a dozen of mind-boggling concepts with gut-wrenching names, such as:

* [Immutable data structures](https://en.wikipedia.org/wiki/Persistent_data_structure)
* [Referential transparency](https://en.wikipedia.org/wiki/Referential_transparency)
* [Functional composition](https://en.wikipedia.org/wiki/Function_composition_(computer_science))
* [Algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type)
* [Combinators](https://en.wikipedia.org/wiki/Fixed-point_combinator)
* [Currying](https://en.wikipedia.org/wiki/Currying)
* [Folding](https://en.wikipedia.org/wiki/Fold_(higher-order_function))
* [Monads](https://en.wikipedia.org/wiki/Monad_(functional_programming))

If you already have an extensive OOP experience, the only way to master FP is to use only (and only) languages that
enforce purity, such as [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language))
or [ML](https://en.wikipedia.org/wiki/ML_(programming_language)) family of languages. But because the ability of the pure FP to
reduce cognitive effort is questionable for any real-world applications, OOP programmers often consider it harmful,
probably, not without a reason. In moderate amounts, though, it can produce truly elegant solutions.

##### Contract Programming

Design by contract, elements of which could be found, in [Eiffel](https://en.wikipedia.org/wiki/Eiffel_(programming_language)),
[Clojure](https://en.wikipedia.org/wiki/Clojure), and [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))
programming languages, aims to eliminate implicit properties of module interfaces and related obscurity by explicitly defining them in
interface contracts, for example, in the form of pre- and post-conditions of function calls, that are programmatically verifiable.
Being by itself a good academic concept, it is often considered harmful by industrial programmers, who face permanently changing
requirements and also have unit tests to maintain.

##### Reactive Programming

Originally, this approach was focused on the increasing responsivity of applications.
Reactive Programming it is where you offload multiple complex tasks from the main thread of execution to a thread pool
(or a [multiprocessing](https://en.wikipedia.org/wiki/Multiprocessing) environment) and wait for their completion
by using [futures or promises](https://en.wikipedia.org/wiki/Futures_and_promises)
(or a subscriber/publisher framework which utilizes a message queue, that is called a reactive event stream),
while showing beautiful ads to the user in full-HD and 60 FPS until the results are obtained. Because the lion share of parallelism-related
complexity is hidden beneath the libraries or language constructs, reactive programming is often sold as a separate paradigm
on the complexity-management and related markets. Unfortunately, if your business is ad-based, this approach may harm it, since
parallelized set of tasks may execute faster than the same set of sequential ones, so there may be places where it
considered harmful.

##### Domain-Specific Languages

[Domain-specific languages](https://en.wikipedia.org/wiki/Domain-specific_language) allow creating reasonably deep
and clear abstractions of such quality, that sometimes they could be used even by non-specialists in computer science.
Although the development of a good DSL, based on metaprogramming or special tools, such as [Xtext](https://en.wikipedia.org/wiki/Xtext)
or Spoofax, usually requires the amount of efforts that is an order of magnitude (or two) higher than that for a regular OOP solution.
So, more than often a DSL results in semantics, that only its authors can understand. Thus, more than often DSLs are
considered harmful.

#### Anti-patterns

* [Optional types](https://en.wikipedia.org/wiki/Option_type) - often offered as a solution of the
  [billion dollar mistake](https://en.wikipedia.org/wiki/Tony_Hoare#Apologies_and_retractions). Such types wrap a value that
  may contain a null-reference. Since they heavily rely on functional programming concepts, their use may look obscure
  and ugly, especially in languages without built-in [pattern matching](https://en.wikipedia.org/wiki/Pattern_matching),
  such as Java. A clever solution, implemented, for example, in [Kotlin](https://en.wikipedia.org/wiki/Kotlin_(programming_language))
  is just to prohibit the use of null-references for non-nullable types, and provide nullable ones with
  [null-coalescing operators](https://en.wikipedia.org/wiki/Null_coalescing_operator).

### Structural Considerations

#### SOLID Principles

These five related principles are considered a crucial discipline in the building of abstractions that are clear,
flexible, and maintainable:

* The [Single-responsibility principle](https://en.wikipedia.org/wiki/Single-responsibility_principle) -
  "A module should have only one reason to change." This principle facilitates the separation of concerns and low coupling.

* The [Open–closed principle](https://en.wikipedia.org/wiki/Open%E2%80%93closed_principle) -
  "A module should be open for extension, but closed for modification." This principle facilitates stability of the
  module interface to minimize the impact of changes. It is not surprising, that stable interfaces are usually
  well thought out.

* The [Liskov substitution principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle) -
  "A module of a given type should be interchangeable with a module of its subtype without any unexpected side effects."
  This principle promotes the consistency of the behavior in type hierarchies, which results in simple clear semantics.

* The [Interface segregation principle](https://en.wikipedia.org/wiki/Interface_segregation_principle) -
  "No client of a module should be forced to depend on methods it does not use." This principle facilitates the separation 
  of concerns (interfaces) and high cohesion, increasing code maintainability.

* The [Dependency inversion principle](https://en.wikipedia.org/wiki/Dependency_inversion_principle) -
  "A client module should not depend on other modules directly, but through abstract interfaces." 
  The use of term "inversion" in this principle name is usually explained by the fact that usually dependencies
  between modules are made to concretions. The aim of this principle to facilitate dependencies to 
  abstractions, which may make sense in the case of a concretion that is highly volatile
  and may propagate undesired changes. An abstract interface is created, that hides such concretion, which also
  eliminates the direct dependency on the referred module source code, providing decoupling. 
  Abstract factories are usually used to create such volatile objects.

All these principles are described in more detail in the book
"Clean Architecture: A Craftsman's Guide to Software Structure and Design" by Robert C. Martin.

#### GRASP Principles

These are nine less-known principles, that are focused on assigning responsibilities to objects
([General Responsibility Assignment Software Patterns](https://en.wikipedia.org/wiki/GRASP_(object-oriented_design))).

* Information expert - a responsibility is assigned to the class that has all necessary information to fulfill it.

* Creator - this pattern determines which class should have the responsibility to create some other class. Several possible
  concerns apply here:
    * Instances of the creator contain or compositely aggregate instances of the created class.
    * Instances of the creator persist instances of the created class.
    * Instances of the creator closely use instances of the created class.
    * Instances of the creator have the initializing information for instances of the created class and pass it on creation.

* Controller - in this pattern some objects are assigned the responsibility to process non-UI events of the application.
  This allows separating application-specific logic from the presentation- and business-logic.

* Indirection - this is a more general pattern, similar to the GoF [mediator](https://en.wikipedia.org/wiki/Mediator_pattern)
  design pattern. For example, the controller in MVC acts as a mediator between the presentation and model layers that
  do not communicate directly and hence are decoupled.

* Low coupling and High cohesion patterns are equivalent to the software metrics discussed above, which should be invoked
  when considering the assignment of responsibilities (behaviors).

* Polymorphism - this pattern facilitates the use of the built-in polymorphic constructs over hardcoded
  switch/case tables when assigning responsibilities, for example, as in GoF [visitor](https://en.wikipedia.org/wiki/Visitor_pattern)
  pattern, to ease a possible refactoring.

* Protected variations - this pattern facilitates isolation of possible sources of variation under the abstract interfaces,
  which can represent different (changed) polymorphic classes, so instability does not propagate to the other parts of the system.

* Pure fabrication - an artificial module, that does not present in the system model and introduced solely to maintain
  abstraction and decoupling.

GRASP principles are described in more detail in the book "Applying UML and Patterns: An Introduction to
Object-Oriented Analysis and Design and Iterative Development" by Craig Larman.

#### GoF Design Patterns

First published in the book "Design Patterns: Elements of Reusable Object-Oriented Software" (1994) by
Erich Gamma, Richard Helm, Ralph Johnson, and John Vlissides who are also known as the Gang of Four (GoF), these patterns
comprise a clever set of abstractions that eliminate many undesirable side effects of naive software design.
Since many excellent books are written on this topic, I will not discuss them. [Here](http://www.vincehuston.org/dp/),
you may find a good brief description of each pattern, of the problem it solves, and the corresponding code examples.

#### Anti-Patterns

* [Excessive use of inheritance instead of composition](https://en.wikipedia.org/wiki/Composition_over_inheritance).
  This anti-pattern is a classical example of an undesirable side effect of the straightforward software design:
  combinatorial breeding of the derived classes. For example, you need to draw N different shapes by M different algorithms
  applicable to each shape. If you want to utilize polymorphism, you may create a base class `Shape` and derive
  a Cartesian product (MxN) of shape subclasses from it, each drawn by its own algorithm, for example: `Shape1A1`, `Shape1A2`,
  `Shape2A1`, ... etc. Although, if you utilize GoF [bridge](https://en.wikipedia.org/wiki/Bridge_pattern) pattern, where  
  algorithms are invoked using composition, you will need only M+N derived classes for your implementation, while still
  using polymorphism on the shape classes.


### Software Quality Attributes

Although software quality attributes, also known as non-functional requirements, are not related to the application
logic, they may act as the major considerations in the choice of system architecture. To understand in which terms
architecture may be constrained or evaluated, we need to see some examples:

* Maintainability - the extent to which software is capable of being changed after deployment.
* Testability - the extent to which software is capable of being tested.
* Scalability - the extent to which the system is capable of growing after its initial deployment.
* Reusability - the extent to which software is capable of being reused.
* Performance - system performance characteristics such as throughput and response times.
* Elasticity - is the ability of a system to add and remove capacity based on demand.
* Availability - addresses system failure and its impact on users or other systems.
* Resilience -  the ability to provide and maintain an acceptable level of service in the face of faults and challenges to normal operation.

### Software Architecture

Software architecture is often described as a high-level system design, which operates on the level of
system layers and subsystems, rather than on the level of separate
modules. Although many divisions are possible, system architecture may be viewed from the five different standpoints:

* View of use-cases - this is a functional requirements view, which is an input
  to develop the software architecture. Each use case describes the sequence of
  interactions between one or more actors (external users) and the system.
* Static view - the architecture is depicted in structural terms of subsystems or components and relationships between them.
* Dynamic view - this view described dynamic interactions between components comprising the system.
* Deployment view - this view depicts a specific configuration of the distributed architecture
  with components assigned to hardware nodes.
* Non-functional requirements view - evaluates non-functional architecture properties such as performance or resilience.

Design on this level generally requires another level of knowledge in technology, soft skills, and modeling that differs
from the skills of a regular software developer.

#### Architecture Styles

Monolithic and distributed architectures represent two opposite approaches, the choice between which is often driven
by the non-functional requirements. Distributed architectures may employ centralized or decentralized coordination,
which is referred to by the technical terms "orchestration" and "choreography" respectively.
Architectures also may be technology- or domain- partitioned, in which
decomposition to components is performed according to the corresponding paradigm. The most common architectural styles used nowadays
are listed below.

* [Layered architecture](https://en.wikipedia.org/wiki/Multitier_architecture) - components within the layered
  architecture are organized into logical horizontal layers, with each layer preforming a specific role within the
  application. Most layered architectures consist of four standard layers: presentation, business logic, persistence,
  and database. This is an example of a technology-partitioned architecture.

* [Pipeline architecture](https://en.wikipedia.org/wiki/Pipeline_(software)) - components within this architecture
  perform only one task and are sequentially connected by mediator components ("pipes") which form a single pipeline
  with input and output ends. Widely used in applications that require simple one-way data processing.

* Microkernel architecture (also known as plug-in architecture) - is a monolithic architecture that consists of the core
  that provides minimal functionality necessary to run
  the whole system and multiple plugin components that implement the main application functionality. Eclipse IDE is a good
  example of such an application. Often used in [software product lines](https://en.wikipedia.org/wiki/Software_product_line).

* [Service-based architecture](https://en.wikipedia.org/wiki/Service-oriented_architecture) -
  is a distributed architecture that may consist of a separately deployed presentation node, a set of coarse-grained
  domain-partitioned service nodes and a monolithic database. A very popular choice for many business-related
  applications.

* [Event-driven architecture](https://en.wikipedia.org/wiki/Event-driven_architecture) - is made up of decoupled event
  processing components that asynchronously receive and process events. The components may be deployed as a clustered
  domain-partitioned set of nodes with centralized or decentralized coordination. This architecture is used in
  highly-scalable responsive and resilient applications.

* [Space-based architecture](https://en.wikipedia.org/wiki/Space-based_architecture) - consist of many processing units
  that contain application logic, virtualized controlling middleware, asynchronous data cache, and a database which
  receives updates from users asynchronously, possibly with a delay. The main aim of this architecture is to overcome the
  bottlenecks that emerge when traditional web-server deployments are trying to synchronously process requests from many users.
  A typical use case is a concert ticket ordering system, which is idle most of the time, but elastically handles
  high loads before concerts by deploying as many processing units as necessary.

* [Microservices architecture](https://en.wikipedia.org/wiki/Microservices) - this architecture takes its roots in
  the domain-driven design and consists of a large set of maximally decoupled domain-partitioned fine-grained services,
  each of which implements a bounded context, or an individual aggregate from the domain model (see below). Each service
  is meant to be independent, which implies that every service usually contains its own database. Because this architecture
  is scalable, elastic, and maintainable, it is a popular choice for modern distributed applications.

Software architecture styles are described in more detail in the book "Fundamentals of Software Architecture: An Engineering Approach"
by Mark Richards and Neal Ford.

#### Architectural Patterns

The domain of software architecture has its own repository of patterns which is described in the
"Pattern-Oriented Software Architecture" book series.

#### Anti-Patterns

* [Big ball of mud](https://en.wikipedia.org/wiki/Big_ball_of_mud) - an architecture (or lack of it), where components
  are not divided into layers or services and promiscuously depend on each other.

### Domain-Driven Design

Folks in [DDD](https://en.wikipedia.org/wiki/Domain-driven_design) camp have a whole special jargon for what other
people call: a jargon, a model, a package, a subsystem, an entity, a class, a persistence framework, and so on.
In the DDD paradigm, you say that ubiquitous language defines a domain, which is separated into bounded contexts that
are comprised of aggregates, which consist of entities and value objects stored in repositories, and so on.
Each term here implies its own deep semantic. Who knows, maybe it helps to think better about domain models.

The domain-driven design methodology is described in detail in the book "Domain-Driven Design: Tackling Complexity in
the Heart of Software" by Eric Evans. Here it is worth listing several lessons that we can derive from this approach:

1. Good understanding of the problem-area jargon may be really important for the deep understanding of the domain model.
2. The domain code (also known as business logic or business rules) should be kept clean from other types of application
   logic to minimize the gap between the domain model and its implementation. It is because of that MVC was invented, 
   and utility logic usually placed into a separate layer of services.
3. Sometimes, when domain logic is dense and volatile, it is more convenient to use [rule-engines](https://en.wikipedia.org/wiki/Business_rules_engine)
   (also known as production systems) than to implement it manually.
4. To save Houston of problems, different units of measurement and other such value objects should be implemented as separate types
   to prevent meaningless arithmetic operations between them.

### Recommended Literature

The literature below is the real introduction to software engineering, and only after reading it all one can approach the
beginning of the understanding of the essentials of system design. Although some book titles may seem misleading, every book
here, to some degree, is dedicated to important foundational concepts in software design. The list only barely touches
more special topics of no less importance such as software development methodologies, unit testing, or algorithm design, 
which should be researched separately.

* Andrew S. Tanenbaum, Todd Austin - Structured Computer Organization
* Andrew S. Tanenbaum, Bos Herbert - Modern Operating Systems
* Andrew S. Tanenbaum, Nick Feamster, David J. Wetherall - Computer Networks
* David Thomas, Andrew Hunt - The Pragmatic Programmer: Your Journey to Mastery
* John Ousterhout - A Philosophy of Software Design
* Robert C. Martin - Clean Architecture: A Craftsman's Guide to Software Structure and Design
* Craig Larman - Applying UML and Patterns: An Introduction to Object-Oriented Analysis and Design and Iterative Development
* Hassan Gomaa - Software Modeling and Design: Uml, Use Cases, Patterns, and Software Architectures
* Mark Richards, Neal Ford - Fundamentals of Software Architecture: An Engineering Approach
* Andrew S. Tanenbaum, Maarten van Steen - Distributed Systems: Principles and Paradigms
* Erich Gamma, Richard Helm, Ralph Johnson, and John Vlissides - Design Patterns: Elements of Reusable Object-Oriented Software
* Alan Shalloway - Design Patterns Explained: A New Perspective on Object-Oriented Design
* Eric Freeman, Kathy Sierra, Bert Bates, Elisabeth Robson - Head First Design Patterns
* Andrei Alexandrescu - Modern C++ Design: Generic Programming and Design Patterns Applied
* Eric Evans - Domain-Driven Design: Tackling Complexity in the Heart of Software
* Harold Abelson, Gerald Jay Sussman, Julie Sussman - Structure and Interpretation of Computer Programs
* Pierre-Yves Saumont - The Joy of Kotlin
* Michael Fogus, Chris Houser - The Joy of Clojure
* Michael Swaine - Functional Programming: A PragPub Anthology: Exploring Clojure, Elixir, Haskell, Scala, and Swift
* Scott Wlaschin - Domain Modeling Made Functional: Tackle Software Complexity with Domain-Driven Design and F#
* Debasish Ghosh - Functional and Reactive Domain Modeling
* Martin Fowler - Domain-Specific Languages
* Ryan D. Kelker - Clojure for Domain-Specific Languages
* Markus Voelter - DSL Engineering: Designing, Implementing and Using Domain-Specific Languages
 
