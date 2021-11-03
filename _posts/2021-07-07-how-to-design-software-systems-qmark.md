---
layout: post
title: How to Design Software Systems?
categories: [Software, Systems, Design]
---

In general, software design implies a multitude of various underlying aspects. For example, it may be concerned with the
quality of user experience or subtle intricacies of project management. Although if you are asking yourself more mundane
questions, such as: "How should I partition these classes?" or "How to implement API of the business layer?" this post
is for you. We begin with well-known basic concepts that you may have encountered elsewhere, but not suspected that they
are all parts of a larger picture, and finish with rarely-read graduate-level literature. Maybe at the end, you will
find your own answer to the question - how to design software systems?

Software engineering, in its essence, is the art of creating abstractions. Quality and elaboration of the used
abstractions separate failed and successfully designed software. The code of a well-designed system is easily
comprehensible, such systems are easy to develop and maintain, and have a minimum amount of defects. So, how to design
quality abstractions, you may ask? We can approach this question from the several following perspectives:

* Software modeling.
* Software design.
* Domain-driven design.
* Separation of concerns.
* Complexity management.
* Structural considerations.
* Software quality attributes.
* Software architecture.

As the introductory paragraph suggests, there are many more additional facets of the trade. By taking all the
discussed principles seriously enough, you will undoubtedly be able to design quality software.

NOTE: some terminology here is borrowed from the recommended literature below. Because authors often
use [ad hoc](https://en.wikipedia.org/wiki/Ad_hoc) terms in the vein of
[Martin Heidegger](https://en.wikipedia.org/wiki/Martin_Heidegger), it may differ from what you are accustomed reading in
revered computer science books. The links are given predominantly to Wikipedia to prevent link rot, but as always, the
best source of knowledge is in the references.

### Software Modelling

To build quality systems it is customary to model software before its implementation. Modeling begins with the formulation
of [functional system requirements](https://en.wikipedia.org/wiki/Functional_requirement), which is a 
[science on its own](https://en.wikipedia.org/wiki/Requirements_engineering). Because we focus on software design,
we assume that requirements are already subdivided into the relevant verbs and nouns, 
corresponding to the behavior and data of the system being modeled, so we can start modeling itself.

Modeling is usually performed in [UML](https://en.wikipedia.org/wiki/Unified_Modeling_Language)
by decomposing the system into subsystems, components, or modules and defining detailed component interfaces. To achieve
this, all dependencies between system components and their interactions, including possible error handling, are
thoroughly examined. A throwaway prototype may be created to evaluate the model. In an ideal world, the developed model
would become a project artifact, changes in which may drive the corresponding changes in the implementation of the
system. But you surely know what happens in these [agile](https://agilemanifesto.org/) projects, where code is valued
over documentation.

In reality, under [agile development methodologies](https://en.wikipedia.org/wiki/Agile_software_development)
modeling often takes place at the first iterations of the development cycle in parallel to writing code. This helps to
define the requirements more precisely, which may be too abstract initially, in close collaboration with the customer (
or without it). Also, no code is usually thrown away, confirming the proverb that the best permanent solution is a
temporary one. Instead of prototyping, some authors (Andy Hunt and Dave Thomas, "The Pragmatic Programmer")
recommend writing "tracer code", in which the most important aspects of the system are implemented and integrated to
test the validity of the developed model.

It is recommended to initially select the riskiest aspects of the system for the modeling and implementation. It is also
often advised that modeling should be performed with the vision of the future evolution of the system, which may 
help to detect potential sources of variation, for which good domain knowledge may be necessary.

Many excellent books are written on this topic. Please, check the recommended literature below.

### Software Design

At this point authors usually insert an image of a craftsman with a chisel and explain how it is important to name
variables in a consistent and obvious way, write literary-inspired comments, create small functions, and so on. But we
are interested in more grandeur aspects of the trade. Namely, how to prevent system rot under the pressure of the 
omnipresent second law of thermodynamics that strives to turn everything into an incorrigible mess? How to minimize the
impact of possible changes? How to make the system easily extensible and comprehensible at the same time?

At the level of design, we have all its basic elements at our full disposal: functions, methods, classes, packages, and
modules. At this particular level, the trick is to arrange them just in the right way under a set of mutually-decoupled
layers of abstractions. Several design principles discussed below, accompanied by a range of well-established design 
patterns, may greatly help with this task. The sad truth is that these aspects of design still remain more art than 
science and require a profound knowledge of the practice of pattern application for any success.

It is a sign of skill if after a look at the model you say: "To properly separate concerns I should implement this core
functionality as a set of fine-grained classes and use decorators for optional features". But the task of design is so
cognitively daunting, that it is rarely done in the right way from the start. Usually, design is gradually improved over
the course of the development, so an image of a craftsman working with clay is more appropriate. 

On the one hand, they say that [YAGNI](https://en.wikipedia.org/wiki/You_aren%27t_gonna_need_it). On the other, you need
to account for the system extensibility, which marks an explicit tradeoff. Successful designers are guided by the most 
salient features in the context of the system evolution, and some good books on thinking may help to discover them, 
such as "Blink: The Power of Thinking Without Thinking" by Malcolm Gladwell, or
"Think Again: The Power of Knowing What You Don't Know" by Adam Grant. More advanced cognitive techniques, such
as [sleep on it](https://link.springer.com/article/10.3758/s13421-012-0256-7)
or [insight meditation](https://www.sciencedirect.com/science/article/abs/pii/S1053810012000578) may significantly
boost success rate if used properly (at this point many authors insert an image of a psychic with a crystal ball).

### Domain-Driven Design

The approach of [domain-driven design](https://en.wikipedia.org/wiki/Domain-driven_design) puts the value of domain
logic in the first place and has an idiosyncratic way of arranging domain objects. Folks in this camp have a whole special
jargon for what other people call: a jargon, a model, a package, a subsystem, an entity, an object, a persistence
framework, and so on. In the DDD paradigm, you say that ubiquitous language defines a domain, which is separated into
bounded contexts that are comprised of aggregates, which consist of entities and value objects stored in repositories,
and so on. Each term here implies its own profound semantics. Who knows, maybe this helps to think better about domain
models. The key principle here is that the units of abstraction should closely correspond to the entities of the domain,
which makes it easier to keep the minimal gap between the domain and implementation.

The domain-driven design methodology is described in the book "Domain-Driven Design: Tackling Complexity in
the Heart of Software" by Eric Evans. Here it is worth listing several lessons that could be derived from this approach:

1. A good understanding of the problem-area jargon may be important for the creation of an adequate domain model.
2. The domain code (also known as business logic or business rules) should be kept clean from other types of application
   logic to minimize the gap between the domain model and its implementation. It is because of that MVC was invented,
   and non-domain utility or application logic is usually placed into a separate layer of controllers or services.
3. To save Houston of problems, different units of measurement and other such value objects should be implemented as
   separate types to prevent meaningless, for example, arithmetic operations between them.

### Separation of Concerns

[Separation of concerns](https://en.wikipedia.org/wiki/Separation_of_concerns) 
(modularization) is the cornerstone of quality software. By keeping each part of the
functionality in a dedicated module, you make the possible changes easier and increase the maintainability of the code.
Well-separated concerns are also immune to variation - changes in some parts of the system produce a minimal impact on its
other parts.

Several principles help to write well-modularized code:

* Do not repeat yourself (also known as [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself)). This principle is
  as simple as it is powerful. By not duplicating code you minimize the area for potential changes and errors.

* [Single responsibility principle](https://en.wikipedia.org/wiki/Single-responsibility_principle) - a component
  should have only one reason to change.

* Decoupling - this concept implies that modules, for example, classes in object-oriented
  languages, should only minimally depend on each other. Two source code metrics are often used in this
  context: [cohesion](https://en.wikipedia.org/wiki/Cohesion_(computer_science)) - the degree to which the elements
  inside a module belong together and [coupling](https://en.wikipedia.org/wiki/Coupling_(computer_programming)) - the
  degree of interdependence between modules. A designer should strive to maximize cohesion and minimize coupling. 
  There are several approaches that facilitate decoupling, for
  example, [dependency inversion](https://en.wikipedia.org/wiki/Dependency_inversion_principle) which is, in essence, 
  is the programming to abstractions, or the GoF [mediator](https://en.wikipedia.org/wiki/Mediator_pattern) pattern.

* Packaging and maintaining component boundaries. On a more coarse-grained level of packages and components, which may
  contain multiple classes, [software package metrics](https://en.wikipedia.org/wiki/Software_package_metrics) are used
  to think about coupling and the impact of changes. In short, system components are divided on unstable (nothing depends on them),
  flexible (little depends on them), and stable (many other components depend on them). The stable abstraction principle 
  states that a stable component should also be abstract to make system extension easy.

* [Layers of abstraction](https://en.wikipedia.org/wiki/Abstraction_layer) - by isolating related modules, components or
  services into layers and by using lower-level layers only from the higher-level ones, you minimize the impact of the
  changes in any given layer on its client layers. Although a good horizontal layered architecture makes it possible to
  easily swap underlying libraries and even technologies, it is not always optimal, so several layering schemes exist,
  such as [hexagonal](https://en.wikipedia.org/wiki/Hexagonal_architecture_(software)) and onion layering.

#### Anti-patterns

* "Smart UI" - a tendency to implement all application logic in UI event handlers, which may violate all the
  principles listed above. This inevitably results in unmaintainable error-prone code and hiders system evolution.
  [MVC](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller) design pattern
  or its variations should be used instead.

### Complexity Management

The quality of the used abstractions is very important in managing complexity. In a broad sense, complexity is characterized
by the cognitive effort necessary to understand the program. A quality abstraction may be distinguished from a bad one
by its depth (this is a technical term: John Ousterhout, "A Philosophy of Software Design"). The depth is measured by
the ratio of elaboration of the abstraction interface to its functionality. A good abstraction has a succinct
well-defined interface with meaningful names and covers wast functionality. The opposite is true for a bad abstraction:
it has a wast interface with poorly named members that, possibly, just delegate functionality to another layer.
Although, sometimes this may be a necessary evil, such as in the
GoF [adapter](https://en.wikipedia.org/wiki/Adapter_pattern) pattern. A good abstraction also hides unimportant details
and reveals only important ones, minimizing the associated cognitive noise, for example, by providing reasonable
defaults.

A yet another simple and powerful principle also helps to keep complexity at bay: Occam's razor (also known as
[KISS](https://en.wikipedia.org/wiki/KISS_principle)). By not multiplying entities without necessity, you are not making
the system less comprehensible. 

We survey other related principles in the "Structural Considerations" section. Below we
examine what the existing programming tools and approaches offer in this field. Generally, they allow managing
complexity to the extent of what you can and can not do with them. Practice suggests that too much or too little 
freedom results in lamentable consequences.

#### A Brief History of Programming Paradigms

##### Before E.D. (Edsger W. Dijkstra)

If you never saw a program in FORTRAN 66, please [take a look](https://en.wikibooks.org/wiki/Fortran/Fortran_examples).
There you may notice the extensive use of the "GO TO" operator without any hesitation.

In his book "Structured Computer Organization" Andrew S. Tanenbaum also offers a good description of what
programming looked like at the dawn of the computer era:

<blockquote style="font-size: 95%">

<p>
In these early years, most computers were "open shop," which meant that the programmer had to operate the machine
personally. Next to each machine was a sign-up sheet. A programmer wanting to run a program signed up for a block of
time, say Wednesday morning 3 to 5 A.M. (many programmers liked to work when it was quiet in the machine room).
When the time arrived, the programmer headed for the machine room with a deck of 80-column punched cards
(an early input medium) in one hand and a sharpened pencil in the other. Upon arriving in the computer room, he or she
gently nudged the previous programmer toward the door and took over the computer.
</p>
<p>
If the programmer wanted to run a FORTRAN program, the following steps were necessary:
</p>
<ol>
<li>He went over to the cabinet where the program library was kept,
   took out the big green deck labeled FORTRAN compiler, put it in the card reader, and pushed the START button.</li>
<li>He put his FORTRAN program in the card reader and pushed the CONTINUE button. The program was read in.</li> 
<li>When the computer stopped, he read his FORTRAN program in a second time. Although some compilers required only one
   pass over the input, many required two or more. For each pass, a large card deck had to be read in.</li>
<li>Finally, the translation neared completion. The programmer often became nervous near the end because if the compiler
   found an error in the program, he had to correct it and start the entire process all over again. If there were no
   errors, the compiler punched out the translated machine language program on cards</li>
<li>The programmer then put the machine language program in the card reader along with the subroutine library deck
   and read them both in.</li>
<li>The program began executing. More often than not it did not work and unexpectedly stopped in the middle.
   Generally, the programmer fiddled with the console switches and looked at the console lights for a while.
   If lucky, he figured out the problem, corrected the error, and went back to the cabinet containing the big green
   FORTRAN compiler to start over again. If less fortunate, he made a printout of the contents of memory,
   called a core dump<sup>†</sup>, and took it home to study.</li>
</ol>

<p>This procedure, with minor variations, was normal at many computer centers for years. It forced the programmers to learn
how to operate the machine and to know what to do when it broke down, which was often. The machine was frequently idle
while people were carrying cards around the room or scratching their heads trying to find out why their programs were
not working properly.</p>

</blockquote>

† Reference to [magnetic-core memory](https://en.wikipedia.org/wiki/Magnetic-core_memory).

At least in the early versions of FORTRAN, you were not too restricted and could do anything in 
an arbitrary way by any existing means. There also were not so many of them.
This resulted in barely intelligible code and frequent errors.

##### Structured Programming: GOTO Considered Harmful

To help this gloomy state of affairs [Edsger Dijkstra](https://en.wikipedia.org/wiki/Edsger_W._Dijkstra) promoted
[structured programming](https://en.wikipedia.org/wiki/Structured_programming) as a discipline to adhere only to
the structured control flow constructs. This was the first major step in the taming of complexity, but it had its own major
drawback: the global program state was exposed, which does not help to minimize coupling. Because this is
undoubtedly harmful, a new technique that addresses this problem was necessary.

##### Object-Oriented Programming

Object-oriented programming introduced three new concepts that greatly aid in the minimization of coupling:

* [Encapsulation](https://en.wikipedia.org/wiki/Encapsulation_(computer_programming)) - also known under the more
  general name: [information hiding](https://en.wikipedia.org/wiki/Information_hiding), which allows hiding module
  internal state, facilitating decoupling.
* [Subclassing](https://en.wikipedia.org/wiki/Inheritance_(object-oriented_programming)) - class inheritance. Subclassing
  facilitates code reuse while maintaining encapsulation.
* [Polymorphism](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)) - ability to call methods of a derived
  class through a reference of a superclass type, which, for example, allows
  [dependency inversion](https://en.wikipedia.org/wiki/Dependency_inversion_principle).

Although OOP had tremendous success and remains a dominant programming paradigm to this day, it is not free of its own
problems inherited from the procedural style, such as uncontrollable side effects, which, for example, make parallelism
a non-trivial task. Because of that, adepts of functional programming consider object-oriented
programming harmful, probably, not without a reason. Being a great and simple tool for managing complexity, OOP requires
equally great experience and discipline for its proper use.

##### Generic Programming

In [strongly-typed](https://en.wikipedia.org/wiki/Strong_and_weak_typing) languages, generic programming allows generating
multiple type-parametrized versions of the code from a single template. This helps to observe the DRY principle.
Being a simple approach by itself, generic programming requires mastering of several related concepts to unleash 
its full potential:

* [Covariance and contravariance](https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science))
  in Java-based languages.
* [Template specialization](https://en.wikipedia.org/wiki/Partial_template_specialization) and template type deduction in
  C++ (which, for example, includes [universal reference](https://isocpp.org/blog/2012/11/universal-references-in-c11-scott-meyers) collapsing).
* [SFINAE](https://en.wikipedia.org/wiki/Substitution_failure_is_not_an_error) in C++, which is utilized in the
  [template metaprogramming](https://en.wikipedia.org/wiki/Template_metaprogramming).

Because extensive use of generic programming may result in uncontrollable binary code bloat, 
it is often considered harmful by byte-counting programmers.

##### Aspect-Oriented Programming

Something is still rotten in your kingdom, say proponents
of [aspect-oriented programming](https://en.wikipedia.org/wiki/Aspect-oriented_programming). You have cross-cutting
concerns (this is, obviously, a technical term), such as logging or transaction management, that dangle from here and
there, and pollute your code. Let's take them into a single place using [advices](https://en.wikipedia.org/wiki/Advice_(programming))
(this is a technical term that may be familiar to Lispers), which will result in a better separation of concerns. Other
programmers are shrugging, and consider this paradigm if not harmful, but of limited use, since pointcuts (this is yet
another technical term, denoting places where advices are applied) often do not provide the same granularity as the direct use
of the functionality being abstracted away. So, it may be appropriate primarily in monstrous enterprise systems with a large number 
of coarse-grained concerns.

##### Metaprogramming

Metaprogramming, which allows to programmatically generate any code, be it template-based, dynamically evaluated, or implemented through
[syntactic macros](https://en.wikipedia.org/wiki/Macro_(computer_science)#Syntactic_macros),
produces abstractions of such depth and eloquence, that it often results in the apparent elimination of complexity
through the introduction of obscurity (opacity of which is limited only by the creativity of the author), both in the places
of abstraction use and implementation. Because of this, metaprogramming is usually considered harmful by the programmers
that are not accustomed to it.

##### Functional Programming

Although functional programming was not developed as a direct response to the problems of OOP and takes its roots in
[category theory](https://en.wikipedia.org/wiki/Category_theory) and [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus)
of Alonso Church, it can successfully eliminate some hurdles of OOP by severely restricting programmers at the level of
language. For example, side effects are prohibited in most places, and looping constructs are replaced by recursion,
which is usually done through folding. Abstractions are built using higher-order functions and functional composition. To
successfully use the functional approach, a programmer needs to learn how to bypass imposed restrictions (often referenced
by the technical term "purity") by utilizing
and [accustoming](https://hub.packtpub.com/a-five-level-learning-roadmap-for-functional-programmers) to a dozen of
mind-boggling concepts with gut-wrenching names, such as:

* [Immutable data structures](https://en.wikipedia.org/wiki/Persistent_data_structure)
* [Referential transparency](https://en.wikipedia.org/wiki/Referential_transparency)
* [Functional composition](https://en.wikipedia.org/wiki/Function_composition_(computer_science))
* [Algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type)
* [Combinators](https://en.wikipedia.org/wiki/Fixed-point_combinator)
* [Currying](https://en.wikipedia.org/wiki/Currying)
* [Folding](https://en.wikipedia.org/wiki/Fold_(higher-order_function))
* [Monoids](https://en.wikipedia.org/wiki/Monoid_(category_theory))
* [Monads](https://en.wikipedia.org/wiki/Monad_(functional_programming))
* [Functors](https://en.wikipedia.org/wiki/Functor_(functional_programming))
* etc.

If you already have extensive OOP experience, the only way to master FP is to use only (and only) languages that
enforce purity, such as [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language))
or [ML](https://en.wikipedia.org/wiki/ML_(programming_language)) family of languages. But because the ability of the pure FP to
reduce cognitive effort is questionable for any real-world application, OOP programmers often consider it harmful,
probably, not without a reason. In moderate amounts, though, it can produce truly elegant solutions.

You may try to convince yourself in this by performing the following steps exactly in the order listed (the list may be a
little opinionated):

* Read "The Joy of Kotlin" by Pierre-Yves Saumont, which thoroughly describes the basics of functional programming and 
does a tremendous job to demonstrate how ugly it may look being practiced with an imperative programming language.

* Read "Functional Programming in Scala" by Paul Chiusano and Runar Bjarnason, which tries to teach more advanced 
functional patterns and shows how an ambiguous unintuitive syntax of a hybrid programming language may obscure
clear enough concepts.
  
* Read "Real World Haskell" by Bryan O'Sullivan, Don Stewart, and John Goerzen, which tells how it is painful and futile
to build something more complex than "Hello, World!" in a purely functional way without industry-grade tooling and frameworks.
In this sense, the book has lost absolutely nothing for almost 15 years of its existence.
  
* "Functional Programming Made Easier" by Charles Scalfani adds a yet another dimension of despair. In two thousand
  pages of fine print, it explains that you can not skillfully build functional abstractions without the knowledge
  of category theory and attempts to teach you some of it along the way.
  
* If you are still struggling, read [Professor Frisby's Mostly Adequate Guide to Functional Programming](https://github.com/MostlyAdequate/mostly-adequate-guide)
which demonstrates how to do things in a purely functional way in JavaScript.

* Now, after you have learned how to compose comonads and write interpreters for domain-specific languages based on algebraic types,
you are ready to obtain the black belt of functional programming. But if there are tools that allow achieving the same with much less
headache and with much more fun, such as [Clojure](https://clojure.org)...
                                 
##### Contract Programming

Design by contract, elements of which could be found in [Eiffel](https://en.wikipedia.org/wiki/Eiffel_(programming_language)),
[Clojure](https://en.wikipedia.org/wiki/Clojure), and [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))
programming languages, aims to eliminate implicit properties of module interfaces and related obscurity by explicitly defining them in
interface contracts, for example, in the form of pre- and post-conditions of function calls, that are programmatically verifiable.
Being by itself a good academic concept, it is often considered harmful by industrial programmers, who face permanently changing
requirements and also have unit tests to maintain.

##### Reactive Programming

Originally, this approach was focused on easing the creation of UIs by eliminating the
mess of wiring between application event handlers. But because it often employs parallelism, and the lion's share of
parallelism-related complexity is hidden beneath the libraries or language
constructs, [reactive programming](https://en.wikipedia.org/wiki/Reactive_programming) is often sold as a separate
paradigm on the complexity-management and related markets, which allows improving application responsivity.

Parallelized reactive programming is where you offload multiple complex tasks from the main thread of execution to a thread pool
(or a [multiprocessing](https://en.wikipedia.org/wiki/Multiprocessing) environment) and wait for their completion by
using [futures or promises](https://en.wikipedia.org/wiki/Futures_and_promises)
or a subscriber/publisher framework which utilizes a message queue that is called a reactive event stream. You can
filter, modify or combine event streams together. 

While events are processed, it is possible to display beautiful ads to the user in full-HD and 60 FPS, until the results are ready. 
Unfortunately, if your business is ad-based, this approach may harm it, since a parallelized set of tasks usually executes faster 
than the same set of sequential ones, so there may be places where it is considered harmful. In other areas, it is considered
harmful because it may introduce an unnecessary bloat.

##### Domain-Specific Languages

[Domain-specific languages](https://en.wikipedia.org/wiki/Domain-specific_language) allow creating reasonably deep
and clear abstractions of such quality, that sometimes they could be used even by non-specialists in computer science.
Although, the development of a good DSL, based on metaprogramming or special tools, such as [Xtext](https://en.wikipedia.org/wiki/Xtext)
or Spoofax, usually requires the amount of effort that is an order of magnitude (or two) higher than that for a regular OOP solution.
So, more than often a DSL results in semantics, that only its authors can understand. Thus, more than often DSLs are
considered harmful.

#### Anti-patterns

* [Optional types](https://en.wikipedia.org/wiki/Option_type) - often offered as a solution of the
  [billion dollar mistake](https://en.wikipedia.org/wiki/Tony_Hoare#Apologies_and_retractions). Such types wrap a value that
  may contain a null-reference. Since they heavily rely on functional programming concepts, their use may look obscure
  and ugly, especially in languages without built-in [pattern matching](https://en.wikipedia.org/wiki/Pattern_matching),
  such as Java. A clever solution, implemented, for example, in [Kotlin](https://en.wikipedia.org/wiki/Kotlin_(programming_language))
  is just to prohibit the use of null-references for non-nullable types and provide nullable ones with
  [null-coalescing operators](https://en.wikipedia.org/wiki/Null_coalescing_operator).

### Structural Considerations

#### SOLID Principles

These five related principles are considered a crucial discipline in the building of abstractions that are clear,
flexible, and maintainable:

* The [Single-responsibility principle](https://en.wikipedia.org/wiki/Single-responsibility_principle) -
  "A module should have only one reason to change." This principle facilitates the separation of concerns and low coupling.

* The [Open–closed principle](https://en.wikipedia.org/wiki/Open%E2%80%93closed_principle) -
  "A module should be open for extension, but closed for modification." This principle facilitates the stability of the
  module interface to minimize the impact of changes. It is not surprising, that stable interfaces are usually
  well-thought-out.

* The [Liskov substitution principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle) -
  "A module of a given type should be interchangeable with a module of its subtype without any unexpected side effects."
  This principle promotes the consistency of behavior in type hierarchies, which results in simple and clear 
  abstraction semantics.

* The [Interface segregation principle](https://en.wikipedia.org/wiki/Interface_segregation_principle) -
  "No client of a module should be forced to depend on methods it does not use." This principle facilitates the separation 
  of concerns (interfaces) and high cohesion, increasing code maintainability.

* The [Dependency inversion principle](https://en.wikipedia.org/wiki/Dependency_inversion_principle) -
  "A client module should not depend on other modules directly, but through abstract interfaces." 
  The use of the term "inversion" in the name of this principle is usually explained by the fact that dependencies
  between modules are customary made to concretions. The aim of this principle to facilitate dependencies to 
  abstractions. It may make sense in the case of a concretion that is highly volatile
  and may propagate undesired changes. An abstract interface is created, that hides such concretion, which also
  eliminates the direct dependency on the referred module source code, increasing decoupling. 
  Abstract factories are usually used to create instances of such volatile objects.

All these principles are described in more detail in the books by Robert C. Martin, for example, in
"Clean Architecture: A Craftsman's Guide to Software Structure and Design". The name of this book may be misleading
because this is a book not about architecture but about tidiness.

#### The Law of Demeter

[The Law of Demeter](https://en.wikipedia.org/wiki/Law_of_Demeter) facilitates the abstinence of the dependence on
the inner structure of the used classes and components. If there is such a dependence, it is better to be reimplemented
as an incapsulated method of the dependent class that utilizes this functionality.

#### GRASP Principles

These are nine less-known principles, that are focused on assigning responsibilities to objects
([General Responsibility Assignment Software Patterns](https://en.wikipedia.org/wiki/GRASP_(object-oriented_design))).
They are also worth to consider since responsibilities are a common currency in the object-oriented world.

* Information expert - a responsibility is preferably assigned to the class that has all necessary information to fulfill it.

* Creator - this pattern determines which class should have the responsibility to create some other class. Several possible
  concerns apply here:
    * Instances of the creator contain or compositely aggregate instances of the created class.
    * Instances of the creator persist instances of the created class.
    * Instances of the creator closely use instances of the created class.
    * Instances of the creator have the initializing information for instances of the created class and pass it on creation.

* Controller - in this pattern, some objects are assigned the responsibility to process non-UI events of the application.
  This allows separating application-specific logic from the presentation- and business-logic.

* Indirection - this is a more general pattern, similar to the GoF [mediator](https://en.wikipedia.org/wiki/Mediator_pattern)
  design pattern. For example, the controller in MVC acts as a mediator between the presentation and model layers that
  do not communicate directly and hence are decoupled.

* Low coupling and High cohesion patterns are equivalent to the source code metrics discussed above, which should be invoked
  when considering the assignment of responsibilities (behaviors).

* Polymorphism - this pattern facilitates the use of the built-in polymorphic constructs over hardcoded
  switch/case tables when assigning responsibilities, for example, as in GoF [visitor](https://en.wikipedia.org/wiki/Visitor_pattern)
  pattern, to ease a possible refactoring (although visitor, probably, is the most complex pattern in the GoF line).

* Protected variations - this pattern facilitates isolation of possible sources of variation under the abstract interfaces,
  which can represent different (changeable) polymorphic classes, so instability does not propagate to the other parts of the system.

* Pure fabrication - is an artificial module, that does not present in the system model and is introduced solely to maintain
  abstraction and decoupling.

GRASP principles are described in more detail in the book "Applying UML and Patterns: An Introduction to
Object-Oriented Analysis and Design and Iterative Development" by Craig Larman.

#### GoF Design Patterns

First published in the book "Design Patterns: Elements of Reusable Object-Oriented Software" (1994) by
Erich Gamma, Richard Helm, Ralph Johnson, and John Vlissides who are also known as the Gang of Four (GoF), these patterns
comprise a clever set of abstractions that eliminate many undesirable side effects of naive software design.
Since many excellent books are written on this topic, I will not discuss them. For example, [here](http://www.vincehuston.org/dp/),
you may find a good brief description of each pattern, of the problem it solves, and the corresponding code examples.

#### Anti-Patterns

* [Excessive use of inheritance instead of composition](https://en.wikipedia.org/wiki/Composition_over_inheritance).
  This anti-pattern is a classical example of an undesirable side effect of the straightforward software design:
  combinatorial breeding of the derived classes. For example, your goal is to draw N different shapes by M different
  algorithms that are applicable to each shape. If you want to utilize polymorphism to draw the shapes, you may create a base class `Shape` and
  derive a Cartesian product (MxN) of shape subclasses from it, each drawn by its own algorithm, for example: `Shape1Alg1`,
  `Shape1Alg2`, `Shape2Alg1`, `Shape2Alg2`, ... etc. Although, if you utilize GoF [bridge](https://en.wikipedia.org/wiki/Bridge_pattern)
  pattern, where algorithms are invoked using composition, you will need only M+N derived classes for your
  implementation, while still using separation of concerns and polymorphism on the Shape subclasses.


### Software Quality Attributes

Earlier we implicitly invoked some [software quality](https://en.wikipedia.org/wiki/Software_quality) attributes, such as
simplicity, reusability, maintainability, clarity, flexibility, correctness, and evolvability. 
To understand what may act as major considerations in the choice of software architecture, and also by which aspects 
it may be constrained or evaluated, we need more examples:

* Granularity - the number of physical nodes needed to deploy an architectural solution.
* Performance - implies system performance characteristics such as throughput and response times.
* Scalability - the extent to which the system is capable of growing after its initial deployment.
* Elasticity - the ability of a system to add and remove capacity based on demand.
* Availability - addresses system failure and its impact on users or other systems.
* Resilience -  the ability to provide and maintain an acceptable level of service in the face of faults and challenges to normal operation.

### Software Architecture

Software architecture is often described as a high-level system design, which operates on the level of system layers and
subsystems, rather than on the level of separate modules or classes. 

In the narrowest sense, software architecture is the art of maintaining dependencies and boundaries between coarse-grained
system components to keep the system testable, maintainable, and evolvable. To do this you need to learn a set of 
*architectural patterns*, described, for example, in the books: "Java Application Architecture" by Kirk Knoernschild, or
now mostly obsolete "Patterns of Enterprise Application Architecture" by Martin Fowler.

In the broader sense, software architecture is an engineering discipline that is concerned with keeping the system in
accordance with various criteria and requirements (mostly non-functional). At this particular level, the trick is to
choose the right method or tool, that is the most appropriate to the problem at hand. To master this you need to learn a
set of *architectural styles*, which are described in the books: "Fundamentals of Software Architecture" by Mark
Richards, Neal Ford, or also in [AOSA](https://aosabook.org/en/index.html).

Although many divisions are possible, system architecture may be viewed from the five different standpoints:

* View of use-cases - this is a functional requirements view, which is an input to develop the software architecture.
  Each use case describes the sequence of interactions between one or more actors (external users) and the system.
* Static view - the architecture is depicted in structural terms of subsystems or components and relationships between them.
* Dynamic view - this view described dynamic interactions between components comprising the system.
* Deployment view - this view depicts a specific configuration of the distributed architecture
  with components assigned to hardware nodes.
* Non-functional requirements view - evaluates non-functional architecture properties such as performance or resilience.

Design on this level generally requires a level of knowledge in technology, soft skills, and modeling that differs
from the skills of a regular software developer. Although, there is an [alternative](https://blog.pragmaticengineer.com/software-architecture-is-overrated/)
point of view.

#### Architectural Patterns

The domain of software architecture has its own repository of patterns which is discussed in the
"Pattern-Oriented Software Architecture" book series or "Architectural Patterns" by Pethuru Raj, Anupama Raman, and Harihara Subramanian.

#### Architecture Styles

Monolithic and distributed architectures represent two opposite approaches, the choice between which is often driven
by the non-functional requirements. Distributed architectures may employ centralized or decentralized coordination,
which is referred to by the technical terms "orchestration" and "choreography" respectively.
Architectures also may be technology- or domain-partitioned, in which
decomposition to components is performed according to the corresponding paradigm. The most common architectural styles
used nowadays are listed below.

* [Layered architecture](https://en.wikipedia.org/wiki/Multitier_architecture) - components within the layered
  architecture are organized into logical horizontal layers, with each layer performing a specific role within the
  application. Most layered architectures consist of four standard layers: presentation, business logic, persistence,
  and database. This is an example of a technologically-partitioned architecture.

* [Pipeline architecture](https://en.wikipedia.org/wiki/Pipeline_(software)) - components within this architecture
  perform only one task and are sequentially connected by mediator components ("pipes") which form a single pipeline
  with input and output ends. Widely used in applications that require simple one-way data processing.

* Microkernel architecture (also known as plug-in architecture) - is a monolithic architecture that consists of the core
  that provides minimal functionality necessary to run
  the whole system and of multiple plugin components that implement the main application functionality. Eclipse IDE is a good
  example of such an application. Often used in [software product lines](https://en.wikipedia.org/wiki/Software_product_line).

* [Service-based architecture](https://en.wikipedia.org/wiki/Service-oriented_architecture) -
  is a distributed architecture that may consist of a separately deployed presentation node, a set of coarse-grained
  domain-partitioned service nodes, and a monolithic database. A very popular choice for many business-related
  applications.

* [Event-driven architecture](https://en.wikipedia.org/wiki/Event-driven_architecture) - is made up of decoupled event
  processing components that asynchronously receive and process events. The components may be deployed as a clustered
  domain-partitioned set of nodes with centralized or decentralized coordination. This architecture is used in
  highly-scalable responsive and resilient applications.

* [Space-based architecture](https://en.wikipedia.org/wiki/Space-based_architecture) - consist of many processing units
  that contain application logic, virtualized controlling middleware, asynchronous data cache, and a database that
  receives updates from users asynchronously, possibly with a delay. The main aim of this architecture is to overcome the
  bottlenecks that emerge when traditional web-server deployments are trying to synchronously process requests from many users.
  A typical use case is a concert ticket ordering system, which is idle most of the time, but elastically handles
  high loads before concerts by deploying as many processing units as necessary.

* [Microservices architecture](https://en.wikipedia.org/wiki/Microservices) - this architecture takes its roots in
  the domain-driven design and consists of a large set of maximally decoupled domain-partitioned fine-grained services,
  each of which implements a bounded context, or an individual aggregate from the domain model. Each service
  is meant to be independent, which implies that every service usually contains its own database. Because this architecture
  is scalable, elastic, and maintainable, it is a popular choice for modern distributed applications.

Software architecture styles are described in more detail in the book "Fundamentals of Software Architecture: An Engineering Approach"
by Mark Richards and Neal Ford.

#### Anti-Patterns

* [Big ball of mud](https://en.wikipedia.org/wiki/Big_ball_of_mud) - an architecture (or the lack of it), where components
  are not divided into layers or services and promiscuously depend on each other.
  

### Recommended Literature

The literature listed below offers a real introduction to software engineering. Its choice is somewhat arbitrary, but
many books contain references to other works. Only after reading it all, one can approach the beginning of the
understanding of system design essentials. Some book titles may seem misleading, but every book there is dedicated, 
at some degree, to important foundational concepts in software design. 

* Andrew S. Tanenbaum, Todd Austin - Structured Computer Organization
* Andrew S. Tanenbaum, Bos Herbert - Modern Operating Systems
* Andrew S. Tanenbaum, Nick Feamster, David J. Wetherall - Computer Networks
* Steven S. Skiena - The Algorithm Design Manual
* Aditya Bhargava - Grokking Algorithms
* Robert C. Martin - Clean Code: A Handbook of Agile Software Craftsmanship
* David Thomas, Andrew Hunt - The Pragmatic Programmer: Your Journey to Mastery
* John Ousterhout - A Philosophy of Software Design
* Erich Gamma, Richard Helm, Ralph Johnson, and John Vlissides - Design Patterns
* Eric Freeman, Kathy Sierra, Bert Bates, Elisabeth Robson - Head First Design Patterns
* Joshua Kerievsky - Refactoring to Patterns
* Clean Architecture: A Craftsman's Guide to Software Structure and Design
* Craig Larman - Applying UML and Patterns: An Introduction to Object-Oriented Analysis and Design and Iterative Development
* Hassan Gomaa - Software Modeling and Design: Uml, Use Cases, Patterns, and Software Architectures
* Mark Richards, Neal Ford - Fundamentals of Software Architecture: An Engineering Approach
* Pethuru Raj, Anupama Raman, and Harihara Subramanian - Architectural Patterns   
* Andrew S. Tanenbaum, Maarten van Steen - Distributed Systems: Principles and Paradigms
* Andrei Alexandrescu - Modern C++ Design: Generic Programming and Design Patterns Applied
* Eric Evans - Domain-Driven Design: Tackling Complexity in the Heart of Software
* Harold Abelson, Gerald Jay Sussman, Julie Sussman - Structure and Interpretation of Computer Programs
* Michael Swaine - Functional Programming: A PragPub Anthology: Exploring Clojure, Elixir, Haskell, Scala, and Swift
* Michael Fogus, Chris Houser - The Joy of Clojure
* Scott Wlaschin - Domain Modeling Made Functional: Tackle Software Complexity with Domain-Driven Design and F#
* Martin Fowler - Domain-Specific Languages
* Ryan D. Kelker - Clojure for Domain-Specific Languages
* Markus Voelter - DSL Engineering: Designing, Implementing and Using Domain-Specific Languages
* Tomasz Nurkiewicz, Ben Christensen - Reactive Programming with RxJava

The devil is in the details. Happy studying!