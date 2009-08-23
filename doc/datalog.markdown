# An Overview of Datalog
By Jeffrey Straszheim

*What Datalog is, and what it can do for you.*

*Work in Progress*

## Introduction ##

Datalog is a logical query language.  It exists somewhere between relational algebra (the formal theory behind SQL) and Prolog, but is closer in motivation to the former than the later.  It was invented to apply some of the principles of logic programming to database theory.  Its primary addition to the semantics of databases is _recursive_ queries.  Examples will be provided below.

The implementation of Datalog that is provided (in this library) departs a bit from the original model insofar as it supports _in memory_ data structures only.  It is intended to give developers tools to use relational modeling for their data.  A good overview of why you would want to do this is Ben Mosely's _Functional Relational Programming_ material, found here: [http://web.mac.com/ben_moseley/frp/frp.html](http://web.mac.com/ben_moseley/frp/frp.html).

## Details ##

### The Database ###

Clojure Datalog supports an in memory relational database format, implemented in clojure.contrib.datalog.database ([here](http://github.com/richhickey/clojure-contrib/blob/master/src/clojure/contrib/datalog/database.clj)).  It supports relations (tables) with named columns and simple hash based indexes.  At the present time it does not support any integrity constraints (perhaps later).

Tables are built with `make-database`, like this:

    (make-database
          (relation :employee [:id :name :position])
          (index :employee :name)
    
          (relation :boss [:employee-id :boss-id])
          (index :boss :employee-id)
    
          (relation :can-do-job [:position :job])
          (index :can-do-job :position)
    
          (relation :job-replacement [:job :can-be-done-by])
    
          (relation :job-exceptions [:id :job]))

The schema can be modified by `add-relation` and `add-index`.  Under the hood, it is standard Clojure map from relation name to relation, and can be directly modified if needed.

Data is added like this:

    (add-tuples db-base
               [:employee :id 1  :name "Bob"    :position :boss]
               [:employee :id 2  :name "Mary"   :position :chief-accountant]
               [:employee :id 3  :name "John"   :position :accountant]
               [:employee :id 4  :name "Sameer" :position :chief-programmer]
               [:employee :id 5  :name "Lilian" :position :programmer]
               [:employee :id 6  :name "Li"     :position :technician]
               [:employee :id 7  :name "Fred"   :position :sales]
               [:employee :id 8  :name "Brenda" :position :sales]
               [:employee :id 9  :name "Miki"   :position :project-management]
               [:employee :id 10 :name "Albert" :position :technician]
               
               [:boss :employee-id 2  :boss-id 1]
               [:boss :employee-id 3  :boss-id 2]
               [:boss :employee-id 4  :boss-id 1]
               [:boss :employee-id 5  :boss-id 4]
               [:boss :employee-id 6  :boss-id 4]
               [:boss :employee-id 7  :boss-id 1]
               [:boss :employee-id 8  :boss-id 7]
               [:boss :employee-id 9  :boss-id 1]
               [:boss :employee-id 10 :boss-id 6]
    
               [:can-do-job :position :boss               :job :management]
               [:can-do-job :position :accountant         :job :accounting]
               [:can-do-job :position :chief-accountant   :job :accounting]
               [:can-do-job :position :programmer         :job :programming]
               [:can-do-job :position :chief-programmer   :job :programming]           
               [:can-do-job :position :technician         :job :server-support]
               [:can-do-job :position :sales              :job :sales]
               [:can-do-job :position :project-management :job :project-management]
    
               [:job-replacement :job :pc-support :can-be-done-by :server-support]
               [:job-replacement :job :pc-support :can-be-done-by :programming]
               [:job-replacement :job :payroll    :can-be-done-by :accounting]
    
               [:job-exceptions :id 4 :job :pc-support])

The meaning is, I believe, obvious.

Functions that add/remove individual tuples are also provided.  Use the source.


### Rules ###

In addition to the database itself, Datalog lets you define a series of _inference rules_ to apply to your data.  Rules look like this:

    (<- (:works-for :employee ?x :boss ?y) (:boss :employee-id ?e-id :boss-id ?b-id)
                                           (:employee :id ?e-id :name ?x)
                                           (:employee :id ?b-id :name ?y))

The `<-` operator represents implication.  The first form is the head of the rule, the remainder the body.  The meaning is that the head is true if each member of the body is true.  We can read this above rule as:

  * An employee (?x) works for a boss (?y) if: in the :boss relation there is an employee id (?e-id) matched to a boss id (?b-id) *and* in the :employee relation that (?e-id) matches the name (?x) *and also* in the :employee relation the id (?b-id) matches the name (?y).

Notice two things:  Logic variables are prefixed by a '?', and the meaning of those variables in a rule can join together entities.

That same rule might be expressed in SQL as:

    select e.name, b.name
       from employee as e, employee as b, boss
       where e.id = boss.employee-id and b.id = boss.boss-id

However, unlike SQL, Datalog rules can be recursive.  Like this:

    (<- (:works-for :employee ?x :boss ?y) (:works-for :employee ?x :boss ?z)
                                           (:works-for :employee ?z :boss ?y))

If you combine these two rules, this builds a _transitive closure_ of the works-for relation.  It will return not only ?x's direct boss, but the boss of his boss, and so on.  This cannot be done in most forms of SQL.

#### Negation and Conditionals ####

_Todo_

### Queries ###

A query is how you request information from a set of rules and a database.  Queries look like this:

    (?- :works-for :employee ??name :boss ?y)

Notice the double '?'!

This asks for the name and boss columns from the works-for relation, which was defined by the two rules above.  The double ?? allow you to parameterize your query.

### Work Plans ###

A set of work rules and a query can form a work plan.  It is done like this:

    (build-work-plan rules (?- :works-for :employee '??name :boss ?x))

This takes a set of rules and a query, and performs some basic filtering an optimization.  it is a fairly expensive operation, so try to build the plans you need once in your program, or perhaps cache them somehow.

In SQL, a work plan is similar to a prepared statement.

### Running your Work Plan ###

You run a work plan like this:

    (run-work-plan wp db {'??name "Albert"})

Where wp is the result of (build-work-plan ...) and db is a database.  The last argument is a map of bindings.  It provides the specific values for any ??X forms in your query.  Given the rules and query defined above, it should return a sequence of tuples of all the people that "Albert" works for.

### Examples ###

A completed example is provided at [http://github.com/richhickey/clojure-contrib/blob/master/src/clojure/contrib/datalog/example.clj](http://github.com/richhickey/clojure-contrib/blob/master/src/clojure/contrib/datalog/example.clj).


