# Gigan


### What is this?

Gigan is an application programming framework for Elm. It's goals are as follows:

1. Supply a fully functioning component system with the prettiest syntax possible.
2. Provide a dynamic environment in which to construct the view output of a component so that any large views can be efficiently updated in a scalable way.
3. Supply a system with a rich and comprehensive DSL that models and maps data or components in a succinct and powerful nestable way. Ultimately, the intent is to, at worst, at least have the _ease of use of_ the active record pattern.


### How do I use it?

A comprehensive set of runnable examples is underway. Right now, the only working example is a full
production application that will be released before the end of April, 2016. Time should allow me to
complete the examples sometime before that as well.

In the meantime, if you are the sort of person who likes to study, then I suggest reading the
documentation and code thoroughly, as a lot is explained, albeit in overwhelming volume to the
uninitiated. The examples are obviously intended to cure this and lend these modules some
credibility.


### Is it done?

Never. Basically. Sort of. Everything contained herein works. That, however, does not have any
bearing on whether or not it is done. There are further development plans beyond the examples for
this first usable edition. These include:

1. Convert Layout to use HTML instead of Graphics, given that Graphics is no longer under active development to the best of my knowledge. To avoid forcing the version up again, this will be placed in a new module called Layout2.
2. Build a set of database back-end adapters for the Knowledge system for general use. Currently, we have one for internal use that I do not believe is clean enough to give out as-is. For this reason, Gigan's provided back-end and localstorage adapters for knowledge base will be released only when deemed fit to share. This is also likely to happen soon, within two months.


### Getting Started

    todo
