# Substratic Engine

Substratic Engine is a 2D game engine written for use in [Gambit
Scheme](http://gambitscheme.org/).  At its core is a scene graph with nodes
composed of components, all written in a functional style.  Communication
between components and nodes is accomplished through events that are propagated
across the graph.

Game code written with this library can be developed interactively with the use
of an embedded REPL.  An Emacs package is being developed to enable a developer
to automatically connect to the REPL of the running game and utilize more
advanced editor integration through a secondary RPC channel.

All of this will be explained in more detail as the project matures.

**DISCLAIMER:** This is a personal side project and isn't meant for general use
(yet).  I'd be happy to hear feedback from anyone willing to try it out but I
can't guarantee any level of support at this point.  Also, beware of sudden API
changes, I'm still figuring out how everything should work.

## License

Substratic Engine is licensed under the [Mozilla Public License
2.0](https://www.mozilla.org/MPL/2.0/), see [LICENSE](LICENSE) for more details.
