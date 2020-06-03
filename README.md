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

## Documentation

In lieu of more formal documentation (coming at a later date), here is a
high-level overview of the engine and its constituent concepts:

### Overview

Substratic Engine is a functional game engine that controls mutation through a
uniform flow of game state through the game loop.

The state of the game engine is treated as a scene graph stemming from a single
root node.

### Nodes

A node is a generic entity in the game engine which contains the following:

- An ID and type
- Keys containing further state that pertains to attached components
- Lists of "methods" for updating, rendering, and handling events for that node

Node state is treated as immutable by the game engine.  Each method can return
an updated state object which then gets passed on to other methods in the engine
and ultimately the engine's next loop cycle.

Interactions between nodes that must cause changes in state are effected by
events.  A component in one node can send an event to be handled by a method in
another node causing a change in its state to be returned by that event handler
and flowed through to the next cycle.

You may have noticed that nodes don't have a direct concept of "children".  This
is because nodes don't have inherent state other than what components apply to
them.  A component will decide whether a node has children, and two different
components attached to a node may have different children.  This enables a more
flexible scene graph than what would be achieved if a node had its own concept
of children.  This does imply, though, that components need to handle
dispatching method calls to their children.  We may provide some convenience
functions to simplify this in the future.

### Methods

There are currently 3 different types of methods that can be attached to nodes.
These methods may come from components or may be standalone methods that can be
attached to a node ad hoc.

- `updater` - updates a node's state with respect to time
- `handler` - handles an event that reached the node
- `renderer` - renders a node based on its current state

Each node has a list of methods for each type that are executed in order in the
associated context.  This is how behavioral composition is achieved: components
can add their own `updater`, `handler`, and `renderer` methods that get invoked
each cycle to result in more complex behavior.

More method types may be added in the future to add custom behavior for other
engine lifecycle events.

#### Method Context

Aside from node state, there is another type of state that is passed through all
methods during a single cycle: method context.  Method context provides
transient contextual information that is needed for methods to operate.  This
kind of state is only passed down through children in the scene graph and never
stored for future cycles.

One universal bit of contextual information is the screen width and height.
Many updaters, handlers, and renderers need to know the screen dimensions so
that they can make the necessary calculations for their behavior.  Other more
game-specific context can be passed down by updating the method context object
before passing it to the methods of child nodes.

### Components

As a concept, a component is a logical unit of state and behavior that can be
applied to a node.  In practice, a component is a function that adds nested
state and methods to a node so that they are used in future loop cycles.

A component may only register state or methods, it does not have to contain
both.  A component will usually only register the type of methods that it needs
to function.

By convention, a component's state is stored under a key with the same name
inside of the node's state.  However, a component can register multiple methods
of the same type which may not have a name consistent with the component itself.
Components who do this may need to provide a method to enable removal of all
methods from the node.

A component's methods may depend on the state of other components that are
attached to the node.  For example, the `movement` component depends on the
state of the `position` component.  To reduce overhead, components are currently
allowed to manipulate the state of another component in the same node.  This may
change in the future if it is decided that this pattern leads to difficult bugs.

### Events

Events are signalled through a function called an "event sink".  This event sink
function is passed to all `updaters` and `handlers` so that events can be raised
in response to game logic.

A node may decide to reduce the scope of event propagation by inserting its own
event sink when passing to its children.  It could also wrap the engine's event
sink function to intercept events or manipulate them before they are dispatched
to children.

When the engine starts a new cycle, it dispatches any new events that were
received from SDL (keyboard, mouse, controller, etc) to the root node.  The
dispatcher will call all handler methods on the root node,
possibly causing handlers of components' children to be invoked.

One interesting aspect of event dispatch is how event response patterns operate
within a single engine cycle.  Each handler may
send more events using the event sink.

### Engine Loop

The main game engine loop is only concerned with a single node, the root node.
The engine loop follows these steps:

1. Dispatch any events received from SDL2 or the RPC channel (if enabled)
2. If after dispatching events the root node is marked with an engine state
   change, apply it:

   - If the engine needs to quit, clear the root node so that the engine exits
   - If the root node needs to switch to a new root, replace the current root
     node for the current cycle

3. Run the updaters for the root node
4. Run the renderers for the root node
5. Update the screen via SDL2
6. Invoke the next frame of the cycle with the final state for the current frame

Generally each node in the scene graph will repeat a variation of the same
`handler` -> `updater` -> `renderer` cycle, returning the updated state all the
way back up through the root node's methods.

## License

Substratic Engine is licensed under the [Mozilla Public License
2.0](https://www.mozilla.org/MPL/2.0/), see [LICENSE](LICENSE) for more details.
