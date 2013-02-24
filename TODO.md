
An unsorted list of things that could be done.

* Replace the Pos data type

This should be replaced by one of the "standard" cartesian coordinate
types on hackage. The idea would be to easily allow operations like
rotation and translation.

* Combine BlockData and BlockType

Should we have something like the following?

```haskell
data BlockInfo = BlockInfo BlockData (Maybe BlockType)
```

* Representing shapes

Since the existing rendering functions create either a position or a
cuboid, does it make sense to encode this as something like

```haskell
data Shape = BlockList BlockInfo [IPos]
           | BlockCuboid BlockInfo IPos IPos
```

so that we can then write routines like

```haskell
draw :: [Shape] -> MCPI ()
xshift :: Int -> [Shape] -> [Shape]
data Circle = Circle { circleShape :: [Shape], ... }
```

An alternative is an approach like the
[diagrams package](http://hackage.haskell.org/package/diagrams).

* Is the current MCPI monad sufficient?

Do we want to allow the user to know that there is no running
MineCraft-PI program, rather than just exiting? It probably
makes sense to expose the open/close connection primitives.

Is it worth using operational, or a Free Monad, here?

* Support more of the API

There is no support for multiple players or checkpoints, amongst other
items.
