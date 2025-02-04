# event-system

This is an Haskell library used to decouple interactions between the main code flow and secondary interaction.

This is done through events.

## Event

An event is a fact happened in the past, like `NewCustomerSubscribed`, `ItemAddedToCart` or `FirstPrizeWon`.
The two main components of an event are its name, or tag, and its payload, i.e. a set of data which is shipped with the event itself.

## Dispatching

The event system can be used to dispatch events to interested listeners.

An event does not have a predefined recipient, the event system dispatches it to all the interested listeners

## Listeners

A listener declares to the event system that it is interested in being notified when an event is triggered. When that happens, the listener can react by invoking a callback which receives as arguments the data contained in the event payload.

## Sync and async

A listener could be synchronous or asynchronous.

In the former case, its callback will be invoked immediately.

In the latter case, the callback will be invoked at a later, possibly more convenient time. This could be needed because the callback is resource intensive and we want to run it when more resources are free, or just because we don't want to delay the main process.

## Event persistence

In case of an asynchronous listener, events need to be persisted by the event system so that they could be processed at a later time.
