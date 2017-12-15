# Prerequisites

This tutorial assumes you're already familiar with pure functional programming and specifically the language **Purescript**, it also assumes you know a little bit of React and finally (and most importantly) that you have some experience using lenses. I don't think you need to have a lot of experience on the latter as I don't have it and I was still able to build this framework so yeah... moving on.

If you don't know these concepts or feel you need to learn more, I found these tutorials very helpful and insightful when learning about them:

1. [Learning Purescript](https://leanpub.com/purescript/read) by Phil Freeman, the creator of Purescript.
2. [React tutorial](https://reactjs.org/tutorial/tutorial.html).
3. [React Elements vs React Components](https://tylermcginnis.com/react-elements-vs-react-components/) by Tyler McGinnis. This article is not a React tutorial but it explains essential React concepts that are good to know.
3. [Learning lenses](https://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html) by Gabriel González. While his tutorial is written in Haskell and focuses on lenses built on top of Functors and not on the more obscure Profunctors on which this framework is based on, the concepts he teaches are pretty much identical to what you'll use in Proact so go ahead and read it!
4. [Fun with Profunctors](https://www.youtube.com/watch?v=OJtGECfksds) Oh yeah, what the heck are Functors and Profunctors, right? Fortunately, you don't need to know about this if you want to use this framework but if you're still curious you can check this very insightful video-tutorial by Phil Freeman.

And that's it, ready to start.

# Building the to-do application

For this exercise we're going to build the popular to-do application (shamelessly copied/adapted from purescript-thermite's own [to-do application](http://try.purescript.org/?gist=f5f273e4c5e4161fceff&backend=thermite&session=6a8ae18b-cf74-8541-f2ff-bdb99819ffcf)) which has become a sort of a "hello world" for web applications. Proact is, I believe, a very simple framework and the way to use it will become very easy to understand with examples. At the end of this tutorial you should be able to create Proact components, integrate them as part of larger components, communicate them and change their state as a response to HTML events. You may notice that the programming model is very similar to imperative architectures such as Vue.js or Angular but without losing purity or state immutability, hopefully you'll see that as an advantage and not otherwise! :)

## But before we start...

Let's begin by adding some helper functions and aliases that will make our code look prettier:

The function (arrow) composition operator has already a symbol assigned to it `(<<<)` but it's annoyingly long and it's not as pretty as the dot operator which when using lenses makes your code look and feel as if it was written in an Object Oriented Programming language, pretty neat.

Unfortunately, though, we cannot actually redefine the dot symbol as an operator in Purescript so we'll choose the second best thing, two dots:

```purescript
o :: forall a b c d . Semigroupoid a => a c d -> a b c -> a b d
o = (<<<)
infixr 9 o as ..
```

Then we also include an implementation of `use` from the [Data.Lens](https://pursuit.purescript.org/packages/purescript-profunctor-lenses/3.8.0/docs/Data.Lens.Getter#v:use) library that works under the `MonadAsk` context instead of `MonadState`, we'll see why this is useful later.

```purescript
use' :: forall s t a b m . MonadAsk s m => Getter s t a b -> m a
use' p = asks (_ ^. p)
```

For this application, we'll very soon require to pass around event handlers from one component to another, to save us some typing we'll define a type alias for such handlers:

```purescript
type ReactHandler fx event = event -> Eff (ReactContext fx) Unit
```

`ReactContext` is a type alias from Proact.

Finally, we throw this last one in:

```purescript
_this :: forall _in out . Iso _in out _in out
_this = id
```

## Building our first component: A filtering menu

Our first component is, of course, the simplest: three buttons that act as radio buttons and filter our to-do tasks in the following manner: When we select "All" we expect all tasks to be displayed, when we select "Active" we only want to see tasks that are unchecked and finally when selecting "Completed" we expect to see only checked activities.

To design a Proact component we usually start by asking how does the state look like? In other words, what is the data that needs to be dynamic or be persisted throughout the application?

In the case of a filtering menu, the only piece of data we care about is the filtering option that the user selected for which we only need the following data structure to store:

```purescript
data Filter =
  Active
  | All
  | Completed

derive instance eqFilter :: Eq (Filter)

instance showFilter :: Show (Filter)
  where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

type State = Filter
```

In the code above we define the three possible filters and we also implement the Eq and Show type classes that will become useful later: When rendering a button for each filter we will need to display the name of the filter and when a filter is selected and we're rendering each button we will need to compare if the selected filter is equal to the filtering button we're currently rendering. Finally, we include a type alias just so that it's clear that the filter data structure is, in fact, the state of our component.

The second step is to design a GUI that renders according to the state we just defined. Let's do so below:

```purescript
data Filter =
  Active
  | All
  | Completed

derive instance eqFilter :: Eq (Filter)

instance showFilter :: Show (Filter)
  where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

type State = Filter

filterMenu :: forall fx . Component fx State ReactElement
filterMenu =
  do
  state <- ask
  pure $ view state
  where
  view state =
    div [ className "btn-group" ] $ map filterButton [ All, Active, Completed ]
    where
    filterButton filter =
      button [ className class' ] [ text $ show filter ]
      where
      class' =
        if filter == state
        then "btn toolbar active"
        else "btn toolbar"
```

We're finally introduced to the concept of a Proact Component that is, actually, an instance of the Monad type class. From within this context we access the state through the [MonadAsk](https://pursuit.purescript.org/packages/purescript-transformers/3.5.0/docs/Control.Monad.Reader.Class#t:MonadAsk) interface and pass it over to the view so that it renders accordingly.

The view is not super complicated:

1. We define a container div for the entire component and decorate it with the "btn-group" CSS class.
2. We iterate over all possible filtering options and render a button for each of them.
3. We decorate each filtering button with the CSS class "btn toolbar" and in the case that the button we're rendering is equal to the button that was selected by the user we decorate it with the additional CSS class "active".
4. Finally we display the name of the filter inside the button.

The next and final step is to define an event handler that responds to HTML events, specifically, we want to handle the clicking event from any of the filtering buttons and update the state accordingly. We do so in the following manner:

```purescript
data Filter =
  Active
  | All
  | Completed

derive instance eqFilter :: Eq (Filter)

instance showFilter :: Show (Filter)
  where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

type State = Filter

filterMenu :: forall fx . Component fx State ReactElement
filterMenu =
  do
  state <- ask
  dispatcher <- eventDispatcher
  pure $ view dispatcher state
  where
  view state =
    div [ className "btn-group" ] $ map filterButton [ All, Active, Completed ]
    where
    filterButton filter =
      button
        [ className class', onClick $ dispatcher onFilterChanged ]
        [ text $ show filter ]
      where
      class' =
        if filter == state
        then "btn toolbar active"
        else "btn toolbar"

      onFilterChanged _ = _this .= filter
```

The code above requests an event dispatcher from Proact that will be tied to state of the component it was called from. In fact we could send around this dispatcher to other modules and register event handlers in places outside the component definition provided they all work under the same state. We won't need to do this for the to-do application but it's something to keep in mind.

The event handler is this one-liner:

```purescript
onFilterChanged _ = _this .= filter
```

As the code suggests, in this line we're assigning the filter that is being currently rendered to the state of the component. One more thing here, if you noticed, we're ignoring an argument on the left-side of the function definition, this is the event data that is passed from the click event for which this handler doesn't care about so we simply ignore it.

The code above, however, looks suspiciously imperative but that is really the intent of this framework, unlike other architectures such as [Elm](http://elm-lang.org/) or [Redux](https://redux.js.org/) we don't need messages to update the state and we simply do so through the power of lenses and have Proact build the global and immutable state for us.

I hope you see the value of following this imperative style in that we're saved from writing lots of boilerplate code that are not uncommon in architectures with message dispatchers.

Moving on to the next component.

## Building our second component: A task

This whole application revolves around the concept of tasks: we add them, we check/complete them, we uncheck them and we delete them.

But how does the state of a task look like in our application?

It's actually very simple, every task has a text description that we'd like to display to the user and a boolean flag marking whether the task was completed or not. We will introduce an additional element to the state which is the index of the task within the list of to-do activities. This'll be useful later but for now let's define the data structure of the state:

```purescript
newtype State =
  State
    { completed :: Boolean
    , description :: String
    , index :: Int
    }

derive instance newtypeState :: Newtype (State) _

instance semigroupState :: Semigroup (State)
  where
  append s1 s2 =
    if s1 ^. _index < 0
    then s2
    else s1

instance monoidState :: Monoid (State)
  where
  mempty =
    State
      { completed : false
      , description : ""
      , index : -1
      }
```

As explained before, we include the fields `completed`, `description` and `index` inside our state but then also make it an instance of the [Monoid](https://pursuit.purescript.org/packages/purescript-monoid/3.3.0/docs/Data.Monoid#t:Monoid) type class. We do so in an interesting manner but first let me explain why does a task need to be a Monoid:

Given that a task will ultimately be composed inside a larger collection along with other tasks, we use lenses to `view` how a fully appended list of tasks would look like. To do so, the lens library demands that we provide a definition of an empty task on top of which it'll start piling the rest of the collection. However, we don't really want a single appended state representing the list of tasks, instead, we want a list of individual states that update independently, so to achieve this, Proact tricks the lensing library into keeping each state separate when appending them together and instead only asks the user for help when deciding how to `append` an actual task state with the `mempty` one. In later versions this could potentially stop being a requirement.

Following the state definition, we also want to include some getters and setters for our state fields so that we don't have to wrap/unwrap the newtype we just created:

```purescript
_completed :: Lens' State Boolean
_completed = _Newtype .. lens _.completed (_ { completed = _ })

_description :: Lens' State String
_description = _Newtype .. lens _.description (_ { description = _ })

_index :: Lens' State Int
_index = _Newtype .. lens _.index (_ { index = _ })
```

And so it's time to implement our Proact component by first designing how the GUI renders with regards to the state we defined above. This looks like the following:

```purescript
task :: forall fx . Component fx State ReactElement
task =
  do
  state <- ask
  pure $ view state
  where
  view state =
    (tr' .. map (td' .. singleton))
      [
        input
          [ _type "checkbox"
          , className "checkbox"
          , checked $ state ^. _completed
          , title "Mark as completed"
          ]
          []
      , text $ state ^. _description
      ,
        a
          [ className "btn btn-danger pull-right", title "Remove item" ]
          [ text "✖" ]
      ]
```

We design a task as a table row with three columns: the first will contain a checkbox either checked or unchecked depending on whether the task is completed, the second will contain the task description and the third one will contain a button to remove the task from the to-do list.

Now it's time to write the event handlers!

The first one responds to the event of checking/unchecking the checkbox and it updates the status of the task, the second in fact, cannot be done in this context as we don't know the list of tasks to which this component belongs to so we will just ask for a handler to this event from whomever decides to include us:

```purescript
task :: forall fx . ReactHandler fx Int -> Component fx State ReactElement
task onDelete =
  do
  state <- ask
  dispatcher <- eventDispatcher
  pure $ view dispatcher state
  where
  view dispatcher state =
    (tr' .. map (td' .. singleton))
      [
        input
          [ _type "checkbox"
          , className "checkbox"
          , checked $ state ^. _completed
          , title "Mark as completed"
          , onChange $ lmap fromInputEvent $ dispatcher onCompleted
          ]
          [ ]
      , text $ state ^. _description
      ,
        a
          [ className "btn btn-danger pull-right"
          , title "Remove item"
          , onClick \_ -> onDelete $ state ^. _index
          ]
          [ text "✖" ]
      ]

  fromInputEvent event = { checked : (unsafeCoerce event).target.checked }

  onCompleted event = _completed .= event.checked
```

Just as explained eariler, we request an event handler to deal with the delete event and we send to it the index of the current task so that it knows where to remove it from. The event that responds to the checkbox control is interesting:

As it turns out, Proact is incapable of reading the DOM elements of the GUI and because of the asynchronous nature of the event handlers and the fact Proact is built on top of React, we cannot access the event data from the context of the event handlers. To work around these two problems we change the type of the checkbox event with `lmap` which is like a backwards `map` for function arguments (or contravariant types, in general). At this point, that is, when changing the event type, we do have access to the event data which among other stuff includes the DOM element that generated the event. From this we can read its checked/unchecked status and pass it to Proact's event handler.

Our check/uncheck handler is the following one-liner:

```purescript
onCompleted event = _completed .= event.checked
```

Very similar to the handler we had for our filtering menu but here we're actually using the lens getter/setter we defined earlier.

And that's it for this component, let's move on to the next one which is a little bit more complicated.

## Building our third component: The to-do task list

This component is more complicated than the ones we created above: we need to display a list of tasks after we filter them according to the option selected in the filtering menu and we also need a text-input box from which the user will add new tasks to the list. How does the state of this component look like?

```purescript
newtype State =
  State
    { filter :: Filter.Filter
    , taskDescription :: String
    , tasks :: Array Task.State
    }
```

Hopefully something like above: the list of tasks, the current filter we're applying and the description of the new task we're going to create.

Now that the state is defined, let's add some getters and setters too:

```purescript
_filter :: Lens' State Filter.Filter
_filter = _Newtype .. lens _.filter (_ { filter = _ })

_taskDescription :: Lens' State String
_taskDescription = _Newtype .. lens _.taskDescription (_ { taskDescription = _ })

_tasks :: Lens' State (Array Task.State)
_tasks = _Newtype .. lens _.tasks (_ { tasks = _ })
```

So what's next? Hard to say, the component is complicated so it's better to split it into smaller subcomponents, let's say: a component for the task (we already have this one), a component for adding a new task and the table that will contain everything.

### Building a new task

The state of this component includes a string with the description of the new task to be added and the list of tasks to which we will append it. We could define a new state for this like we did for all the components before but we're lazy so we'll just reuse the same state type that we created above.

Next we design the GUI:

```purescript
taskBox :: forall fx . Component fx State ReactElement
taskBox =
  do
  state <- ask
  dispatcher <- eventDispatcher
  pure $ view dispatcher state
  where
  view dispatcher state =
    input
      [ className "form-control"
      , placeholder "Create a new task"
      , value $ state ^. _taskDescription
      ,
        onKeyUp
          $ unsafeCoerce
          $ lmap fromInputEvent
          $ dispatcher onNewTaskEnter
      ,
        onChange
          $ unsafeCoerce
          $ lmap fromInputEvent
          $ dispatcher onTextChanged
      ]
      []
    where
    fromInputEvent event =
      { keyCode : (unsafeCoerce event).keyCode
      , text : (unsafeCoerce event).target.value
      }

    newTask index text = (Task._index .~ index) .. (Task._description .~ text)

    onNewTaskEnter event =
      if event.keyCode == 13 && event.text /= ""
      then
        do
        let index = length $ state ^. _tasks
        _tasks %= flip snoc (newTask index event.text mempty)
      else if event.keyCode == 27
      then _taskDescription .= ""
      else pure unit

    onTextChanged event = _taskDescription .= event.text
```

This time I also included the event handlers together with the GUI but hopefully, nothing looks very shocking to you at this point:

1. We define the component as an input box to enter the new task description.
2. We subscribe to two different events from this control:
  ..- The key-up event to detect whether the <kbd>ENTER</kbd> or <kbd>ESC</kbd> keys were pressed.
  ..- The change event to detect whenever the text inside the box was changed.
3. We add event handlers to respond to the events by:
  ..- Clearing the new task description if <kbd>ESC</kbd> key was pressed.
  ..- Adding the new task to the list if <kbd>ENTER</kbd> key was pressed.
  ..- Updating the text description when the text inside the input box is changed.

### Building a task... again

Wait, I thought we already had a component for this, right?

Not quite, for this application we need to filter tasks and decide whether to show them or not depending on the active filter so we're forced to create a new component that handles this behavior.

It would be nice if we didn't have to create a new component for this and rely on the parent that contains the task collection to handle the operation. There's even a [lens for it](https://pursuit.purescript.org/packages/purescript-profunctor-lenses/3.8.0/docs/Data.Lens.Fold#v:filtered), unfortunately, Proact at this moment has a limitation when working with Prisms and Traversal lenses at the same time and the filtering lens just so happen to be built on top of both of these :(

Alas, we have to build a filtered task:

```purescript
filteredTask
  :: forall fx
   . (Task.State -> Boolean)
  -> ReactHandler fx Int
  -> Component fx Task.State (Array ReactElement)
filteredTask filter' onDelete =
  do
  visible <- asks filter'
  if visible
    then singleton <$> Task.task onDelete
    else pure [ ]
```

It wasn't too bad, we take as input the same on-delete event handler parameter that the task component did and an additional filtering predicate that we apply inside the component. You'll notice that we change the return type of the Monad from `ReactElement` to `Array ReactElement` and this is to handle the scenario where the task is not to be shown.

### Building composed components: The task table

The task table is quite simple once we fragmented it into smaller components and the only behavior it needs to include on top of what we have defined before is the ability to delete tasks from the list. Its state is, in fact, exactly what we defined at the beginning so no need to worry about this anymore and instead we can focus on building the component now:

```purescript
taskTable :: forall fx . Component fx State ReactElement
taskTable =
  do
  filter' <- use' _filter
  dispatcher <- eventDispatcher
  tasksView <-
    focus (_tasks .. traversed)
      $ filteredTask (taskFilter filter')
      $ dispatcher onDelete

  taskBoxView <- taskBox

  pure $ view taskBoxView tasksView
  where
  view taskBoxView tasksView =
    table
      [className "table table-striped"]
      [
        thead'
          [
            tr'
              [ th [className "col-md-1"] []
              , th [className "col-md-10"] [text "Description"]
              , th [className "col-md-1"] []
              ]
          ]
      , tbody' $ tr' [td' [], td' [taskBoxView], td' []] : tasksView
      ]

  taskFilter Filter.All _ = true
  taskFilter Filter.Completed task = task ^. Task._completed
  taskFilter Filter.Active task = not $ task ^. Task._completed

  onDelete index = _tasks %= deleteTaskAt index

  deleteTaskAt index array =
    (unwrap <<< unsafePartial)
      do
      let array' = fromJust $ deleteAt index array
      traverseWithIndex resetIndex array'

  resetIndex :: Int -> Task.State -> Identity Task.State
  resetIndex index task = pure $ task # Task._index .~ index
```

Lots of things here to explain:

1. The GUI is just a table containing the smaller GUIs from each of the individual components that we extracted in a monadic style.
2. The task box component shares the same state as the task table which means it shares the same Monadic type.
3. The filtered task component we previously defined uses a different state: a task, which needs to be transformed into an array of tasks somehow.
.. This somehow is the `focus` function provided by Proact which takes as input the transformation lens `_tasks .. traversed` and the component whose state is to be transformed.
4. We send to the task component the filter that is currently selected and an event handler that responds to the **delete** event.
5. The event handler takes as input the index of the task to be removed, removes that one task from the list and updates the index of each item of the list to accomodate for the deleted item.

So now we have a list of tasks inside a table and we're almost done writing this application. Time to move on to the final component.

## Building our fourth and last component: The To-do application

Let's finish the application by putting the filtering menu and the task table together in a to-do component.

The state for this includes all the states defined before and nothing more, thus, once again, we find that we can reuse the same state definition that we had for our last component since it already includes the individual states of the filtering menu and the task table.

So now we only need to design the GUI:

```purescript
todo :: forall fx . Component fx State ReactElement
todo =
  do
  state <- ask
  filterMenuView <- focus' _filter Filter.filterMenu
  taskTableView <- taskTable

  pure $ view state filterMenuView taskTableView
  where
  view state filterMenuView taskTableView =
    div
      [ className "container" ]
      [ h1' [ text "To-do App" ]
      , filterMenuView
      , br' [ ]
      , br' [ ]
      , taskTableView
      , p' [ text $ totalCompleted <> "/" <> total <> " tasks completed." ]
      ]
    where
    tasks = state ^. _tasks

    total = show $ length tasks

    totalCompleted = show $ length $ filter (_ ^. Task._completed) tasks
```

At this point, the only thing worth mentioning is the use of the function `focus'` instead of `focus` when absorbing the filtering menu component. You see, `focus` is universal for all lenses (it takes as input a Traversal which is a universal lens) but demands both the type of the state and the monadic return to be Monoids, this is to handle the scenario where there are zero or more instances of the same component. However, for this case we only have one instance of the filtering menu and we can use a less universal lens which happens to be the getter/setter of the filter from the global state. For these scenarios Proact provides the alternative `focus'` that doesn't impose additional constraints on any of the data types of the component but has the downside that it only supports a limited number of lenses: Isos and Lenses.

Before moving on to the last section, it's always a good idea for complex components to define a function that returns an initial state, we'll call it `mempty'`:

```purescript
mempty' :: State
mempty' =
  State
    { filter : Filter.All
    , taskDescription : ""
    , tasks : [ ]
    }
```

## Completing the application and writing the Main module

Proact is built on top of React and thus both frameworks play together nicely. In fact, you cannot really run or render a Proact component without first defining it as a React one. You do this by calling the `spec` function which takes as input a Proact component and its initial state and outputs a React spec which can be understood as the first step of the following process:

1. [React Spec](https://engineering.musefind.com/react-lifecycle-methods-how-and-when-to-use-them-2111a1b692b1): A collection of functions that a React Class exposes in order for the user of the library to control the component's lifecycle.
2. React Class: In Purescript, this is the actual React Class whose lifecycle you overwrote in the previous step but that at this point you cannot overwrite anymore.
3. React Element: An object in the virtual DOM that **is not HTML** and whose properties (props) can be overwritten. However, Proact components do not use props so don't worry about this.
4. HTML Element: At this point, we have no longer any meaningful control of our component as we're ready to render it into the page.

The following routine illustrates the process described above:

```purescript
main =
  unsafePartial
    do
    let spec = spec todo mempty'
    let element = flip createFactory { } $ createClass spec
    rDocument <- map D.htmlDocumentToParentNode $ D.window >>= D.document
    rApp <- fromJust <$> D.querySelector (D.QuerySelector "#app") rDocument
    void $ render element rApp
```

Only the fourth line is relevant for the purpose of this tutorial and is very simple: `spec` is a function that returns a React Spec and takes as input the `todo` Proact component and its `mempty'` initial state.

## Not quite complete yet

This is still not a web page, actually. It's just purescript code. We still need to make an actual HTML page, compile the purescript code into javascript, import it into the page and also import Bootstrap because, really, any app looks awful without color or style.

You can check how to do all this by looking into the source repository for this project that can be found [here](https://github.com/alvart/proact/tree/master/examples/todo/src).

I hope this exercise helps you understand how easy is to write applications with Proact and, who knows, maybe even motivates you to write your next big project on it.

Cheers
:)
