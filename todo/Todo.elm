module Todo where
{-| TodoMVC partially implemented in Elm. Your challenge is to add the missing
functionality.

This application is broken up into four distinct parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML
  4. Inputs - the signals necessary to manage events

This clean division of concerns is a core part of Elm. You can read more about
this in the Pong tutorial: http://elm-lang.org/blog/Pong.elm

You will be adding new ways to update the model in the UPDATE section and then
adding code to the VIEW section to actually trigger these updates at the
appropriate times.
-}

import Debug
import Graphics.Input as Input
import Html
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Tags (..)
import Html.Optimize.RefEq as Ref
import Maybe
import String
import Window


---- MODEL ----

-- The full application state of our todo app.
type State =
    { tasks      : [Task]
    , field      : String
    , uid        : Int
    , visibility : String
    }

type Task =
    { description : String
    , completed   : Bool
    , editing     : Bool
    , id          : Int
    }

newTask : String -> Int -> Task
newTask desc id =
    { description = Debug.watch "Most Recent Task Description" desc
    , completed = False 
    , editing = False
    , id = id
    }

emptyState : State
emptyState =
    { tasks = []
    , visibility = "All"
    , field = ""
    , uid = 0
    }


---- UPDATE ----

-- A description of the kinds of actions that can be performed on the state of
-- the application. See the following post for more info on this pattern and
-- some alternatives: https://gist.github.com/evancz/2b2ba366cae1887fe621
data Action
    = NoOp
    | UpdateField String
    | Add

-- How we step the state forward for any given action
step : Action -> State -> State
step action state =
    case Debug.watch "Current action" action of
      NoOp -> state

      UpdateField str ->
          { state |
              field <- str
          }

      Add ->
          { state |
              uid <- Debug.watch "Todo ID" (state.uid + 1),
              field <- "",
              tasks <-
                  if String.isEmpty state.field
                      then state.tasks
                      else state.tasks ++ [newTask state.field state.uid]
          }


---- VIEW ----

view : State -> Html
view state =
    div
      [ class "todomvc-wrapper"
      , style [ prop "visibility" "hidden" ]
      ]
      [ section
          [ id "todoapp" ]
          [ Ref.lazy taskEntry state.field
          , Ref.lazy2 taskList state.visibility state.tasks
          , Ref.lazy2 controls state.visibility state.tasks
          ]
      , infoFooter
      ]

onEnter : Input.Handle a -> a -> Attribute
onEnter handle value =
    on "keydown" (when (\k -> k.keyCode == 13) getKeyboardEvent) handle (always value)

taskEntry : String -> Html
taskEntry task =
    header 
      [ id "header" ]
      [ h1 [] [ text "todos" ]
      , input
          [ id "new-todo"
          , placeholder "What needs to be done?"
          , autofocus True
          , value task
          , name "newTodo"
          , on "input" getValue actions.handle UpdateField
          , onEnter actions.handle Add
          ]
          []
      ]

taskList : String -> [Task] -> Html
taskList visibility tasks =
    let isVisible todo =
            case visibility of
              "Completed" -> todo.completed
              "Active" -> not todo.completed
              "All" -> True

        allCompleted = all .completed tasks

        cssVisibility = if isEmpty tasks then "hidden" else "visible"
    in
    section
      [ id "main"
      , style [ prop "visibility" cssVisibility ]
      ]
      [ input
          [ id "toggle-all"
          , type' "checkbox"
          , name "toggle"
          , checked allCompleted
          ]
          []
      , label
          [ for "toggle-all" ]
          [ text "Mark all as complete" ]
      , ul
          [ id "todo-list" ]
          (map todoItem (filter isVisible tasks))
      ]

todoItem : Task -> Html
todoItem todo =
    let className = (if todo.completed then "completed " else "") ++
                    (if todo.editing   then "editing"    else "")
    in

    li
      [ class className ]
      [ div
          [ class "view" ]
          [ input
              [ class "toggle"
              , type' "checkbox"
              , checked todo.completed
              ]
              []
          , label
              []
              [ text todo.description ]
          , button
              [ class "destroy"
              ]
              []
          ]
      , input
          [ class "edit"
          , value todo.description
          , name "title"
          , id ("todo-" ++ show todo.id)
          ]
          []
      ]

controls : String -> [Task] -> Html
controls visibility tasks =
    let tasksCompleted = length (filter .completed tasks)
        tasksLeft = length tasks - tasksCompleted
        item_ = if tasksLeft == 1 then " item" else " items"
    in
    footer
      [ id "footer"
      , hidden (isEmpty tasks)
      ]
      [ span
          [ id "todo-count" ]
          [ strong [] [ text (show tasksLeft) ]
          , text (item_ ++ " left")
          ]
      , ul
          [ id "filters" ]
          [ visibilitySwap "#/" "All" visibility
          , text " "
          , visibilitySwap "#/active" "Active" visibility
          , text " "
          , visibilitySwap "#/completed" "Completed" visibility
          ]
      , button
          [ class "clear-completed"
          , id "clear-completed"
          , hidden (tasksCompleted == 0)
          ]
          [ text ("Clear completed (" ++ show tasksCompleted ++ ")") ]
      ]

visibilitySwap : String -> String -> String -> Html
visibilitySwap uri visibility actualVisibility =
    let className = if visibility == actualVisibility then "selected" else "" in
    li  []
        [ a [ class className, href uri ] [ text visibility ] ]

infoFooter : Html
infoFooter =
    footer [ id "info" ]
      [ p [] [ text "Double-click to edit a todo" ]
      , p [] [ text "Written by "
             , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
             ]
      , p [] [ text "Part of "
             , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
             ]
      ]


---- INPUTS ----

-- wire the entire application together
main : Signal Element
main = lift2 scene state Window.dimensions

scene : State -> (Int,Int) -> Element
scene state (w,h) =
    container w h midTop (Html.toElement 550 h (view state))

-- manage the state of our application over time
state : Signal State
state = foldp step startingState actions.signal

startingState : State
startingState = emptyState

-- actions from user input
actions : Input.Input Action
actions = Input.input NoOp
