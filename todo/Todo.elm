module Todo where
{-| TodoMVC partially implemented in Elm. Your challenge is to add the missing
functionality.

This application is broken up into four distinct parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML
  4. Inputs - the signals necessary to manage events

You will be adding new ways to update the model in the UPDATE section and then
adding code to the VIEW section to actually trigger these updates at the
appropriate times.
-}

import String
import Html
import Html (..)
import Html.Events (..)
import Html.Optimize.RefEq as Ref
import Maybe
import Window

import Graphics.Input (..)
import Graphics.Input as Input


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
    { description = desc
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
    case action of
        NoOp -> state

        UpdateField str ->
            { state |
                field <- str
            }

        Add ->
            { state |
                uid <- state.uid + 1,
                field <- "",
                tasks <-
                    if String.isEmpty state.field
                      then state.tasks
                      else state.tasks ++ [newTask state.field state.uid]
            }


---- VIEW ----

view : State -> Html
view state =
    node "div"
      [ "className" := "todomvc-wrapper" ]
      [ "visibility" := "hidden" ]
      [ node "section"
          [ "id" := "todoapp" ]
          []
          [ Ref.lazy taskEntry state.field
          , Ref.lazy2 taskList state.visibility state.tasks
          , Ref.lazy2 controls state.visibility state.tasks
          ]
      , infoFooter
      ]

onEnter : Handle a -> a -> EventListener
onEnter handle value =
    on "keydown" (when (\k -> k.keyCode == 13) getKeyboardEvent) handle (always value)

taskEntry : String -> Html
taskEntry value =
    node "header" 
      [ "id" := "header" ]
      []
      [ node "h1" [] [] [ text "todos" ]
      , eventNode "input"
          [ "id"          := "new-todo"
          , "placeholder" := "What needs to be done?"
          , "autofocus"   := "true"
          , "value"       := value
          , "name"        := "newTodo"
          ]
          []
          [ on "input" getValue actions.handle UpdateField
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
    in
    node "section"
      [ "id" := "main" ]
      [ "visibility" := if isEmpty tasks then "hidden" else "visible" ]
      [ node "input"
          [ "id" := "toggle-all"
          , "type" := "checkbox"
          , "name" := "toggle"
          , bool "checked" allCompleted
          ]
          []
          []
      , node "label"
          [ "htmlFor" := "toggle-all" ]
          []
          [ text "Mark all as complete" ]
      , node "ul"
          [ "id" := "todo-list" ]
          []
          (map todoItem (filter isVisible tasks))
      ]

todoItem : Task -> Html
todoItem todo =
    let className = (if todo.completed then "completed " else "") ++
                    (if todo.editing   then "editing"    else "")
    in

    node "li" [ "className" := className ] []
      [ node "div" [ "className" := "view" ] []
          [ node "input"
              [ "className" := "toggle"
              , "type" := "checkbox"
              , bool "checked" todo.completed
              ]
              []
              []
          , node "label" [] []
              [ text todo.description ]
          , node "button" [ "className" := "destroy" ] [] []
          ]
      , node "input"
          [ "className" := "edit"
          , "value" := todo.description
          , "name" := "title"
          , "id" := ("todo-" ++ show todo.id)
          ]
          []
          []
      ]

controls : String -> [Task] -> Html
controls visibility tasks =
    let tasksCompleted = length (filter .completed tasks)
        tasksLeft = length tasks - tasksCompleted
    in
    node "footer" [ "id" := "footer", bool "hidden" (isEmpty tasks) ] []
      [ node "span" [ "id" := "todo-count" ] []
          [ node "strong" [] [] [ text (show tasksLeft) ]
          , let item_ = if tasksLeft == 1 then " item" else " items"
            in  text (item_ ++ " left")
          ]
      , node "ul" [ "id" := "filters" ] []
          [ visibilitySwap "#/"          "All"       visibility
          , text " "
          , visibilitySwap "#/active"    "Active"    visibility
          , text " "
          , visibilitySwap "#/completed" "Completed" visibility
          ]
      , node "button"
          [ "className" := "clear-completed"
          , "id" := "clear-completed"
          , bool "hidden" (tasksCompleted == 0)
          ]
          []
          [ text ("Clear completed (" ++ show tasksCompleted ++ ")") ]
      ]

visibilitySwap : String -> String -> String -> Html
visibilitySwap uri visibility actualVisibility =
    let className = if visibility == actualVisibility then "selected" else "" in
    node "li" [] []
      [ node "a" [ "className" := className, "href" := uri ] [] [ text visibility ]
      ]

infoFooter : Html
infoFooter =
    node "footer" [ "id" := "info" ] []
      [ node "p" [] []
          [ text "Double-click to edit a todo"
          ]
      , node "p" [] []
          [ text "Written by "
          , node "a" [ "href" := "https://github.com/evancz" ] [] [ text "Evan Czaplicki" ]
          ]
      , node "p" [] []
          [ text "Part of "
          , node "a" [ "href" := "http://todomvc.com" ] [] [ text "TodoMVC" ]
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
actions : Input Action
actions = Input.input NoOp
