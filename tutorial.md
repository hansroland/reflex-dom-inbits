# A Tutorial about Reflex-Dom

Today most computer programs have a graphical user interface (GUI). 
However Haskell programs with a GUI are still rare. Haskell programs normally use a command line interface. 
For a long time, there were no good options to write GUI programs in Haskell. 
The event driven nature of a GUI's is difficult to map onto the functional paradigm. 
The traditional object oriented way to program a GUI application uses callbacks. 
These callbacks need a lot of state and managing state is not easy in Haskell.

To solve these problems the Haskell community developed a lot of new new ideas. 
One of the solutions is Functional Reactive Programming, *FRP* for short. 
Conal Elliott and Paul Hudak first developed the basic idea and published 2007 the paper [Functional Reactive Animation](http://conal.net/papers/icfp97/)
On Hackage you can find a lot
of different FRP libraries, eg *grapefruit-frp*, *reactive-banana*, *elera*, *frpnow*, *netwire*, *reflex* and many more.

In this tuorial we use Reflex and Reflex-Dom. Reflex is a FRP implemetation written by Ryan Trinke.
It gives a string foundation to handle events and values that change over time. 
Reflex-Dom is built on Reflex and on GHCJS.Dom. It allows you to write GUI programs that run in a 
Web Browser or as a 'native' application in Webkit. Reflex-Dom was written by Ryan Trinkle too.


# Basics of Functional Reactive Programming (FRP) and Reflex

## Basic Idea of Functional Reactive Programming

Normally input functions are impure. Assume a function *getChar* that reads a single character from the keyboard.
For different invocations the function *getChar*  normally returns a different character, depending on which key on the keyboad was presse.
Therefore such a function is not a pure Haskell function. As everybody knows Haskell uses monadic IO actions to avoid unpure functions.

Functional Reactive Programming takes an other approach. All potentially impure functions have a time parameter
and the FRP system makes sure, that every call to such a function is done with a new time value.

In pseudo code:

~~~ { .haskell }
getChar :: Time -> Char

getChar 1     -- this returns eg a 'F'
getChar 2     -- this returns eg a 'R'
~~~ 

Everytime you call *getChar* with a parameter of 1, it will return the character 'F'. 
However the FRP framework will not allow you to do multiple calls to *getChar* with the parameter 1.

With this trick, *getChar* is now a pure function.

In Reflex the time parameter is always shown explicit in the type declaration of the function.
Normally a type parameter with the name *t* is used.
However it's never necessary to supply this parameter as a programmer when you call the function.

Reflex uses 3 main time dependent data types:

* Events
* Behaviors
* Dynamics

A typical type declaration for a function could be:

~~~ { .haskell }
dispEvent :: MonadWidget t m => T.Text  -> Event t ClickInfo -> m ()
~~~

Here the *t* is the time parameter. This parameter is always introduced with a precondition. In our case
```  MonadWidget t m => ```. The *m* is normally some monad. *ClickInfo* would be a user defined Haskell data type.

To call the above function, in some monadic context, you would write:

~~~ { .haskell }
   dispEvent "This is my event" evClick
~~~ 

Events and Behaviors are common data types in different FRP implementaions. Dynamics, however, are probabaly unique
in Reflex.

## Event

Events occure at some points in time and they carry a value. 
The most prominent example for events are mouse clicks or pressing keys on a keyboard. 
The value of a keyboard event is normally the code of the pressed key.

During any given time frame, an *Event* is either occurring or not occurring; if it is occurring, it will contain a value of
the given type. This is shown in the following diagram:

![Events](https://github.com/hansroland/reflex-dom-inbits/raw/master/images//event.png "Events")


In Reflex the data type *Event* has the following simplified type:

~~~ { .haskell }
data Event t a
~~~ 

'*a*' is the type of the event. I also call '*a*' the payload of the event.  It can be more or less every Haskell data type. It can even be a function.

Events are the main work horses withing Reflex. As we will see, it is very common to use functions to transform an Event of type *a*
into an event of type *b*. 

The data type *Event* is an instance of the Haskell Functor type class. 
This allows event transformation with the well known *fmap* function:

~~~ { .haskell }
fmap :: (a ->  b) ->  Event a -> Event b
~~~ 

Later we will see other functions to transform events:


## Behavior

A Behaviors is a container for a value, that can change over time. Other than events, they always have a value. 
It is not possible to be notified when a Behavior changes.

In Reflex the data type *Behavior* has the following simplified type:

~~~ { .haskell }
data Behavior t a
~~~


Behaviors can change their values only at time points where Events occur. This is shown in the following diagram.

![Behavior](https://github.com/hansroland/reflex-dom-inbits/raw/master/images//behavior.png "Behavior")

To write a Reflex-dom application we rarely use Behaviors. If we need values that change over time, we use Dynamics. 

## Dynamic

A *Dynamic* is container for a value that can change over time and allows notifications on changes. 
Dynamics are special to Reflex. They are a combination of the types *Behavior* and *Event*.

~~~ { .haskell }
data Dynamic t a
~~~

![Behavior](https://github.com/hansroland/reflex-dom-inbits/raw/master/images//dynamic.png "Behavior")

A Dynamic is a monad and therefore an Applicative too. 
When working with Dynamics it is very common to use applicative syntax .

# Before we really start ...

## Popular Language Extensions

To write Reflex programs, very often we use some of the following GHC Haskell language Extensions

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #}
~~~ 

~~~ { .haskell }
{-# LANGUAGE RecursiveDo #}
~~~ 

~~~ { .haskell }
{-# LANGUAGE ScopedTypeVariables #}
~~~ 

## Popular Imports

From all the 1001 libraries stored on Hackage, we will use only very few:

```import Reflex.Dom``` 

Ok the tutorial is about Reflex.Dom so we should import it. 
It's not necessary to import Reflex. Reflex.Dom reexports all the needed functions of Reflex.

```import qualified Data.Text as Text```

Instead of the data type *String* reflex-dom uses the type *Text*. Text strings perform better than String strings.



## Some comments to the code examples

A lot of these examples could be written with less lines, just by using the monadic fucntions (>>=),
(>>), (=<<), (<<) etc.

Sometimes I used more lines in my code in order to have code that is not so terse, This code should be simpler to understand for beginners.

# A First Simple Reflex-Dom Example

Let's begin with a first simple reflex example. This code is in the file src/count01.hs

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget $ display =<< count =<< button "ClickMe"
~~~

This example uses 4 reflex or reflex-dom functions:

* mainWidget
* display
* count
* button

Let's look at the types of these functions and what they do:

## Function: *mainWidget*

~~~ { .haskell }
mainWidget :: (forall x. Widget x ()) -> IO ()
~~~ 

It sets up the reflex-dom environment. It takes an argument of type *Widget* and returns *IO ()*

The type *Widget* is a little bit scary. However we never really need to work with the details of it. 

~~~ { .haskell }
type Widget x =
  PostBuildT
    Spider
    (ImmediateDomBuilderT
       Spider (WithWebView x (PerformEventT Spider (SpiderHost Global))))
~~~ 

PostBuildT is a monad transformer. It set's up a monadic environement for reflex-dom. 
As side effects some of the reflex-dom functions will create the DOM elements.

The function *mainWidget* has 2 sister functions *mainWidgetWithCss* and *mainWidgetWithHead".
We will see them later.

## Function: *display*

~~~ { .haskell }
display :: (Show a, ... ) => Dynamic t a -> m ()
~~~ 

The function takes an argument of type *Dynamic t a* and returns unit in the current monad.
It uses the Show instance of the datatype *a* to build a string representation of its first parameter.
Then it creates in the DOM a text element, where it displays this string. 
As mentioned above, the creation of the DOM element is a monadic side effect.

*display* has a precondition of *Show a*. It has other preconditions too, 
If you use *mainWidget* or one of its sister functions, the other preconditions are normally fullfilled automatically.
Thefore I don't show them here and in the examples to follow..

## Function: *count*

~~~ { .haskell }
count :: (Num b, ...) =>  Event t a -> m (Dynamic t b)
~~~normally

The *count* function takes an event as argument and creates a Dynamic.
In this Dynamic the function count's up the number of times the event occured or fired.

## Function: *button*

~~~ { .haskell }
button :: (...) => Text -> m (Event t ())
~~~

The *button* function takes a text as argument. It creates a DOM button element with the text, and returns
an event with *()* as payload.

Now it's easy to understand the whole line *mainWidget $ display =<< count =<< button "ClickMe"*:

* Clicking on the button creates or triggers events, 
* The function *count* creates a dynamic value with the total number of these events.
* The function *display* creates a DOM element with a string representation of this number and displays it as DOM element
 normally

**Try it!**

# Creating Other DOM elements

Till now we used the 2 helper functions *button* and *display* to create DOM elements.
Reflex has 2 other very frequently used helper functions to create DOM elements:

* text
* dynText

## Function: *text*

~~~ { .haskell }
text :: (...) => Text -> m ()
~~~

It is very simple: It just displays the text in the DOM. During program execution the text displayed in the DOM never changes. 
The text is not of type *Dynamic t Text* but only of *Text*, hence it is static!!

## Function: *dynText* 

~~~ { .haskell }
dynText :: (...) => Dynamic t Text -> m ()
~~~

The function *dynText* does more or less the same as the function *text*. 
However, the function argument is of type *Dynamic t Text*, so now the text may change during the execution of the program!!
We will see some examples later.


## Function family el, elAttr, elClass, elDynAttr, elDynClass

Reflex-dom has 2 function families to create all the different kinds of DOM elements:

* el, elAttr, elClass, elDynAttr, elDynClass
* el', elAttr', elClass', elDynAttr', elDynClass'

First we will only look at the first family without the primes ' in the name:

## Function: *el*

This function has the type signature:

~~~ { .haskell }
el :: (...) => Text -> m a -> m a
~~~ 

It takes the type of the DOM element as a first argument. The second argument is either the text of the element
or a child element.

The file *src/dom01.hs* contains a typical first example:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget $ el "h1" $ text "Welcome to Reflex-Dom"
~~~ 

This will create a header-1 DOM element with the text "Welcome to Reflex-Dom".

In HTML:

~~~ { .html }
<html>
  <head>
    ...
  </head>
  <body>
    <h1>Welcome to Reflex-Dom</h1>
  </body>
</html>
~~~

We are now able to create whole web pages. The file *src/dom02.hs* contains a small example:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO()
main = mainWidget $ do
  el "h1" $ text "Welcome to Reflex-Dom"
  el "div" $ do
    el "p" $ text "Reflex-Dom is:"
    el "ul" $ do
      el "li" $ text "Fun"
      el "li" $ text "Not difficult"
      el "li" $ text "Efficient"
~~~

If you use HTML elements without any values or without a child, you simply write:

~~~ { .haskell }
    el "br" $ return () 
~~~

*Try it!!*

## Function: *elAttr*

With the function *el*, we can't create a DOM element with attributes, eg a link:

~~~ { .html }
<a target="_blank" href="http://google.com">Google!</a>
~~~ 

To add attributes, reflex-dom has a function *elAttr*. It has the type:

~~~ { .haskell }
elAttr :: (...) => Text -> Map Text Text -> m a -> m a
~~~

The function *elAttr* is similar to the function *el*, but it takes an additional 
parameter of type *Map Text Text*. This parameter contains the attributes of the DOM element.
A Map is a *key-value* relation,
In the above link example, *target* and *href* are the keys and *"_blank"* and *"http://google.com"*
are the values.

The library Data.Map defines a little helper function (=:) to create a singelton Map.

~~~ { .haskell }
(=:) :: k -> a -> Map k a
~~~

The singleton Maps are then appended / merged with the (<>) operator from Monoids.

The file src/dom03.hs contains the example:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import qualified Data.Text as T 
import qualified Data.Map as Map
import           Data.Monoid ((<>))

main :: IO ()
main = mainWidget $ do
    el "h1" $ text "A link to Google in a new tab"
    elAttr "a" attrs $ text "Google!"

attrs :: Map.Map T.Text T.Text
attrs = ("target" =: "_blank") <> ("href" =: "http://google.com")
~~~

## Function: *elClass*

The function *elClass* allows to specify the name of a class to be used by the Cascaded Style Sheets (CSS).

It has the following type:

~~~ { .haskell }
elClass :: (...) => T.Text -> T.Text -> m a -> m a
~~~

The first parameter is again the type of the DOM element. The second is the name of the CSS class.

A small example:

~~~ { .haskell }
elClass "h1" "mainTitle" $ text "This is the main title"
~~~

In HTML:

~~~ { .html }
<h1 class="mainTitle">This is the main title</h1>
~~~

## Function: *elDynAttr*

All the above functions allow us to define static DOM elements. 
But you cannot change them while the program is running!

With the function *elDynAttr*, as the name says, you can specify attributes, 
that change during program execution. It has the following type:

~~~ { .haskell }
elDynAttr  :: (...) => Text -> Dynamic t (Map Text Text) -> m a -> m a
~~~

You guessed it, the first parameter is again the type, and the second is a map with **any** attribute,
you can use for your DOM element. However, this time this map is wrapped in a Dynamic.

To use the function *elDynAttr* we must somehow create a Dynamic. 
Reflex has several functions to create Dynamic values. As a first example, we will use the function *toggle*:

~~~ { .haskell }
toggle :: (...) => Bool -> Event t a -> m (Dynamic t Bool)
~~~

The function toogle create a new Dynamic using the first parameter as the initial value
and flips this value every time the Event in the second parameter occurs.

The file *src/dom04.hs* contains an example for the function *elDynAttr*:

~~~ { .haskell .numberLines}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import           Reflex.Dom
import qualified Data.Text as T 
import qualified Data.Map as Map
import           Data.Monoid ((<>))

main :: IO ()
main = mainWidget $ do
  rec
    dynBool <- toggle False evClick
    let dynAttrs = attrs <$> dynBool
    elDynAttr "h1" dynAttrs $ text "Changing color"
    evClick <- button "Change Color"
  return ()

attrs :: Bool -> Map.Map T.Text T.Text
attrs b = "style" =: ("color: " <> color b)
  where 
    color True = "red"
    color _    = "green"
~~~

Comments: 

* We need recursive do, because we refer to the event *evClick* before it is defined.
* *dynBool* contains our value of type *Dynamic t bool*. It is created by the *toggle* function.
* *dynAttrs* contains the *Dynamic t (Map Text Text). It is created with an applicative call to the function *attrs*.
* The function *attrs* containsthe 'business logic' of this example: 
It decides on the boolean parameter about the color of the DOM element.
* Please note, that the function *attrs* is a normal pure function as we know and love since Haskell kindergarden!
* Transforming a Dynamic value or combining several Dynamic values with the help of applicative syntax and a pure fucntion is a common pattern in Reflex.

## Function *elDynClass*

The function *elDynClass* is similar to *elClass* but here the type of the parameter to specify the name of the CSS class is Dynamic.
This allows you to change the CSS-class of the element dynamically during runtime.

The function has the following type:

~~~ { .haskell }
elDynClass :: (...) => Text -> Dynamic t Text -> m a -> m a
~~~

# Basic Event Handling



# Input Widgets

## Function button - Do it yourself!

*button* is a simple helper function to create a button in the DOM. It returns an event
with unit *()* in the payload.
Unfortunately, this function is not very flexible: We can only specify the label of the button and nothing more.
If we want to add a CSS class or directly some attributes we have to define the button with on of the *el* functions:


## Text Input Fields

Reflex-dom defines a Haskell data type to specify the different options of a text input field:

~~~ { .haskell }
data TextInputConfig t
   = TextInputConfig { _textInputConfig_inputType :: Text
                     , _textInputConfig_initialValue :: Text
                     , _textInputConfig_setValue :: Event t Text
                     , _textInputConfig_attributes :: Dynamic t (Map Text Text) }
~~~

and it also defines a data type for the resulting DOM element:

~~~ { .haskell }
data TextInput t
   = TextInput { _textInput_value :: Dynamic t Text
               , _textInput_input :: Event t Text
               , _textInput_keypress :: Event t Int
               , _textInput_keydown :: Event t Int
               , _textInput_keyup :: Event t Int
               , _textInput_hasFocus :: Dynamic t Bool
               , _textInput_builderElement :: InputElement EventResult GhcjsDomSpace t }
~~~

The function to create a text input element is:

~~~ { .haskell }
textInput :: (...) => TextInputConfig t -> m (TextInput t)
~~~

### The type class Default

The Haskell library *data-default-class* defines a type class *Default* to initialize a data type with default values:

~~~ { .haskell }
-- | A class for types with a default value.
class Default a where
    -- | The default value for this type.
    def :: a
~~~

It has one single function *def*. 

The data type *TextInputConfig* is an instance of this type class and the *def* function is defined like this:

~~~ { .haskell }
instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig { _textInputConfig_inputType = "text"
                        , _textInputConfig_initialValue = ""
                        , _textInputConfig_setValue = never
                        , _textInputConfig_attributes = constDyn mempty }
~~~

### Examples





## TextAreas

TextInput fields have only one input text line. If you want several input lines, you must use TextAreas.

TextAreas are built in the same way as TextInput fields: There is a configuration record and a data record 
exactly as for TextInput fields. The names are different: *_textInput* is replaced by *_textArea*.

## First useful example: ColorViewer.hs

With  


## Checkboxes

For checkboxes, we have the same thing as for TexInput and TextArea: There is a record structure for the configuration,
one for the widget element and a function to create the checkbox widget in the DOM. 
However, everything is a little bit simpler:

The record structure for the configuration:

~~~ { .haskell }
data CheckboxConfig t
    = CheckboxConfig { _checkboxConfig_setValue :: Event t Bool
                     , _checkboxConfig_attributes :: Dynamic t (Map Text Text) }
~~~

The data type *CheckboxConfig* supports the *Default* type class:

~~~ { .haskell }
instance Reflex t => Default (CheckboxConfig t) where
  def = CheckboxConfig { _checkboxConfig_setValue = never
                       , _checkboxConfig_attributes = constDyn mempty }
~~~

And here is the function to create the DOM element

~~~ { .haskell }
checkbox :: (...) => Bool -> CheckboxConfig t -> m (Checkbox t)
~~~

### Examples

#### checkbox01.hs

This is the most simple way to create and use a checkbox. However, you have to click exactly into 
the small square to change the state of the checkbox. When you click into the label *Click me* it does not
change it's state. This is very user unfriendly!

#### checkbox02.hs

This example shows how to fix this issue:
We create a combined widget: The checkbox element is a child of a *label* element. 
The result of the combined widget is the event of the checkbox.

It works now as expected: To change the stete of the checkbox, you can either click onto the small square 
or into the text *Click me*

## Radio Buttons

~~~ { .haskell }

~~~

## Drop Downs

~~~ { .haskell }

~~~

## Listboxes

~~~ { .haskell }

~~~

### Communication with a Web Server
