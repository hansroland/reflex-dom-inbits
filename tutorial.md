# A Tutorial about Reflex-Dom

Today most computer programs have a graphical user interface (GUI). 
However Haskell programs with a GUI are still rare. Haskell programs normally use a command line interface (CLI). 
For a long time, there were no good options to write GUI programs in Haskell. 
It is difficult to mat the event driven nature of a GUI program onto the functional paradigm. 
The traditional object oriented way to program a GUI application uses callbacks. 
These callbacks need a lot of global state and managing state is not easy in Haskell.

To solve these problems the Haskell community developed a lot of new new ideas. 
One of the solutions is called *Functional Reactive Programming*, *FRP* for short. 
Conal Elliott and Paul Hudak first developed the basic ideas and published 2007 the paper [Functional Reactive Animation](http://conal.net/papers/icfp97/)
On Hackage there a lot
of different FRP libraries, eg *elera*, *frpnow*, *grapefruit-frp*, *netwire*, *reactive-banana*, *reflex* and many more.

In this tuorial we use Reflex and Reflex-Dom. Reflex is a FRP implemetation written by Ryan Trinke.
Reflex is a strong foundation to handle events and values that change over time. 
Reflex-Dom is built on Reflex and on GHCJS.Dom. It allows you to write GUI programs that run in a 
Web Browser or as a 'native' application in Webkit. Reflex-Dom was written by Ryan Trinkle too.

Reflex-dom protects you from all the low level details of an FRP implementation. Writing GUI programs in reflex-dom is much fun.
You can really write GUI programs in a functional way and you can separate the GUI logic from the business logic.
It's not necessary to be a Haskell guru to write GUI programs with reflex-dom. 
A good understanding of basic Haskell with the concepts of *Functor*, *Applicative* and *Monad* is enough. 
Of course, the more experience you have, the easier it is.


# Basics of Functional Reactive Programming (FRP) and Reflex

## The Basic Ideas of Functional Reactive Programming

Normally input functions are impure. Assume a function *getChar* that reads a single character from the keyboard.
For different invocations the function *getChar*  normally returns a different character, depending on the key that was pressed on the keyboad .
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

Reflex uses 3 main time-dependent data types:

* Event
* Behavior
* Dynamic

A typical type declaration for a function could be:

```dispEvent :: MonadWidget t m => T.Text  -> Event t ClickInfo -> m ()```


Here the *t* is the time parameter. This parameter is always introduced with a precondition. In our case
```  MonadWidget t m => ```. The *m* is normally some monad. *ClickInfo* would be a user defined Haskell data type.

To call the above function, in some monadic context, you would write:

```dispEvent "This is my event" evClick```


Events and Behaviors are common data types in different FRP implementations. Dynamics, however, are probably unique
in Reflex.

## Event

Events occur at some points in time and they carry a value. 
The most prominent example for events are mouse clicks and pressing keys on a keyboard. 
The value of a keyboard event is normally the code of the pressed key.

During any given time frame, an *Event* is either occurring or not occurring; if it is occurring, it will contain a value of
the given type. This is shown in the following diagram:

![Events](https://github.com/hansroland/reflex-dom-inbits/raw/master/images//event.png "Events")


In Reflex the data type *Event* has the following simplified type:

```data Event t a```

'*a*' is the type of the event. I also call the value '*a*' the payload of the event. It can be more or less every Haskell data type. 
Sometimes we will use functions.

Events are the main work horses in Reflex. As we will see, it is very common to transform an event of type *a*
into an event of type *b*. 

The data type *Event* is an instance of the Haskell Functor type class. 
This allows event transformation with the well known *fmap* function:

```fmap :: (a -> b) -> Event a -> Event b```

Later we will see other functions to transform events.


## Behavior

A *Behavior* is a container for a value, that changes over time. Other than events, Behaviors always have a value. 
It is not possible to be notified when a Behavior changes.

In Reflex the data type *Behavior* has the following simplified type:

```data Behavior t a```


Behaviors can change their values only at time points where events occur. This is shown in the following diagram.

![Behavior](https://github.com/hansroland/reflex-dom-inbits/raw/master/images//behavior.png "Behavior")

To write a Reflex-dom application we rarely use Behaviors. We use *Dynamics* if we need values that change over time. 

## Dynamic

A *Dynamic* is a container for a value that can change over time and allows notifications on changes. 
Dynamics are special to Reflex. They are a combination of the types *Behavior* and *Event*.

```data Dynamic t a```


![Behavior](https://github.com/hansroland/reflex-dom-inbits/raw/master/images//dynamic.png "Behavior")

A Dynamic is a monad and therefore an Applicative too. 
When working with Dynamics it is very common to use applicative syntax.

# Before we really start coding...

## Used Library Versions

Today Hackage has reflex version 0.4 and reflex-dom version 0.3. 
However for both libraries there are newer and better versions on Github: [reflex-0.5](https://github.com/reflex-frp/reflex) and 
[reflex-dom.0.4](https://github.com/reflex-frp/reflex-dom)

The main improvements of the Github versions are:

* Use the *text* library with the data type *Text* instead of *String*. This gives performance.
* Data type *Event* is now a Functor. 
* Data type *Dynamic* is a Monad.

The last 2 changes make the programs simpler!

In this tutorial, we will use the library versions reflex-dom-0.4 and reflex-0.5 from Github. 
Unfortunately most of the examples will not compile with reflex-dom-0.3!
 

## Popular Language Extensions

To write Reflex programs, very often we use some of the following GHC Haskell language extensions

```{-# LANGUAGE OverloadedStrings #-}```

We need it, because *Reflex* uses *Text* instead of *String*. The extension *OverloadedStrings*  allows automatic conversion of string constants like "I'm a String"
to the correct string type. We don't need to pack and unpack the string constants ourselfs.

```{-# LANGUAGE RecursiveDo #-}```

Sometimes we need to access a value or an event from a DOM element before it is defined. The magic behind
*RecursiveDo* makes this possible.

```{-# LANGUAGE ScopedTypeVariables #-}```

Sometimes the compiler is unable to infer the type of a bound variable in a do-block. 
Or sometimes we want to document the type of such a variable. This makes it easier to understand.
With the extension *ScopedTypeVariables* GHC accepts such type annotations.

In the following example we specify the type of the value *name*.

~~~ { .haskell }
{-# LANGUAGE ScopedTypeVariables #-}
main :: IO ()
main = do
    putStrLn "What's your name"
    name :: String <- getLine
    putStrLn $ "Hello " ++ name
~~~

## Popular Imports

From all the 1001 libraries stored on Hackage, we will use only very few:

```import Reflex.Dom``` 

Ok this tutorial is about Reflex.Dom so we should import and use it. 
It's not necessary to import Reflex. Reflex.Dom reexports all the needed functions of Reflex.

```import qualified Data.Text as T```

As mentioned above, reflex-dom uses the data type *Text* instead of the type *String*. 
So we have to import it!

```import qualified Data.Map as Map```

Haskell Maps are very popular in *Reflex-dom*. They are used in a lot of functions.

```import Data.Monoid``` 

We normally use the function *mempty* to create an empty Map, 
and the function *mappend* rsp *(<>)* to combine two Maps.


## Some comments to the code examples

A lot of these examples could be written with less lines, just by using the monadic functions (>>=),
(>>), (=<<), (<<) etc.

Sometimes I used more lines in my code in order to have code that is easier to understand by beginners.

# A First Simple Reflex-Dom Example

Let's begin with a first simple reflex-dom example. This code is in the file *src/count01.hs*:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget $ display =<< count =<< button "ClickMe"
~~~

This example uses four reflex or reflex-dom functions:

* mainWidget
* display
* count
* button

Let's look at the types of these functions and what they do:

## Function: *mainWidget*

```mainWidget :: (forall x. Widget x ()) -> IO ()```

It sets up the reflex-dom environment. It takes an argument of type ```Widget``` and returns ```IO ()```

The type *Widget* is a little bit scary. However we never really need to work with the details of it. 

~~~ { .haskell }
type Widget x =
  PostBuildT
    Spider
    (ImmediateDomBuilderT
       Spider (WithWebView x (PerformEventT Spider (SpiderHost Global))))
~~~ 

*PostBuildT* is a monad transformer. It set's up a monadic environement for reflex-dom. 
As side effects, some of the reflex-dom functions will create the DOM elements. 
To follow this tutorial you don't need to understand the concepts behind monad transformers.

The function *mainWidget* has two sister functions *mainWidgetWithCss* and *mainWidgetWithHead".
We will see them later.

## Function: *display*

```display :: (Show a, ... ) => Dynamic t a -> m ()```

The function takes an argument of type ```Dynamic t a``` and returns unit in the current monad.
It uses the Show instance of the datatype *a* to build a string representation of its first parameter.
Then it creates in the DOM a text element, where it displays this string. 
As mentioned above, the creation of the DOM element is a monadic side effect.

*display* has a precondition of *Show a*. It has other preconditions too, 
If you use *mainWidget* or one of its sister functions, the other preconditions are normally fullfilled automatically.
Thefore I don't show them here and in the examples to follow..

## Function: *count*

```count :: (Num b, ...) =>  Event t a -> m (Dynamic t b)```

The *count* function takes an event as argument and creates a Dynamic.
In this Dynamic the function count's up the number of times the event occured or fired.

## Function: *button*

```button :: (...) => Text -> m (Event t ())```

The *button* function takes a text as argument. It creates a DOM button element with the text, and returns
an event with *()* as payload.

Now it's easy to understand the whole line *mainWidget $ display =<< count =<< button "ClickMe"*:

* Clicking on the button creates or triggers events, 
* The function *count* creates a dynamic value with the total number of these events.
* The function *display* creates a DOM element with a string representation of this number and displays it as DOM element.

**Try it!**

# Creating Other DOM Elements

Till now we used the 2 helper functions *button* and *display* to create DOM elements.
Reflex has 2 other very frequently used helper functions to create DOM elements:

* text
* dynText

## Function: *text*

```text :: (...) => Text -> m ()```

It is very simple: It just displays the text in the DOM. During program execution the text displayed in the DOM never changes. 
The text is not of type *Dynamic t Text* but only of *Text*, hence it is static!!

## Function: *dynText* 

```dynText :: (...) => Dynamic t Text -> m ()```

The function *dynText* does more or less the same as the function *text*. 
However, the function argument is of type *Dynamic t Text*, so now the text may change during the execution of the program!!
We will see some examples later.

## The Function family *el*, *elAttr*, *elClass*, *elDynAttr*, *elDynClass*

Reflex-dom has 2 function families to create all the different kinds of DOM elements:

* el, elAttr, elClass, elDynAttr, elDynClass
* el', elAttr', elClass', elDynAttr', elDynClass'

First we will look at the family without the primes ' in the name. 
The second function family with the primes in the name we will cover in a later section.

## Function: *el*

This function has the type signature:

```el :: (...) => Text -> m a -> m a```

It takes the type of the DOM element as a first argument. The second argument is either the text of the element
or a child element.

The file *src/dom01.hs* contains a typical first example and shows basic usage of the function *el*:

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

```el "br" $ return ()``` 

**Try it!!**

## Function: *elAttr*

With the function *el*, we can't create a DOM element with attributes, eg a link:

```<a target="_blank" href="http://google.com">Google!</a>```

To add attributes, reflexse functions all have -dom has a function *elAttr*. It has the type:

```elAttr :: (...) => Text -> Map Text Text -> m a -> m a```

The function *elAttr* is similar to the function *el*, but it takes an additional 
parameter of type *Map Text Text*. This parameter contains the attributes of the DOM element.
A Map is a *key-value* relation,
In the above link example, *target* and *href* are the keys and *"_blank"* and *"http://google.com"*
are the values.

The file *src/dom03.hs* contains an example for *elAttr*:

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

The library Data.Map defines a little helper function (=:) to create a singelton Map.

```(=:) :: k -> a -> Map k a```

Two singleton maps are then appended / merged with the (<>) operator from Data.Monoid.


## Function: *elClass*

The function *elClass* allows you to specify the name of a class to be used by the Cascaded Style Sheets (CSS).

It has the following type:

```elClass :: (...) => Text -> Text -> m a -> m a```

The first parameter is again the type of the DOM element. The second is the name of the CSS class.

A small example:

```elClass "h1" "mainTitle" $ text "This is the main title"```

In HTML:

```<h1 class="mainTitle">This is the main title</h1>```

## Function: *elDynAttr*

All the above functions allow us to define static DOM elements. 
But you cannot change them while the program is running!

With the function *elDynAttr*, as the name says, you can specify attributes, 
that change during program execution. It has the following type:

```elDynAttr  :: (...) => Text -> Dynamic t (Map Text Text) -> m a -> m a```

You guessed it, the first parameter is again the type, and the second is a map with **any** attribute,
you can use for your DOM element. However, this time this map is wrapped in a Dynamic.

To use the function *elDynAttr* we must somehow create a Dynamic. 
Reflex has several functions to create Dynamic values. As a first example, we will use the function *toggle*:

```toggle :: (...) => Bool -> Event t a -> m (Dynamic t Bool)```

The function toogle create a new Dynamic using the first parameter as the initial value
and flips this boolean value every time the event in the second parameter occurs.

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
* *dynAttrs* contains the *Dynamic t (Map Text Text)*. It is created with an applicative call to the function *attrs*.
* The function *attrs* containsthe 'business logic' of this example: 
It decides on the boolean parameter about the color of the DOM element.
* Please note, that the function *attrs* is a normal pure function as we know and love them since Haskell kindergarden!
* Transforming a Dynamic value or combining several Dynamic values with the help of applicative syntax and a pure fucntion is a common pattern in Reflex.

## Function *elDynClass*

The function *elDynClass* is similar to *elClass* but here the type of the parameter to specify the name of the CSS class is Dynamic.
This allows you to change the CSS-class of the element dynamically during runtime.

The function has the type:

```elDynClass :: (...) => Text -> Dynamic t Text -> m a -> m a```

# Main Functions

With the function *elAttr*, *elClass*, *elDynAttr*, and *elDynClass* we can reference selectors of CSS style sheets.
But with the function *mainWidget* we have no possibility to specify one or several css files. 
As mentioned above, *mainWidget* has two sister function, and they solve this little issue..

This is not a tutorial about Cascaded Style Sheets. Therefore I don't spice up my examples with sexy css.
In the next 2 examples, I reference a file "css/simple.css". It contains the most simple css ever possible:

```h1 { color: Green; }```

It just uses a green color for all header-1 DOM elements.


## Function *mainWidgetWithCss*

This function has the following type:

```
mainWidgetWithCss :: ByteString -> 
        Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> 
        IO ()
```

Again it looks scary. The first parameter is a ByteString containing your css specifications.
Normally you don't want to have the css specs in your Haskell program. You want them in a separate file.

On Hackage, there is a library called *file-embed*. 
It contains a function *embedFile* that allows you, during compilation, to embed  the contents of a file into your source code.
This function uses Template Haskell, so we need the GHC language extension for Template Haskell.

The second paramter of *mainWidgetWithCss* is just the same thing as the parameter of the function *mainWidget*, 
we used for all our examples till now. So it is the HTML body element.

The file *src/main01.hs* contains a full example:

~~~ { .haskell }
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import Data.FileEmbed

main :: IO ()
main = mainWidgetWithCss css bodyElement
   where css = $(embedFile "css/simple.css")

bodyElement :: MonadWidget t m => m ()
bodyElement =  el "div" $ do
     el "h1" $ text "This title should be green"
     return ()
~~~

Comments:

* Template Haskell runs at compile time. If you change something in your css file, you have to recompile 
and redeploy your application.
* The path to the css file (*css/simple.css* in the above example) is used by the compiler and therefore relative to your working
directory during compile time.
* If the css file does not exist, or the path is wrong, you will get an error during compile time.

## Function *mainWidgetWithHead*

The function *mainWidgetWithHead* has the following type:

``` 
mainWidgetWithHead :: 
   Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> 
   Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO () 
```
The function *mainWidgetWithHead* takes two parameters as we know them from the functions *mainWidget* and *mainWidgetWithCss*.
The first parameter is the HTML head element, and the second parameter the HTML body element.

File *src/main02.hs* contains the example:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import Data.Map as Map

main :: IO ()
main = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "Main Title"
  styleSheet "css/simple.css"
  where
    styleSheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()

bodyElement :: MonadWidget t m => m ()
bodyElement =  el "div" $ do
     el "h1" $ text "This title should be green"
     return ()
~~~

Comments:

* We use the function *Map.fromList* to create the map parameter for the function *elAttr*.
* The path to the css file (*css/simple.css* in the above example) is used during run time. 
It is relative to the directory you run your program from.
* Depending on how you run your reflex-dom program, you have to copy your *.css files to the correct directory.
* If the css file does not exist, or the path is wrong, the browser or WebkitGtk will simply ignore your css specs.
* If you change your css files, the changes become active after a restart of your program.
* If you run your program in the interactive shell with *ghcjs --interactive*, this example will not work. 
The interactive shell of ghcjs does not serve any files.
* It is possible, to specify other options in your header element.
* Unfortunately you have to annotate the type ```:: MonadWidget t m => m ()``` for the functions *headElement* and *bodyElement*. 
GHC is not able to infer these types and gives you a not so nice error message.

## Summary

* Use *mainWidget* for small examples.
* Use *mainWidgetWithCss* if you don't want anybody to change your CSS specifications.
* Use *mainWidgetWithHead* for professional projects.


# Basic Event Handling

In this section we will have a first look on how to use events. 

## Function *foldDyn*

Remember the first example *src/dom01.hs* with the counter. There we used the predefined function *count*.
We will now do the same example, but we handle the events ourselfs with the *foldDyn* function.

The function *foldDyn* has the type   

```foldDyn :: (...) => (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)```

It works similar to the wellknown *foldr* function from the list data type. 
It first creates a Dynamic with the value specified in the second parameter.
Every time an event (third parameter) occurs, 
it uses the fold function in the first parameter and folds up the values of the event.
 
File *src/event01.hs* contains an example for *foldDyn*

~~~ { .haskell .numberLines}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO () 
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  rec el "h2" $ text "Counter as a fold"
      numbs <- foldDyn (+) (0 :: Int)  (1 <$ evIncr)
      el "div" $ display numbs
      evIncr <- button "Increment"
  return ()
~~~

Look at the line: ```numbs <- foldDyn (+) (0 :: Int)  (1 <$ evIncr)```:

* We use the normal addition as a fold function.
* We have to specify the type of the initial value. The compiler does not know whether we want to count up numbers of type *Int* or *Integer* or even *Float*.
* evIncr has the type ```Event t ()```. We cannot use () as an argument for the (+) fold function. Therfore we use
applicative syntax to replace *()* by the number *1*. 1 we can use together with our fold function (+)!
* This is the first example that uses *event tranformation* 

Please note, that in reflex-dom the implemention of *count* differs from our example above.

## Function *leftmost*

Now we want to have a second button to decrement our counter. 
To combine the events of the 2 buttons we use the function *leftmost*:

```leftmost :: Reflex t => [Event t a] -> Event t a```

If an event in the array of the first parameter occurs, the function *leftmost* returns this event. 
If two ore more events in the array occur simultaneously, 
the function *leftmost* returns the leftmost of all the simultaneously occuring events.

File *src/event02.hs* contains the example:

~~~ { .haskell .numberLines}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  rec el "h2" $ text "Combining Events with leftmost"
      counts <- foldDyn (+) (0 :: Int) $ leftmost [1 <$ evIncr, -1 <$ evDecr]
      el "div" $ display counts
      evIncr <- button "Increment"
      evDecr <- button "Decrement"
  return ()
~~~

## Function *mergeWith*

Assume, it would be possible to click in the above example both buttons simultaneously. 
If we click both buttons together, the function *leftmost* returns only *evIncr* and we loose *evDecr*.
In situations, where we are not allowed to loose events, we can use the function *mergeWith*.

The function *mergeWith* has the following type:

```mergeWith :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a```

It uses a function of type *(a -> a -> a)* to combine the payloads of all simultaneously occuring events.

File *src/event03.hs* contains the full example:

~~~ { .haskell .numberLines}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  rec el "h2" $ text "Combining Events with mergeWith and foldDyn"
      dynCount <- foldDyn (+) (0 :: Int)  (mergeWith (+) [1 <$ evIncr, -1 <$ evDecr])
      el "div" $ display dynCount
      evIncr <- button "Increment"
      evDecr <- button "Decrement"
  return ()
~~~

## Function Application as Fold Function

Now in addition to the increment and decrement buttons, we want a third button to reset the counter to zero.
If we still want to use normal addition as a folding function, to reset, we have to read out the current value of the counter
and replace the reset event with the negative value of the counter. This is very messy!!

A better approach is to use events, that carry functions as payloads. 
We transform the payload of the event of the increment button to the function ```(+ 1)```, 
the payload of the event of the decrement button to the function ```(+ (-1))```,
and the payload of the event of the reset button to the function ```const 0```.
As a fold function we then use normal function application *($)* to apply the transformed function to the current value of our counter.

File *src/event04.hs* has the full example:

~~~ { .haskell .numberLines}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
   el "h2" $ text "Using foldDyn with function application"
   rec dynNum <- foldDyn ($) (0 :: Int) $ leftmost [(+ 1) <$ evIncr, (+ (-1)) <$ evDecr, const 0 <$ evReset]  
       el "div" $ display dynNum
       evIncr <- button "Increment"
       evDecr <- button "Decrement"
       evReset <- button "Reset"
   return ()
~~~

Using function application as a fold function over a current value is very powerful!!

# Predefined Input Widgets

In this section, we look at the standard reflex-dom input elements. They are predefined and easy to use.
We already have seen buttons, hence we will not cover them here.
 
For most of the input widgets, reflex-dom defines two data structures

* configuration record 
* element record.

## Text Input Fields

*TextInput* fields are of the most popular input widgets in GUI applications. 
They allows the user to enter texual data, eg their name, address, phone and credit card numbers and so on.

The configuration record has the following definition:

~~~ { .haskell }
data TextInputConfig t
   = TextInputConfig { _textInputConfig_inputType :: Text
                     , _textInputConfig_initialValue :: Text
                     , _textInputConfig_setValue :: Event t Text
                     , _textInputConfig_attributes :: Dynamic t (Map Text Text) }
~~~

and the element record is defined as:

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

The type *GhcjsDomSpace* originates from a lower level GHCJS library, that is used to build Reflex-dom. You will not use it in your program.

The function to create a text input element is:

```textInput :: (...) => TextInputConfig t -> m (TextInput t)```

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

### Syntactic Sugar with (&) and (.~)

*TextInputConfig* is a normal Haskell record structure with accessor functions. 
You can use the following code to create a textInput widget configured with an initial value:

```textInput def { _textInputConfig_initialValue = "0"}```

However Reflex-dom uses lenses to give us syntactic sugar to populate these configuration records.

With the two combinators *(&)* and *(.~)*. we can write:

```textInput $ def & textInputConfig_initialValue .~ "input"```

Note that the underscore (_) in front of *_textInputConfig* has gone.

If you are not familar with lenses, you can use the standard Haskell record syntax.

### Examples

The first example in *src/textinput01.hs* is very simple:

A TextInput widget where you can enter some text. The text you entered is immediately shown in a second widget.
It uses the *dynText* function we described earlier and the function *value* to extract the current value out of the TextInput widget.

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ do
  t <- textInput def
  dynText $ value t
~~~

The next example in *src/textinput02.hs* shows how to use the different options to configure a TextInput widget.

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import Data.Monoid ((<>))

main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  el "h2" $ text "Text Input - Configuration"

  el "h4" $ text "Max Length 14"
  t1 <- textInput $ def & attributes .~ constDyn ("maxlength" =: "14") 
  dynText $ _textInput_value t1

  el "h4" $ text "Initial Value"
  t2 <- textInput $ def & textInputConfig_initialValue .~ "input"
  dynText $ _textInput_value t2

  el "h4" $ text "Input Hint"
  t3 <- textInput $ 
        def & attributes .~ constDyn("placeholder" =: "type something")
  dynText $ _textInput_value t3

  el "h4" $ text "Password"
  t4 <- textInput $ def & textInputConfig_inputType .~ "password"
  dynText $ _textInput_value t4

  el "h4" $ text "Multiple Attributes: Hint + Max Length"
  t5 <- textInput $  def & attributes .~ constDyn ("placeholder" =: "Max 6 chars" <> "maxlength" =: "6")
  dynText $ _textInput_value t5

  el "h4" $ text "Numeric Field with initial value"
  t6 <- textInput $ def & textInputConfig_inputType .~ "number"
                        & textInputConfig_initialValue .~ "0"
  dynText $ _textInput_value t6
  
  return ()
~~~

## TextAreas

TextInput fields have only one input text line. If you want several input lines, you must use TextAreas.

TextAreas are built in the same way as TextInput fields: There is a configuration record and a data record. 
They are similar to the record for TextInput fields. The names are different: *_textInput* is replaced by *_textArea*.

## Using Several TextInput Fields

With the TextInput and TextArea widgets we are now able to write our first usefull GUI program in Haskell:
It is a RGB color viewer. We enter the 3 color components, and the program shows us the resulting RGB color.

The file *src/colorviewer.hs* contains the example:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import           Data.Map
import qualified Data.Text as T

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = do
    el "h2" $ text "Several Text Inputs: RGB Viewer"
    el "div" $ text "Enter RGB component values as numbers between 0 and 255"
    dfsRed <- labledBox "Red: "
    dfsGreen <- labledBox "Green: "
    dfsBlue <- labledBox "Blue: "
    textArea $ 
        def & attributes .~ (styleMap <$> value dfsRed <*> value dfsGreen <*> value dfsBlue) 
    return ()

labledBox :: MonadWidget t m => T.Text -> m (TextInput t)
labledBox lbl = el "div" $ do
    text lbl
    textInput $ def & textInputConfig_inputType .~ "number"
                    & textInputConfig_initialValue .~ "0"
    
styleMap :: T.Text -> T.Text -> T.Text -> Map T.Text T.Text
styleMap r g b = "style" =: mconcat ["background-color: rgb(", r, ", ", g, ", ", b, ")"]
~~~

As soon as you change the value in one of the TextInput fields, the background color of the TextArea widget changes!

Comments:

* The function *labledBox* combines a TextInput field with a label.
* The main magic happens in the line ```styleMap <$> value dfsRed <*> value dfsGreen <*> value dfsBlue```. 
We use again applicative syntax to call the function *styleMap* with the current values of our 3 input fields.
* The function styleMap contains our 'business logic'. It creates the correct string to color the resulting TextArea widget.
* Again the function *styleMap* is a normal pure Haskell function. 
* The example shows, how to process the input of several TextInput fields

## Checkboxes

Checkboxes are rather simple, therefore the configuration and the element records are simple too.

The configuration record:

~~~ { .haskell }
data CheckboxConfig t
    = CheckboxConfig { _checkboxConfig_setValue :: Event t Bool
                     , _checkboxConfig_attributes :: Dynamic t (Map Text Text) }
~~~

The *Default* instance:

~~~ { .haskell }
instance Reflex t => Default (CheckboxConfig t) where
  def = CheckboxConfig { _checkboxConfig_setValue = never
                       , _checkboxConfig_attributes = constDyn mempty }
~~~

The element function:

```checkbox :: (...) => Bool -> CheckboxConfig t -> m (Checkbox t)```

### Example *src/checkbox01.hs*

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import qualified Data.Text as T

main :: IO ()
main = mainWidget $ el "div" $ do
  el "h2" $ text "Checkbox (Out of the box)"
  cb <- checkbox True def
  text "Click me"
  el "p" $ return ()
  let dynState = checkedState <$> value cb 
  dynText dynState 

checkedState :: Bool -> T.Text
checkedState True = "Checkbox is checked"
checkedState _    = "Checkbox is not checked"
~~~

This is the most simple way to create and use a checkbox. However, you have to click exactly into 
the small square to change the state of the checkbox. When you click at the label *Click me* it does not
change it's state. This is very user unfriendly!

### Example *src/checkbox02.hs*

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import qualified Data.Text as T

main = mainWidget $ el "div" $ do
  el "h2" $ text "Checkbox - User friendly"
  cb <- el "label" $ do
    cb1 <- checkbox True def
    text "Click me"
    return cb1
  el "p" $ return ()
  let dynState = checkedState <$> value cb  
  dynText dynState 

checkedState :: Bool -> T.Text
checkedState True = "Checkbox is checked"
checkedState _    = "Checkbox is not checked"
~~~

This example shows how to fix the issue with checkbox01.hs.
We create a combined widget: The checkbox element is a child of a *label* element. 
The result of the combined widget is the checkbox.

It works now as expected: To change the state of the checkbox, you can either click into the small square 
or at the text *Click me*

## Radio Buttons

~~~ { .haskell }

~~~

## Drop Downs

~~~ { .haskell }

~~~

## Listboxes

~~~ { .haskell }

~~~

# The Function family *el'*, *elAttr'*, *elClass'*, *elDynAttr'*, *elDynClass'* 

In this section of the tutorial, we will look at the second function family to create DOM elements.
The names of these functions all end with a prime (').


## Function *button* - Do it yourself!

*button* is a simple helper function to create a button in the DOM. It returns an event
with unit *()* in the payload.
Unfortunately, this function is not very flexible: We can only specify the label of the button and nothing more.
If we want to add a CSS class or directly some attributes we have to define the button with on of the *el* functions:


### Communication with a Web Server
