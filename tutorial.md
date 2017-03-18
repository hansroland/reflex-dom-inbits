# Writing GUI Programs in Haskell - A Beginner Friendly Tutorial using reflex-dom

This is a beginner-friendly tutorial. It shows how to write Haskell programs  with a graphical user interface using reflex-dom. 

Today most computer programs have a graphical user interface (GUI). 
However Haskell programs with a GUI are still rare. Haskell programs normally use a command line interface (CLI). 
For a long time, there were no good options to write GUI programs in Haskell. 
It is difficult to match the event driven nature of a GUI program onto the functional paradigm. 
The traditional object oriented way to program a GUI application uses callbacks. 
These callbacks need a lot of global state and in Haskell managing state is not easy.

To solve these problems the Haskell community developed a lot of new new ideas. 
One of them is called *Functional Reactive Programming*, *FRP* for short. 
Conal Elliott and Paul Hudak first developed the basic ideas and published them in 2007 in the paper [Functional Reactive Animation](http://conal.net/papers/icfp97/).
On Hackage you can find a lot
of different FRP libraries, eg *elera*, *frpnow*, *grapefruit-frp*, *netwire*, *reactive-banana*, *reflex* and many more.

In this tuorial we use Reflex and Reflex-Dom. Reflex is a FRP implemetation written by Ryan Trinkle.
Reflex is a strong foundation to handle events and values that change over time. 
Reflex-Dom is built on Reflex and on GHCJS.Dom. It allows you to write GUI programs that run in a 
Web Browser or as a 'native' application in WebkitGtk. Reflex-Dom was written by Ryan Trinkle too.

Reflex-dom protects you from all the low level details of an FRP implementation. Writing GUI programs in reflex-dom is much fun.
You can really write GUI programs in a functional way and you can separate the GUI logic from the business logic.
It's not necessary to be a Haskell guru to write programs with reflex-dom. 
A good understanding of basic Haskell with the concepts of *Functor*, *Applicative* and *Monad* is enough. 
Of course, the more experience you have, the easier it is.

# Basics of Functional Reactive Programming (FRP) and Reflex

## The Basic Ideas of Functional Reactive Programming

Normally input functions are impure. Assume a function *getChar* that reads a single character from the keyboard.
For different calls the function *getChar*  normally returns a different character, depending on the key that was pressed on the keyboad .
Therefore such a function is not a pure Haskell function. As everybody knows Haskell uses monadic IO actions to avoid impure functions.
Functional Reactive Programming (FRP) takes an other approach. All potentially impure functions have a time parameter
and the FRP system makes sure, that every call to such a function is done with a new and unique time value.

In pseudo code:

~~~ { .haskell }
getChar :: Time -> Char

getChar 1     -- this returns eg a 'F'
getChar 2     -- this returns eg a 'R'
~~~ 

Everytime you call *getChar* with a parameter of 1, it will return the character 'F'. 
However the FRP framework will not allow you to do multiple calls to *getChar* with the parameter 1.

With this trick, *getChar* is now a pure function.

In the type declaration of a reflex function the time parameter is always shown explicitely.
Normally a type parameter with the name *t* is used.
However it's **never** necessary to supply this parameter as a programmer when you call the function.

Reflex has 3 main time-dependent data types:

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
Sometimes we will even use functions as event payloads.

Events are the main work horses in Reflex. As we will see, it is very common to transform an event of type *a*
into an event of type *b*. 

The data type *Event* is an instance of the Haskell *Functor* type class. 
This allows easy event transformation with the well known *fmap* function:

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

The data type *Dynamic* is an instance of the Haskell *Applicative* type class.
It is very common to use applicative syntax when working with Dynamics.

# Before we really start coding...

## Used Library Versions

Today Hackage has reflex version 0.4 and reflex-dom version 0.3. 
However for both libraries there are newer and much better versions on Github: [reflex-0.5](https://github.com/reflex-frp/reflex) and 
[reflex-dom.0.4](https://github.com/reflex-frp/reflex-dom)

The main improvements of the Github versions are:

* Use the *text* library with the data type *Text* instead of the data type *String*. This gives performance.
* Data type *Event* is now a Functor. 
* Data type *Dynamic* is a Monad.

The last 2 changes make the programs much simpler!

In this tutorial, we will use the newer library versions reflex-dom-0.4 and reflex-0.5 from Github. 
Unfortunately most of the examples will not compile with reflex-dom-0.3!
 

## Popular Language Extensions

To write Reflex programs, very often we use some of the following GHC Haskell language extensions:

```{-# LANGUAGE OverloadedStrings #-}```

We need it, because *Reflex* uses *Text* instead of *String*. The extension *OverloadedStrings*  allows automatic conversion of string constants like "I'm a String"
to the correct string type. We don't need to pack and unpack the string constants ourselfs.

```{-# LANGUAGE RecursiveDo #-}```

Sometimes we need to access a value or an event from a DOM element before it is defined. The magic behind
*RecursiveDo* makes this possible.

```{-# LANGUAGE ScopedTypeVariables #-}```

Sometimes the compiler is unable to infer the type of a bound variable in a do-block. 
Or sometimes we want to document the type of such a variable. This makes it easier to understand the code.
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
It's not necessary to import Reflex. Reflex.Dom re-exports all the needed functions of Reflex.

```import qualified Data.Text as T```

As mentioned above, reflex-dom uses the data type *Text* instead of *String*. 
So we have to import it!

```import qualified Data.Map as Map```

Haskell maps are very popular in *Reflex-dom*. They are used in a lot of functions.

```import Data.Monoid``` 

We normally use the function *mempty* to create an empty map,
the function (=:) to create a singelton map
and the function *mappend* rsp *(<>)* to combine two maps.


## Some comments to the code examples

A lot of the following examples could be written with less lines, just by using the monadic functions (>>=),
(>>), (=<<), (<<) etc.

Sometimes I use more lines in my code in order to have code that is easier to understand by beginners.

# A First Simple Reflex-Dom Example

Let's begin with a first simple reflex-dom example. The code is in the file *src/count01.hs*:

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
As side effects, some of the reflex-dom functions will create and change the DOM elements. 
To follow this tutorial you don't need to understand the concepts behind monad transformers.

The function *mainWidget* has two sister functions *mainWidgetWithCss* and *mainWidgetWithHead*.
We will see them later.

## Function: *display*

```display :: (Show a, ... ) => Dynamic t a -> m ()```

The function takes an argument of type ```Dynamic t a``` and returns unit in the current monad.
It uses the *Show* instance of the datatype *a* to build a string representation of its first parameter.
Then it creates a text element in the DOM, where it displays the string. 
As mentioned above, the creation of the DOM element is a monadic side effect.

*display* has a precondition of *Show a*. It has other preconditions too. 
If you use *mainWidget* or one of its sister functions, the other preconditions are normally fullfilled automatically.
Thefore I don't show them here and in the most examples to follow..

## Function: *count*

```count :: (Num b, ...) =>  Event t a -> m (Dynamic t b)```

The *count* function takes an event as argument and creates a Dynamic.
In this Dynamic the function counts up the number of times the event occured or fired.

## Function: *button*

```button :: (...) => Text -> m (Event t ())```

The *button* function takes a text as argument. It creates a DOM button element labelled with the text, and returns
an event with *()* as payload.

Now it's easy to understand the whole line *mainWidget $ display =<< count =<< button "ClickMe"*:

* Clicking on the button creates or triggers an event. 
* The function *count* creates a *Dynamic* value with the total number of these events.
* The function *display* creates a DOM element with a string representation of this number and displays it as DOM element.

**Try it!**

# Creating other DOM Elements

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
However, the function argument is of type *Dynamic t Text*. The text may change during the execution of the program!!
We will see some examples later.

## The Function family *el*, *elAttr*, *elClass*, *elDynAttr*, *elDynClass*

Reflex-dom has 2 function families to create all the different kind of DOM elements:

* el, elAttr, elClass, elDynAttr, elDynClass
* el', elAttr', elClass', elDynAttr', elDynClass'

First we will look at the family without the primes ' in the name. 
We will cover the second function family with the primes in the names in a later section.

## Function: *el*

This function has the type signature:

```el :: (...) => Text -> m a -> m a```

It takes a text string with the HTML-type of the DOM element as a first parameter. 
The second parameter is either the text of the element or a child element.

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

**Try it!!**

Unfortunately, this web page is very static, It doesn't react to any input actions of the user.
Later, we will learn how make more dynamic web pages. 

## Function *blank*

If you use HTML elements without any values or without a child, you could simply write:

```el "br" $ return ()``` 

Because this ```return ()``` is used frequently and we need a ```$```, there is a little helper with the following definition:

~~~ { .haskell }
blank :: forall m. Monad m => m ()
blank = return ()
~~~

Hence we can write: ```el "br" blank```


## Function: *elAttr*

With the function *el*, we can't create a DOM element with attributes, eg a link:

```<a target="_blank" href="http://google.com">Google!</a>```

To add attributes, reflex-dom has a function *elAttr*. It has the type:

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

Two singleton maps are then merged / appended with the (<>) operator from Data.Monoid.


## Function: *elClass*

The function *elClass* allows you to specify the name of the class to be used by the Cascaded Style Sheets (CSS).

It has the following type:

```elClass :: (...) => Text -> Text -> m a -> m a```

The first parameter is again the HTML-type of the DOM element. The second is the name of the CSS class.

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

* We need recursive do: We refer to the event *evClick* before it's defined.
* *dynBool* contains our value of type *Dynamic t bool*. It is created by the *toggle* function.
* *dynAttrs* contains the *Dynamic t (Map Text Text)*. It is created with an applicative call to the function *attrs*.
* The function *attrs* contains the 'business logic' of this example: 
It decides on the boolean parameter about the color of the DOM element.
* Please note, that the function *attrs* is a normal pure function as we know and love them since Haskell kindergarden!
So if you have a program that needs some input values, you can easily write a reflex-dom frontend, without changing your logic!
* Transforming a Dynamic value or combining several Dynamic values with the help of applicative syntax and a pure function is a common pattern in Reflex.

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
Normally you don't want to have the css specs embeded in your Haskell program. You want them in a separate file.

On Hackage, there is a library called *file-embed*.
It contains a function *embedFile* that allows you, during compilation, to embed the contents of a file into your source code.
This function uses a GHC feature called *Template Haskell*. Hence we need the GHC language extension for Template Haskell.
And we need an additional import (*Data.FileEmbed*).

The second paramter of *mainWidgetWithCss* is just the same thing as the parameter of the function *mainWidget*, 
we used for all our examples till now. It's the HTML body element.

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
and re-deploy your application.
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
* Unfortunately you have to annotate the type ```:: MonadWidget t m => m ()``` for the functions *headElement* and *bodyElement*. GHC is not able to infer these types and gives you a not so nice error message.
* The *title* element in the header, will be used in the page tab of your browser.

## Summary

* Use *mainWidget* for small examples.
* Use *mainWidgetWithCss* if you don't want anybody to change your CSS specifications.
* Use *mainWidgetWithHead* for professional projects.


# Basic Event Handling

In this section we will have a first look on how to use events. 

## Function *foldDyn*

Remember the first example *src/dom01.hs* with the counter. There we used the predefined function *count*.
We will now do the same example, but we handle the events ourselfs with the *foldDyn* function.

The function *foldDyn* has the type:  

```foldDyn :: (...) => (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)```

It works similar to the well known *foldr* function from the list data type. 
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
applicative syntax to replace the event payload *()* by the number *1*. 1 we can use together with our fold function (+)!
* This is the first example that uses *event tranformation* 

Please note, that in reflex-dom the implemention of *count* differs from our example above.

## Function *leftmost*

Now we want a second button to decrement our counter. 
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
The challenge is this reset: To continue to use normal addition as a fold function, we would have to read out the current value of the counter
and replace the reset event with the negative value of the counter. This, however, is very messy!!

A better approach is to use events, that carry functions as payloads. 
We transform 

* the payload of the event of the increment button to the function ```(+ 1)```, 
* the payload of the event of the decrement button to the function ```(+ (-1))```,
* the payload of the event of the reset button to the function ```const 0```.

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

Using function application as a fold function over a current value is very powerful!! W'll see more examples.

# Predefined Input Widgets

In this section, we look at the standard reflex-dom input elements. They are predefined and easy to use.
We already have seen buttons, hence we will not cover them here again.
 
For most of the input widgets, reflex-dom defines two data structures

* a configuration record 
* an element record.

## Text Input Fields

*TextInput* fields are of the most popular input widgets in GUI applications. 
They allow the user to enter texual data, eg their name, address, phone and credit card numbers and so on.

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

We will see more configuration records. They are all instances of the type class *Default*.

### The function *never*

``` never :: Event t a```

It's an event, that never occurs.


### The function *constDyn*

Note the type of the _textInputConfig_attributes: It's ```Dynamic t (Map Text Text)```.
To create a Dynamic map we can use the function *constDyn*: 
It takes an value of type ```a``` and returns a value of type ```Dynamic t a```. 
Of course, a Dynamic created with *contDyn* will not change while our program is running.


~~~ { .haskell }
constDyn :: Reflex t => a -> Dynamic t a
~~~

### Syntactic Sugar with (&) and (.~)

*TextInputConfig* is a normal Haskell record structure with accessor functions. 
You can use the following code to create a TextInput widget configured with an initial value:

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
  el "h2" $ text "Simple Text Input"
  ti <- textInput def
  dynText $ value ti
~~~

### The Type Class *HasValue*

In the above example the value *ti* is of type *TextInput*. 
The function *dynText* needs an parameter of type *Dynamic t Text*. 
So the function *value* should have the type: ```value :: TextInput -> Dynamic t Text```.

However, this is not quite true!
In the section about checkboxes you will see a function again called *value*. It will have the type: 

```value :: Checkbox -> Dynamic t Bool``` .

Reflex-dom uses advanced Haskell type level hackery to define this *value* function polymorphicly. For you it's simple:
The function *value* normally does what you naturally expect! 
Most of the predefined input widgets are instances of the type class *HasValue*. Hence most of these widgets
support the *value* function. All of these *value* functions return *Dynamic* values.

The next example in *src/textinput02.hs* shows some examples how to configure a TextInput widget.

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
    el "h2" $ text "RGB Viewer"
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
* The function *styleMap* is again a normal, simple, pure Haskell function! 
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

The first pramameter is the initial state of the checkbox: True for checked, False for unchecked.

### Example *src/checkbox01.hs*

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import qualified Data.Text as T

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement =  el "div" $ do
  el "h2" $ text "Checkbox (Out of the box)"
  cb <- checkbox True def
  text "Click me"
  el "p" blank
  let dynState = checkedState <$> value cb 
  dynText dynState 

checkedState :: Bool -> T.Text
checkedState True = "Checkbox is checked"
checkedState _    = "Checkbox is not checked"
~~~

As mentioned above, here the function *value* is used with the type signature: ```value :: Checkbox -> Dynamic t Bool``` .

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
  el "p" blank
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

Unfortunately basic reflex-dom does not contain a simple predefined function to create radio buttons. 
We will learn how to write our own radio button function later in the chapter *Defining your own events*.

However there is a library reflex-dom-contrib on Github: [https://github.com/reflex-frp/reflex-dom-contrib](https://github.com/reflex-frp/reflex-dom-contrib).

This library also defines a configuration record and a widget record.

The configuration record is defined as:

~~~ { .haskell }
data WidgetConfig t a
    = WidgetConfig { _widgetConfig_setValue :: Event t a
                   , _widgetConfig_initialValue :: a
                   , _widgetConfig_attributes :: Dynamic t (Map Text Text)
                   }
~~~

The widget record is defined as:

~~~ { .haskell }
data HtmlWidget t a = HtmlWidget
    { _hwidget_value    :: Dynamic t a
      -- ^ The authoritative value for this widget.
    , _hwidget_change   :: Event t a
      -- ^ Event that fires when the widget changes internally (not via a
      -- setValue event).
    , _hwidget_keypress :: Event t Int
    , _hwidget_keydown  :: Event t Int
    , _hwidget_keyup    :: Event t Int
    , _hwidget_hasFocus :: Dynamic t Bool
    }
~~~

From this library we use the function *radioGroup*:

~~~ { .haskell }
radioGroup :: (Eq a, ...) => Dynamic t T.Text -> Dynamic t [(a, T.Text)] -> GWidget t m (Maybe a)
~~~

Remember: In HTML a radio button in a list looks like:

~~~ { .html }
<li><input type="radio" name="size" value="Large">LARGE</li>
~~~

The first parameter of the function *radioGroup* is a *Dynamic T.Text* that is used to create the *name* attribute (*size* in the above HTML).


The second parameter of the function *radioGroup* takes a list of tuples. The left component of one tuple contains a value. 
This value will be the payload of the event, that is created when the radio button is clicked. 
The type of this value must be an instance of the *Eq* type class.
The right component of the tuple will be used as label of the radio button.

The function *radioGroup* returns a value of type GWidget. The documentation for the  function *radioGroup* says:
*Radio group in a 'GWidget' interface (function from 'WidgetConfig' to 'HtmlWidget' )*. 
Hence we have to add a *WidgetConfig* value, that will be consumed by the resulting function of *radioGroup*

The final result is of type *HtmlWidget*. 

File *src/radio01.hs* shows the details

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import qualified Data.Text as T

main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  el "h2" $ text "Radio Buttons from the Contrib Library"
  rec
    rbs :: HtmlWidget t (Maybe Selection) <- 
       radioGroup 
            (constDyn "size") 
            (constDyn [(Small, "small"), (Medium, "Medium"), (Large, "LARGE")])
            WidgetConfig { _widgetConfig_initialValue = Nothing
                         , _widgetConfig_setValue     = never
                         , _widgetConfig_attributes   = constDyn mempty}
    text "Result: "
    display (translate <$> _hwidget_value rbs)
  return ()

-- | A data type for the different choices 
data Selection = Small | Medium | Large
  deriving Eq

-- | Helper function to translate a Selection to an Text value containing a number
translate :: Maybe Selection -> T.Text
translate Nothing = "0"
translate (Just Small) = "10"
translate (Just Medium) = "50"
translate (Just Large) = "800"
~~~

Comments

* If you use the debugger or inspector of your web browser, you will see, that the function *radioGroup* does not pack the radio buttons into a HTML list. It packs them into a HTML table.
* The type annotation in the line ```rbs :: HtmlWidget t (Maybe Selection) <- radioGroup ... ``` is not necessary. I added it, so you can immediately see the type.
* Again we use applicative syntax and a pure Haskell function to transform the *rbs* value to a dynamic text.
* Again translating the Selection to our result string (the *business logic*) is done with a simple pure function!
* Radio buttons from the contrib library are userfriendly: To check, you can either click on the small circle or on the label.

## DropDowns

A DropDown allows you to select items from a predefined list. Normally you only see the selected item. When you click on the little down array, the dropdown widget presents a list of possible values and you can choose one.

DropDowns are defined in the basic reflex-dom library. We don't need the *contrib* library to use them.

DropDowns also have a configuration record that supports the *Default* instance and an element record:

The configuration record:

~~~ { .haskell }
data DropdownConfig t k
   = DropdownConfig { _dropdownConfig_setValue :: Event t k
                    , _dropdownConfig_attributes :: Dynamic t (Map Text Text)
                    }
~~~

The default instance:

~~~ { .haskell }
instance Reflex t => Default (DropdownConfig t k) where
  def = DropdownConfig { _dropdownConfig_setValue = never
                       , _dropdownConfig_attributes = constDyn mempty
                       }
~~~

The element record:

~~~ { .haskell }
data Dropdown t k
    = Dropdown { _dropdown_value :: Dynamic t k
               , _dropdown_change :: Event t k
               }
~~~

The *dropdown* function has the following type:

~~~ { .haskell }
dropdown :: (Ord k, ...) => k
     -> Dynamic t (Map.Map k Text)
     -> DropdownConfig t k
     -> m (Dropdown t k)
~~~

Let's start with the second parameter: It's a map. The keys of type k of this map identify the values. 
The *Text* values will be shown in the dropdown to the user. The values will be presented in the order of the keys.
In the example below, the keys are of type *Int*.

The first parameter is the key of the item, that is initially selected. In the example below it's the *2*. 
Hence the dropdown shows *Switzerland*.

The file *src/dropdown01.hs* has the example:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))
import           Data.Maybe (fromJust)extInpu

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ do
  el "h2" $ text "Dropdown"
  text "Select country "
  dd <- dropdown 2 (constDyn countries) def
  el "p" $ return ()
  let selItem = result <$> value dd 
  dynText selItem 

countries :: Map.Map Int T.Text
countries = Map.fromList [(1, "France"), (2, "Switzerland"), (3, "Germany"), (4, "Italy"), (5, "USA")]

result :: Int -> T.Text
result key = "You selected: " <> fromJust (Map.lookup key countries)
~~~

Let's look at the line ```let selItem = result <$> value dd``` .
In our example the expression *value dd* returns an element of the type *Int*.
If the user chooses "Germany" this expression evaluates to *3*. This is the map-key of the selected item.
To print out the selected item, we use the function *result* to look up this key in our map.

If you use the function *dropdown* with a first parameter that is missing as key in the map of the second parameter,
reflex will add a *(key,value)* pair with this missing key and an empty text string. Hence the use of *Map.lookup* 
is not dangerous!

## Ranges

Ranges allow the user to select a value from a range of values.

Ranges again have a configuration and an element record.
The configuration record is an instance of the *Default* type class.

The configuration record:

~~~ { .haskell }
data RangeInputConfig t
   = RangeInputConfig { _rangeInputConfig_initialValue :: Float
                      , _rangeInputConfig_setValue :: Event t Float
                      , _rangeInputConfig_attributes :: Dynamic t (Map Text Text)
                      }
~~~

The *Default* instance:

~~~ { .haskell }
instance Reflex t => Default (RangeInputConfig t) where
  def = RangeInputConfig { _rangeInputConfig_initialValue = 0
                        , _rangeInputConfig_setValue = never
                        , _rangeInputConfig_attributes = constDyn mempty
                        }
~~~

The element record:

~~~ { .haskell }
data RangeInput t
   = RangeInput { _rangeInput_value :: Dynamic t Float
                , _rangeInput_input :: Event t Float
                , _rangeInput_mouseup :: Event t (Int, Int)
                , _rangeInput_hasFocus :: Dynamic t Bool
                , _rangeInput_element :: HTMLInputElement
                }
~~~

The example in *src/range01* uses default values for everything:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import qualified Data.Text as T

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = do
    el "h2" $ text "Range Input"
    rg <- rangeInput def
    el "p" blank
    display $ _rangeInput_value rg
    return ()
~~~

As you can see, the default range goes from 0 to 100.

The next example in *src/range02.hs* allows the user to select numbers in the range of -100 to +100. 
We have to set the minimum attribute to -100.

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import qualified Data.Text as T

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = do
    el "h2" $ text "Range Input"
    rg <- rangeInput $ def & attributes .~ constDyn ("min" =: "-100")
    el "p" blank
    display $ _rangeInput_value rg
    return ()
~~~

*RangeInput* does not support the *HasValue* type class. Here we cannot use the *value* function!

The next example from *src/range03.hs* allows only numbers from -100 to +100 in steps from 10.
We add ticks above the values of -30, 0 and 50:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

import Data.Map
import qualified Data.Text as T
import Data.Monoid ((<>))

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = do
    el "h2" $ text "Range Input"
    rg <- rangeInput $ def & attributes .~ constDyn 
        ("min" =: "-100" <> "max" =: "100" <> "value" =: "0" <> "step" =: "10" <> "list" =: "powers" )
    elAttr "datalist" ("id" =: "powers") $ do
       elAttr "option" ("value" =: "0") blank
       elAttr "option" ("value" =: "-30") blank
       elAttr "option" ("value" =: "50") blank
    el "p" blank
    display $ _rangeInput_value rg
    return ()
~~~

It generates the following HTML:

~~~ { .html }
<input type="range" min="-100" max="100" value="0" step="10" name="power" list="powers">
<datalist id="powers">
  <option value="0">
  <option value="-30">
  <option value="+50">
</datalist>
~~~

# Defining your own Elements with Events

## The Function family *el'*, *elAttr'*, *elClass'*, *elDynAttr'*, *elDynClass'* 

In this section of the tutorial, we will look at the second function family to create DOM elements.
The names of these functions all end with a prime ('). They take the same parameters and work similar as the functions without the prime 
in the name, but they give you more power!

As an example we look at the difference between *el* and *el'*. Similar facts hold for the other functions.

The unprimed version *el* creates a DOM element, but does not give access to the created element:

~~~ { .haskell }
el :: DomBuilder t m => Text -> m a -> m a
~~~

With the primed function *el'* we get access to the created DOM element:

~~~ { .haskell }
el' :: DomBuilder t m => Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
~~~

Again the type of *el'* is a little bit scary!

Without going into details, we see that the primed function *el'* takes the same arguments as the unprimed function *el*. 

We can simplify the type of *el'* to

el' :: (...) => Text -> m a -> m (*Element*, a)


It returns a tuple *(Element, a)*:
The first value of this tuple is the DOM element, the second value is (ignoring monadic wrapping) the second parameter of the *el'* function.

With access to the DOM element, we are now able to add events to our widgets. The next section shows the details. 

## The function *domEvent*

When we use the unprimed version of *el* to define a button

```el "button" $ text "Click me"```

we get a button, but when clicked, it will not create any events. 

With the return value of the primed version(*el'*), and with the help of the function *domEvent* we are now able to add events to DOM elements:

~~~ { .haskell }
button :: DomBuilder t m => Text -> m (Event t ())
button t = do
  (e, _) <- el' "button" $ text t
  return $ domEvent Click e
~~~

Please note, that reflex-dom uses a slightly different definition. 

The function *domEvent* takes an event name as a first parameter and an element as a second parameter, and returns an event of a variable type.

Reflex-dom uses some advanced type hackery like TypeFamilies to create events of variable types depending of the  event name. 

* ```domEvent Click e``` returns an event of type *()*
* ```domEvent Mousedown e``` returns an event of type *(Int,Int)* with the mouse coordinates.

This is defined in the module [Reflex.Dom.Builder.Class.Events](https://github.com/reflex-frp/reflex-dom/blob/develop/src/Reflex/Dom/Builder/Class/Events.hs):

* The data type *EventName* lists the possible event names.
* The type family *EventResultType* defines the type of the resulting event.

## Example: *Disable / enable a Button*

In a lot of web shops you must check a checkbox to accept the business conditions like *"low quality at high prices"*.
If you don't accept the conditions, the *order* button is disabled and you cannot order!
We are now able to define the checkbox, the button and the logic to enable or disable the button.

File *src/button01.hs* contains the code:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Reflex.Dom
import qualified Data.Text as T
import           Data.Monoid

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ do
  el "h2" $ text "Button enabled / disabled"
  cb <- el "label" $ do
    cb1 <- checkbox True def
    text "Enable or Disable the button"
    return cb1
  el "p" blank
  counter :: Dynamic t Int <- count =<< disaButton (_checkbox_value cb) "Click me"
  el "p" blank
  display counter

-- | A button that can be enabled and disabled
disaButton :: MonadWidget t m
            => Dynamic t Bool -- ^ enable or disable button
            -> T.Text         -- ^ Label
            -> m (Event t ())
disaButton enabled label = do
    let attrs = ffor enabled $ \e -> monoidGuard (not e) $ "disabled" =: "disabled"
    (btn, _) <- elDynAttr' "button" attrs $ text label
    pure $ domEvent Click btn

-- | A little helper function for data types in the *Monoid* type class: 
-- If the boolean is True, return the first parameter, else return the null or empty element of the monoid
monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty
~~~

The function *disaButton* contains the main logic. It takes a Dynamic Bool, which indicates whether 
the button should be enabled or disabled. 

*ffor* is like *fmap* but with flipped parameters: ```ffor :: Functor f => f a -> (a -> b) -> f b```

## Radio Buttons Revisited

We are now able to write our own function to create radio buttons. File *src/radio02.hs* has the code:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement =  el "div" $ do
  rec
    el "h2" $ text "Own Radio buttons"
    let group = "g"
    let dynAttrs = styleMap <$> dynColor
    evRad1 <- radioBtn "orange" group Orange dynAttrs
    evRad2 <- radioBtn "green" group Green dynAttrs
    evRad3 <- radioBtn "red" group Red dynAttrs
    let evRadio = (T.pack . show) <$> leftmost [evRad1, evRad2, evRad3]
    dynColor <- holdDyn "lightgrey" evRadio
  return ()

data Color = White | Red | Orange | Green
  deriving (Eq, Ord, Show)

-- | Helper function to create a radio button
radioBtn :: (Eq a, Show a, MonadWidget t m) => T.Text -> T.Text -> a -> Dynamic t (Map.Map T.Text T.Text)-> m (Event t a)
radioBtn label group rid dynAttrs = do
    el "br" blank 
    ev <- elDynAttr "label" dynAttrs $ do
        (rb1, _) <- elAttr' "input" ("name" =: group <> "type" =: "radio" <> "value" =: T.pack (show rid))  blank
        text label
        return $ domEvent Click rb1
    return $ rid <$ ev

styleMap :: T.Text -> Map.Map T.Text T.Text
styleMap c = "style" =: ("background-color: " <> c)
~~~

Comments:

* The function *radioBtn* contains the logic to create a radio button. 
* Like the previous radio button example with the function *radioGroup* from the *contrib* library, it uses an user defined datatype as payload for the click event.
* Maybe you want to add an additonal Boolean parameter to specify whether a radio button is initially checked.
* Similar to the checkbox example, these radio buttons are userfriendly. You can click on the circle or on the label.
* Depending on the checked radio button, the background color of the whole radio button changes.
* Again we use event transformation.
* We use the reflex function *holdDyn* to create a Dynamic value. I describe it just below:

### The Function *holdDyn*

The function *holdDyn* has the following type: 

```holdDyn :: (...) => a -> Event t a -> m (Dynamic t a)```

It converts an Event with a payload of type *a* into a Dynamic with the same value. 
We have to specify a default value, to be used before the first event occurs.

# Timers

A timer will send you always an event after a predefined amount of time has expired. Reflex-dom has two timer functions *tickLossy* and *tickLossyFrom*. The function *tickLossyFrom* is used only in applications where you need several parallel timers. Normally you will use *tickLossy*. It will start sending events immediately after the startup of your application. It has the following type

```tickLossy :: (...) => NominalDiffTime -> UTCTime -> m (Event t TickInfo)```

The types *NominalDiffTime* and *UTCTime* are defined in the basic GHC library *time*. To use them, we need to import Data.Time.

The first parameter *NominalDiffTime* is the length of the time interval between two events. It is measured in seconds. The second parameter is an UTCTime. I never really found out what it's used for.
You can give an arbitrary data-time field. Normally I use current time.

The result is a series of Events. Their paylod is the data structure *TickInfo*:

~~~ { .haskell }
data TickInfo
  = TickInfo { _tickInfo_lastUTC :: UTCTime
             -- ^ UTC time immediately after the last tick.
             , _tickInfo_n :: Integer
             -- ^ Number of time periods since t0
             , _tickInfo_alreadyElapsed :: NominalDiffTime
             -- ^ Amount of time already elapsed in the current tick period.
             }
~~~ 

Both functions *tickLossy* and *tickLossyFrom* have the term *lossy* in their name:
If the system starts running behind, occurrences of events will be dropped rather than buffered.

A simple example is in the file *src/timer01.hs*:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import           Data.Time
import           Control.Monad.Trans (liftIO)
import qualified Data.Text as T

main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m =>  m()
bodyElement = do
  el "h2" $ text "A Simple Clock"
  now <- liftIO getCurrentTime
  evTick <- tickLossy 1 now
  let evTime = (T.pack . show . _tickInfo_lastUTC) <$>  evTick
  dynText =<< holdDyn "No ticks yet" evTime
~~~