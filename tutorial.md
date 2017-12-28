# A Beginner-friendly Step by Step Tutorial for Reflex-Dom

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

In this tutorial we use *reflex* and *reflex-dom*. Reflex is a FRP implementation written by Ryan Trinkle from [Obsidian](https://obsidian.systems/).
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

If you used the *reflex-platform* to install reflex and reflex-dom, you will have the newer versions. 
 

## Popular Language Extensions

To write Reflex programs, very often we use some of the following GHC Haskell language extensions:

```{-# LANGUAGE OverloadedStrings #-}```

We need it, because *Reflex* uses *Text* instead of *String*. The extension *OverloadedStrings*  allows automatic conversion of string constants like "I'm a String"
to the correct string type. We don't need to pack and unpack the string constants ourselfs.

```{-# LANGUAGE RecursiveDo #-}```

Sometimes we need to access a value or an event from a DOM element before it is defined. *RecursiveDo* makes this easy, by extending the `do`-notation syntax sugar.

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

Rarely we will use other libraries, eg

* import Data.FileEmbed  - library *file-embed* on Hackage and installed by reflex-platform.
* import Data.Text.Encoding - will be installed with reflex-dom
* import Data.Maybe - part of base
* import Data.Time - part of base
* import Reflex.Dom.Contrib.Widgets - from [https://github.com/reflex-frp/reflex-dom-contrib](https://github.com/reflex-frp/reflex-dom-contrib)
* import Control.Monad.Trans - part of base
* import Data.Meteo.Swiss - from [https://github.com/hansroland/opench/tree/master/meteo](https://github.com/hansroland/opench/tree/master/meteo)


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

## Running the Program in the Browser

If you installed *reflex-platform* do the following to run the program *src/count01.hs* in the browser:

* Navigate into your *reflex-platform* directory.
* Start the nix-shell by typing ``` ./try-reflex ``` (The first time this may take some time...)
* In the nix-shell, navigate into your *reflex-dom-inbits* directory. You can use normal linux *cd* commands.
* Compile the program with ``` ghcjs src/count01.hs```
* Open the resulting *src/count01.jsexe/index.html* file with your browser. eg ``` chromium src/count01.jsexe/index.html```

If you installed reflex with *stack*, use the command ```stack exec ghcjs src/count01.hs``` and then
open the resulting *src/count01.jsexe/index.html* file with your browser as described above.

Unfortunately interactive ghcjs does not yet work, if the ghcjs compiler was compiled with GHC 8.0.

## Running the Program in WebkitGtk

If you have installed *reflex-platform* do the following to run the program *src/count01.hs* as a native programme:

* Navigate into your *reflex-platform* directory.
* Start the nix-shell by typing ``` ./try-reflex ``` (The first time this may take some time...)
* In the nix-shell, navigate into your *reflex-dom-inbits* directory. You can use normal linux *cd* commands.
* Run the program with ``` runghc src/count01.hs```


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
The second parameter is either the text of the element or a valid child element.

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

The module Reflex.Dom defines a little helper function (=:) to create a singelton Map.

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

All the above functions allow us to define DOM elements with static attributes. 
But you cannot change the attributes while the program is running!

With the function *elDynAttr*, as the name says, you can specify dynamic attributes, 
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
directory during compile time. I assume that your working directory is *reflex-dom-inbits*.
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
* If you compile your program with *ghcjs* and run it with *chromium src/<program>.jsexe/index.html*, your working 
directory is *src/<program>.jsexe*. You have to manually copy the directory from *reflex-dom-inbits/css* to *src/<program>.jsexe*.
* If you change your css files, the changes become active after a reload of the *index.html* page in the browser.
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
* We need *recursive do": We refer to *evIncr* before it is defined.

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

Using function application as a fold function over a current value is very powerful!! We'll see more examples.

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

### The Type Class Default

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

### The Function *never*

``` never :: Event t a```

It's an event, that never occurs.


### The Function *constDyn*

Note the type of the _textInputConfig_attributes: It's ```Dynamic t (Map Text Text)```.
To create a Dynamic map we can use the function *constDyn*: 
It takes an value of type ```a``` and returns a value of type ```Dynamic t a```. 
Of course, a Dynamic created with *constDyn* will not change while our program is running.


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

## Reading out the Value of a TextInput Widget on an Event

In all the above examples we used the contents of the TextInput field immediately when it changed.
Sometimes you want to use this contents when the user clicks a button.

### Function *tagPromptlyDyn*

```tagPromptlyDyn :: Reflex t => Dynamic t a -> Event t b -> Event t a```

When the event in the second function parameter occurs, the function returns a new event with the payload of the Dynamic value in the first function parameter. 
The function *tagPromptlyDyn* *lifts* the Dynamic value onto the Event.

### Function *holdDyn*

```holdDyn :: (...) => a -> Event t a -> m (Dynamic t a)```

It converts an Event with a payload of type *a* into a Dynamic with the same value. 
We have to specify a default value, to be used before the first event occurs.

Look at the last two lines in the file *src/textinput03.hs*:

* *evText* is the event that carries the contents of the TextInput as payload
* With the function *holdDyn* we create a Dynamic. Its value changes on each click on the button.


~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  el "h2" $ text "Text Input - Read Value on Button Click"
  ti <- textInput def
  evClick <- button "Click Me"
  el "br" blank
  text "Contents of TextInput on last click: "
  let evText = tagPromptlyDyn (value ti) evClick
  dynText =<< holdDyn "" evText
~~~ 

## Reading out the Value of a TextInput Widget on Pressing the *Return* Key

Sometimes you want to use the text in the TextInput widget when the user presses the *Return/Enter* key
inside the widget. 

### Function *keypress*

```keypress :: (...) => Key -> e -> Event t ()```

Instead of the button, we use this function to create the event that triggers reading the TextInput value:

The code in *src/textinput04.hs* is similar to the example above:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  el "h2" $ text "Text Input - Read Value on 'Enter'"
  ti <- textInput def
  el "br" blank
  text "Contents of TextInput after 'Enter': "
  let evEnter = keypress Enter ti
  let evText = tagPromptlyDyn (value ti) evEnter
  dynText =<< holdDyn "" evText
~~~

## Setting the Contents of a TextInput Widget

Sometimes you want to set the text in the TextInput widget with the value of an Dynamic t T.Text.
Remember, reflex-dom is Haskell and we cannot set the value of a widget *somewhere* in the code.
We have to specify this, when we define the widget in our code.

Using the syntactic sugar of the lens library is the easiest way to set the value of a TextInput widget:

```ti <- textInput $ def & setValue .~ evText```

Here the value *evText* has the type ```Event t T.Text```. The text payload of this event will be written
into the TextInput widget. That's it! 

If you are not familiar with lenses, just take this as a syntax construct.

File *src/textinput05.hs* has the example:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom 
import qualified Data.Text as T

main :: IO ()
main = mainWidget body
         
body :: MonadWidget t m => m ()
body = do
  el "h1" $ text "Write into TextInput Widget"
  t1 <- textInput def
  evCopy <- button ">>>"
  let evText = tagPromptlyDyn (value t1) evCopy
  t2 <- textInput $ def & setValue .~ evText
  return ()
~~~

We define two TextInput widgets and a button in between. Clicking the button
generates the event *evCopy* with a unit *()* as payload. 
Then we use the function *tagPromptlyDyn* to create a new event *evText*, with
the value of the first textbox as payload. In the definition of the second TextInput widget we 
use this event to set the text of the second textbox.

Sometimes you want to have a *Reset* button to clear TextInput widgets. This is now easy: 
We use event transformation to create an event with an empty text. The code is in *src/textinput06.hs*:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import           Reflex.Dom 
import qualified Data.Text as T

main :: IO ()
main = mainWidget body
         
body :: MonadWidget t m => m ()
body = do
  rec el "h1" $ text "Clear TextInput Widget"
      ti <- textInput $ def & setValue .~ ("" <$ evReset)
      evReset <- button "Reset"
  return ()
~~~

We use *recursive do* because we define the Reset button after the TextInput widget.


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
* The interesting thing happens in the line ```styleMap <$> value dfsRed <*> value dfsGreen <*> value dfsBlue```. 
We again use applicative syntax to call the function *styleMap* with the current values of our 3 input fields.
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
change it's state. This is not user friendly!

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

Depending on your version of *RangeInput*, it does not support the *HasValue* type class. The  *HasValue* class was added end of March 2017. 
If your version is older, you cannot use the *value* function. However in all versions you can use the *_rangeInput_value* function!

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

This is defined in the module [Reflex.Dom.Builder.Class.Events](https://github.com/reflex-frp/reflex-dom/blob/develop/reflex-dom-core/src/Reflex/Dom/Builder/Class/Events.hs):

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
* We use the reflex function *holdDyn* to convert an Event to a Dynamic value.

# Debugging

## Tracing Events

In a reflex-dom program a lot of code works with events. 
In big programs you may get lost. To help you out, there are two reflex
functions *traceEvent* and *traceEventWith*. They allow you to trace events. 

The function *traceEvent* has the following type:

```traceEvent :: (Reflex t, Show a) => String -> Event t a -> Event t a```

It takes a String (not a *Text*!) and an Event with a payload of type *a* and returns a new
unmodified Event with the same payload. 
The type *a* must have a *Show* instance.
When the event occurs, it creates a trace entry with the string and the payload of the event using the *show* function.

If the type of your payload is not an instance of *Show* or the string produced by the *show* function 
is far to long you can use the *traceEventWith* function. It has the type:

```traceEventWith :: Reflex t => (a -> String) -> Event t a -> Event t a```

Instead of using the *Show* instance, you provide a custom function to get a string representation
of the payload.

In the reflex source code both functions have the following comment:

```
-- Note: As with Debug.Trace.trace, the message will only be printed if the
-- 'Event' is actually used.
```
It's not specified explicitely, but you have to use / consume the **new** event returned by the *traceEvent*/*traceEventWith* function. *Use* or *consume* the event means, that this event or a
transformed child event must finally have some effect in the DOM.

Let's add tracing to the above radio button example. Here I show only the *bodyElement* function, 
the rest is unmodified. The full code is in *src/trace01.hs*.

~~~ { .haskell }
bodyElement =  el "div" $ do
  rec
    el "h2" $ text "Some Tracing"
    let group = "g"
    let dynAttrs = styleMap <$> dynColor
    evRad1 <- radioBtn "orange" group Orange dynAttrs
    evRad2 <- radioBtn "green" group Green dynAttrs
    evRad3 <- radioBtn "red" group Red dynAttrs
    let evRadio = (T.pack . show) <$> leftmost [evRad1, evRad2, evRad3]

    -- added line:
    let evRadioT = traceEvent ("Clicked rb in group " <> T.unpack group) evRadio

    -- modified line: evRadioT instead of evRadio 
    dynColor <- holdDyn "lightgrey" evRadioT

  return ()
~~~

Here is an example of my test output (from WebkitGtk):

```
Clicked radio button in group g: "Orange"
Clicked radio button in group g: "Green"
Clicked radio button in group g: "Red"
```

* If you use a browser, you can see the trace output in your browser-console.
* If you use WebkitGtk, the trace output is routed to *stderr*.
* Look at the modified line staring with ```dynColor```:  If you change back to the event *evRadio*
tracing will no longer work. You **have** to use *evRadio**T***  returned by the *traceEvent* function!!

## Tracing Changes of Dynamics

Similar to Events you can also trace Dynamics. 
The names and types of the functions are:

```traceDyn :: (Reflex t, Show a) => String -> Dynamic t a -> Dynamic t a```

```traceDynWith :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a```

They work similar to the *traceEvent* and *traceEventWith* functions. Everytime the Dynamic value
changes a trace entry will be created. Also the new created Dynamic value must finally be used for an effect in the DOM.

# Timers

A timer will send you always an event after a predefined amount of time has expired. Reflex-dom has two timer functions *tickLossy* and *tickLossyFrom*. The function *tickLossyFrom* is used only in applications where you need several parallel timers. Normally you will use *tickLossy*. It will start sending events immediately after the startup of your application. It has the following type

```tickLossy :: (...) => NominalDiffTime -> UTCTime -> m (Event t TickInfo)```

The types *NominalDiffTime* and *UTCTime* are defined in the basic GHC library *time*. To use them, we need to import Data.Time.

The first parameter *NominalDiffTime* is the length of the time interval between two events. It is measured in seconds. The second parameter is an UTCTime. I never really found out what it's used for.
You can give an arbitrary data-time field. Normally I use current time.

The result is a series of Events. Their payload is the data structure *TickInfo*:

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

# Server Requests

In the next examples we will send requests to a Web server and process the responses in reflex-dom. 

Note: **The following examples run only in the browser, but not in WebkitGtk.** There are some security issues with the *same-origin security policy* and with *cross-origin resource sharing* (CORS). Please inform me, if you know how to overcome this issue.

## Functions and Data Structures to Send Requests and Receive Responses

### Requests

To send a request we need a data structure called XhrRequest

~~~ { .haskell }
data XhrRequest a
   = XhrRequest { _xhrRequest_method :: Text
                , _xhrRequest_url :: Text
                , _xhrRequest_config :: XhrRequestConfig a
                }
   deriving (Show, Read, Eq, Ord, Typeable, Functor)
~~~

Again this needs a configuration record:

~~~ { .haskell }
data XhrRequestConfig a
   = XhrRequestConfig { _xhrRequestConfig_headers :: Map Text Text
                      , _xhrRequestConfig_user :: Maybe Text
                      , _xhrRequestConfig_password :: Maybe Text
                      , _xhrRequestConfig_responseType :: Maybe XhrResponseType
                      , _xhrRequestConfig_sendData :: a
                      , _xhrRequestConfig_withCredentials :: Bool
                      , _xhrRequestConfig_responseHeaders :: XhrResponseHeaders
                      }
   deriving (Show, Read, Eq, Ord, Typeable, Functor)
~~~

and again this configuration record is an instance of the type class *Default*

~~~ { .haskell }
instance a ~ () => Default (XhrRequestConfig a) where
  def = XhrRequestConfig { _xhrRequestConfig_headers = Map.empty
                         , _xhrRequestConfig_user = Nothing
                         , _xhrRequestConfig_password  = Nothing
                         , _xhrRequestConfig_responseType  = Nothing
                         , _xhrRequestConfig_sendData  = ()
                         , _xhrRequestConfig_withCredentials = False
                         , _xhrRequestConfig_responseHeaders = def
                         }
~~~

### Functions *performRequestAsync* and *performRequestAsyncWithError*

To send a request to the server, we use the function

```performRequestAsync :: (...) => Event t (XhrRequest a) -> m (Event t XhrResponse)```

So we have to pack a XhrRequest into an Event. Of course we'll again use event transformation to accomplish this.
Sending the request does not block our reflex-dom frontend. When the response arrives from the server, we just get an 'Event t XhrRespone'

The data type XhrResponse is defined as:

~~~ { .haskell }
data XhrResponse
   = XhrResponse { _xhrResponse_status :: Word
                 , _xhrResponse_statusText :: Text
                 , _xhrResponse_response :: Maybe XhrResponseBody
                 , _xhrResponse_responseText :: Maybe Text
                 , _xhrResponse_headers :: Map Text Text
                 }
   deriving (Typeable)
~~~

The type *XhrResponseBody* is defined as:

~~~ { .haskell }
data XhrResponseBody = XhrResponseBody_Default Text
                     | XhrResponseBody_Text Text
                     | XhrResponseBody_Blob Blob
                     | XhrResponseBody_ArrayBuffer ByteString
    deriving (Eq)
~~~

If you want to write rock solid software, you should use the function *performRequestAsyncWithError* instead of *performRequestAsync*.
It has the following type: 

```performRequestAsyncWithError :: (...) => Event t (XhrRequest a) -> m (Event t (Either XhrException XhrResponse))```

In case of error you get back a XhrException value. It's defined like:

~~~ { .haskell }
data XhrException = XhrException_Error
                  | XhrException_Aborted
     deriving (Show, Read, Eq, Ord, Typeable)
~~~

To keep my examples small, I'll only use *performRequestAsync*.


### Function *getPostBuild*

This is a little helper function we use in the next example. It has the type: 

```getPostBuild :: PostBuild t m => m (Event t ())```

It generates a single event at the time the HTML page has been created. It's similar to the HTML *onload*.


# Swiss Meteo Data Example 1

As an example server we'll use a Web service, that returns the measurement data of the last 10 minutes of automatic meteo stations. This Web service has some advantages:

* You can just use this service. It's not necessary to register with your e-mail address.
* It's very simple: There are only 2 different requests and responses.
* It returns the data in JSON format.

For the first example we use a dropdown with a fixed list of stations:

* Bern the captal: [http://www.bern.com/en](http://www.bern.com/en)
* Zurich, the main city: [https://www.zuerich.com/en](https://www.zuerich.com/en)
* Jungfraujoch, called *Top of Europe* with 3454 meters above sea level really high in the Swiss mountains [https://www.jungfrau.ch/en-gb/](https://www.jungfrau.ch/en-gb/)
* Zermatt, the world famous mountain resort at the bottom of the Matterhorn: [http://www.zermatt.ch/en](http://www.zermatt.ch/en)
* Binn, a small and very lovely mountain village, far off mainstream tourism: [http://www.landschaftspark-binntal.ch/en/meta/fotogalerie.php](http://www.landschaftspark-binntal.ch/en/meta/fotogalerie.php)

Every meteo station has a 3-letter code eg "BER" for Bern. We send this 3-letter code to the server and it returns a JSON string wih the measured data. To start, we just show this JSON string as it is, without any nice formatting.

The code is in the file *src/xhr01.hs*:

~~~ { .haskell }
{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))

main :: IO ()
main = mainWidget body

body :: MonadWidget t m => m ()
body  = el "div" $ do
  el "h2" $ text "Swiss Meteo Data (raw version)"
  text "Choose station: "
  dd <- dropdown "BER" (constDyn stations) def
  -- Build and send the request
  evStart <- getPostBuild
  let evCode = tagPromptlyDyn (value dd) $ leftmost [ () <$ _dropdown_change dd, evStart]
  evRsp <- performRequestAsync $ buildReq <$> evCode
  -- Display the whole response
  el "h5" $ text "Response Text:"
  let evResult = (fromMaybe "" . _xhrResponse_responseText) <$> evRsp
  dynText =<< holdDyn "" evResult
  return ()

buildReq :: T.Text -> XhrRequest ()
buildReq code = XhrRequest "GET" ("https://opendata.netcetera.com/smn/smn/" <> code) def

stations :: Map.Map T.Text T.Text
stations = Map.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch")]
~~~

Comments:

* We use *tagPromptlyDyn* to lift the 3-letter code from the dropdown onto an event.
* Then we use this event to create a new event with the XhrRequest.
* With the function *performRequestAsync* we send this XhrRequest to the server. 
* We send a request immediately after startup and every time the value of the dropdown changes.
* At the time of this writing, the station *Binn* is not operational. The server returns a code of 204 in the *_xhrResponse_status* field.

# Swiss Meteo Data Example 2

Now we want not just display the raw JSON string. We want to display the meteo data (more or less) nicely.
We will use a tabbed display to show the returned data. If the station has no data, we will show an error page.
I'll not reproduce the whole code here. You can find it in the file *src/xhr02.hs*.

## Handling the Error Case with Event Filtering

From the server we get back responses with an HTTP return code. If this respone code is 200, the server
returned real meteo data. If the response code has an other value, something strange happend and we issue
an error message. We need to separate these 2 types of events. Event filtering is the way to do it.

There are 2 functions:

``` ffilter   :: (a -> Bool) ->  Event t a -> Event t a ```

``` fforMaybe :: Event t a -> (a -> Maybe b) -> Event t b ```

The function *ffilter* selects only those events that fullfill the predicate *(a -> Bool)*. Other events are discarded.
The function *fforMaybe* selects only those events, where the function *(a -> Maybe b)* returns a *Just b* value,
and does a corresponding event transformation from *a* to *b*.

Separating the good and the bad events is now easy:

~~~ { .haskell }
let (evOk, evErr) = checkXhrRsp evRsp
~~~ 

where

~~~ { .haskell }
-- | Split up good and bad response events
checkXhrRsp :: FunctorMaybe f => f XhrResponse -> (f XhrResponse, f XhrResponse)
checkXhrRsp evRsp = (evOk, evErr)
  where
    evOk = ffilter (\rsp -> _xhrResponse_status rsp == 200) evRsp
    evErr = ffilter (\rsp -> _xhrResponse_status rsp /= 200) evRsp
~~~

Remarks:

* Ok, in a professional environnment you would replace the hardcoded number *200* with a nice constant.
* I asked *ghci* to tell me the type signature of the function *checkXhrRsp*.
* *Event Filtering* is one of the powerful methods used in reflex-dom programs.

## Hiding unwanted HTML elements

If the server sent a nice response with a status code of 200 we don't want to show the error page.
If we receive a response with a bad status code, means different from 200, we don't want to show the data page.
In the *Dynamic* value *dynPage* we store the page we want to display.
For this we have our data type *Page* with an enumeration of our pages.
Again we use *foldDyn* with function application. The last event sets the value of *dynPage*:

~~~ { .haskell }
dynPage <- foldDyn ($) PageData $ leftmost [const PageData <$ evOk, const PageError <$ evErr]
~~~

For each of our pages we write an own function to display the data. As parameters we use
the corresponding event and the value *dynPage*:

~~~ { .haskell }
  pageData evOk dynPage
  pageErr evErr dynPage
~~~

Inside the page functions we use the function *visible* to create a dynamic attribute map,
which contains a visible or hide attribute. The example shows the code for the error page:

~~~ { .haskell }
-- | Display the error page
pageErr :: MonadWidget t m => Event t XhrResponse -> Dynamic t Page -> m ()
pageErr evErr dynPage = do
  let dynAttr = visible <$> dynPage <*> pure PageError
  elDynAttr "div" dynAttr $ do 
     el "h3" $ text "Error"
~~~ 

where

~~~ { .haskell }
-- | Helper function to create a dynamic attribute map for the visibility of an element
visible :: Eq p => p -> p -> Map.Map T.Text T.Text
visible p1 p2 = "style" =: ("display: " <> choose (p1 == p2) "inline" "none")
  where 
    choose True  t _ = t
    choose False _ f = f
~~~ 

## Function *decodeXhrResponse*: Parsing the JSON String

If we receive a response with a status code of 200, we need to parse the JSON string.
For this we use the popular *aeson* library. We need a data structure that corresponds to the JSON string. 

The library *opench-meteo* contains Haskell definitons for the Swiss meteo data and the instances of the 
*FromJSON* type class. You can find this library on [Hackage](http://hackage.haskell.org/package/opench-meteo)
or on [https://github.com/hansroland/opench/tree/master/meteo](https://github.com/hansroland/opench/tree/master/meteo).
You must use version 0.2.0.0 or higher. Version 0.1.* used _http_, but the now server expects a _https_ request.

We need to import this library: ```import Data.Meteo.Swiss```.

The function *decodeXhrResponse* converts the response returned by the *performRequestAsync* function to your data type. It has the type:

```decodeXhrResponse :: FromJSON a =>  XhrResponse -> Maybe a```

However sometimes you have to give a little bit help to the compiler. The compiler must know the target data type for the 
*decodeXhrResponse* function. In our case we want back a *Event t SmnRecord*. 
Therefore I tried to add the type annotation in a let expression:

```let evSmnRec :: (Event t SmnRecord) = fmapMaybe decodeXhrResponse evRsp```

However, I couldn't find a syntax, that was accepted by the compiler. 

So with *return* I wrap the *fmapMaybe decodeXhrResponse evRsp* into the monad and can use the syntax of the 
*ScopedTypeVariables* language extension:

```evSmnRec :: (Event t SmnRecord) <- return $  fmapMaybe decodeXhrResponse evRsp```

Note: In our situation the scoped type varaible is not really necessary, because we nicely type annotated all functions.
However in a lot of situations you may have to give help to the compiler.

The function *decodeXhrResponse* returns a *Maybe (Event t SmnRecord)*. 
Again we are sloppy and ignore error handling.
Therefore we use *fmapMaybe*. 

## Function *tabDisplay*: Display the data on a tabbed page

Now we need to present the data to the user. We have 2 types of data:

* The Weather Data *SmnRecord*
* The Station Record *SmnStation*

An event with the SmnRecord we get back from the decodeXhrResponse. An event with the SmnStation record we create with normal event transformation.

```let evSmnStat = fmapMaybe smnStation evSmnRec``` 

For each of these data records we will have an own tab on the page. For this we use the function *tabDisplay*.
It has the following type:

```tabDisplay :: (...) => T.Text -> T.Text -> Map.Map k (T.Text, m ()) -> m ()```

Instead of real tabs it creates a ``` <ul> / <li> ``` HTML list. We then use CSS to transform this list into nice tab headers.

The first *Text* parameter is the CSS class applied to the ```<ul>``` HTML element.   
The second *Text* parameter is the CSS class applied to the currently active ```<li>``` element. 
The third parameter is a Map from a key *k* to a pair *(T.Text, m ()) -> m ())*. The first component *Text* is the label for the tab header and the function *m ()) -> m ()* in the second component is the function to create the tab data.
The data type for the key *k* must be an instance of the Haskell type class *Ord*. The tabs  will be ordered by the values of *k*.

We need some CSS, so we add ```{-# LANGUAGE TemplateHaskell #-}``` to our list of GHC language exensions and we add
```import Data.FileEmbed``` to our import list and use *mainWidgetWithCss* as our main function.

To create the tab display we now use: 

~~~ { .haskell }
tabDisplay "tab" "tabact" $ tabMap evSmnRec evSmnStat 

-- | Create a tabbed display
tabMap :: MonadWidget t m => Event t SmnRecord -> Event t SmnStation -> Map.Map Int (T.Text, m ())
tabMap evMeteo evStat = Map.fromList[ (1, ("Station", tabStat evStat)),
            (2, ("MeteoData", tabMeteo evMeteo))]
~~~

We need two functions *tabStat* and *tabMeteo* to display the data:

~~~ { .haskell }
-- | Create the DOM elements for the Station tab
tabStat :: MonadWidget t m => Event t SmnStation -> m ()
tabStat evStat = do 
  dispStatField "Code" staCode evStat
  dispStatField "Name" staName evStat
  dispStatField "Y-Coord" (tShow . staCh1903Y) evStat
  dispStatField "X-Coord" (tShow . staCh1903X) evStat
  dispStatField "Elevation" (tShow . staElevation) evStat
  return ()

-- | Create the DOM elements for the Meteo data tab
tabMeteo :: MonadWidget t m => Event t SmnRecord -> m ()
tabMeteo evMeteo = do 
  dispMeteoField "Date/Time" (tShow . smnDateTime) evMeteo
  dispMeteoField "Temperature" smnTemperature evMeteo
  dispMeteoField "Sunnshine" smnSunshine evMeteo
  dispMeteoField "Precipitation" smnPrecipitation evMeteo
  dispMeteoField "Wind Direction" smnWindDirection evMeteo
  dispMeteoField "Wind Speed" smnWindSpeed evMeteo
  return ()
~~~

And again two functions  *dispStatField* and *dispStatField* to display the single fields.
We also need a little helper function to nicely display non text values wrapped in a Maybe.

~~~ { .haskell }
-- Display a single field from the SmnStation record
dispStatField :: MonadWidget t m => T.Text -> (SmnStation -> T.Text) -> Event t SmnStation -> m ()
dispStatField label rend evStat = do
  el "br" blank
  text $ label <> ": "
  dynText =<< holdDyn "" (fmap rend evStat)
  return ()

-- Display a single field from the SmnRecord record
dispMeteoField :: MonadWidget t m => T.Text -> (SmnRecord -> T.Text) -> Event t SmnRecord -> m ()
dispMeteoField label rend evRec = do
  el "br"blank
  text $ label <> ": "
  dynText =<< holdDyn "" (fmap rend evRec)
  return ()

-- | Small helper function to convert showable values wrapped in Maybe to T.Text. 
-- You should use the text-show library from Hackage!! 
tShow :: Show a => Maybe a -> T.Text
tShow Nothing = ""
tShow (Just x) = (T.pack . show) x
~~~

Note: With the function *tabDisplay* the number of tabs is fixed. You cannot change it during run time.

# Swiss Meteo Data Example 3

The code is in file *src/xhr03.hs*.

Till now we only had our 5 fix predefined meteo stations. Now we want a first page with a list of all stations,
a second page with the detailed data of one selected station and an error page. We need a XhrRequest to get a list 
of all stations.
The page with the detailed data of a single station and the error page both need a *Back* button, 
so the user can return to the first page with the station list, and therefore the page rendering functions have to 
return the click events of these *Back* buttons. 

We do all this in the function *body*. The additonal events and the two XhrRequests make the logic 
of this function a little bit more complicated. 
Because we refer to events defined later, we have to use *recursive do*. In the function *body* we don't use
any new function or programming concept!

A word of caution to *recursive do*: It is possible to create event loops that will blow up your program! So be careful! 

## Function *switchPromptlyDyn*

Reflex-Dom has a helper function *switchPromptlyDyn* with the following type:

```switchPromptlyDyn :: Reflex t => Dynamic t (Event t a) -> Event t a```

It just unwraps an Event out of a Dynamic.

## Function *simpleList*

The real new thing in this example is: We have a variable number of meteo stations to display on the first page!
All our examples so far displayed a static predefined number of HTML elements.
Reflex-dom has several functions to display a variable number of items: *dyn*, *widgetHold*, *listWithKey*,
*list*, *simpleList*, *listViewWithKey*, *selectViewListWithKey* and *listWithKey'*. We use the function 
*simpleList*. It has the following definition:

```(...) => Dynamic t [v] -> (Dynamic t v -> m a) -> m (Dynamic t [a])```

The first parameter *Dynamic t [v]* is a dynamic list of items we want to render. After we parse the JSON string
we have a value of type *Event t [SmnRecord]*. With *holdDyn* we can convert this to *Dynamic t [SmnRecord]*,
which can be use as first parameter for the function *simpleList*.

The second parameter is a function to render a single element of type *v*, or in our case *SmnRecord*. For each 
station record, we render a *View* button, and the name of the station.
We pack this into a row of a HTML table, so we need *tr* and *td* HTML elements. 

This is done in the function *displayStationRow* with a helper function to create the button:

~~~ { .haskell }
-- | Create the HTML element for a single HTML table row
displayStationRow :: MonadWidget t m => Dynamic t SmnRecord -> m (Event t T.Text)
displayStationRow dynRec =  el "tr" $ do
  evRow <- el "td" $ cmdButton "View" dynRec
  el "td" $ dynText $ staName . fromJust . smnStation <$> dynRec
  return evRow

cmdButton :: MonadWidget t m => T.Text -> Dynamic t SmnRecord -> m (Event t T.Text)
cmdButton label staRec = do
    (btn, _) <- el' "button" $ text label
    let dynNam = smnCode <$> staRec
    return $ tagPromptlyDyn dynNam $ domEvent Click btn  
~~~

When the user clicks on one of the buttons, we need to know, to which station this button belongs.
Therefore clicking on the *view* button, returns an event with the 3-letter code of the station as payload.

The function *simpleList* aggregates all the return values of the rendering function into a list.

So in our case, the function *simpleList* has the type:

```(...) => Dynamic t [SmnRecord] -> (Dynamic t SmnRecord -> m (Event t T.Text)) -> m (Dynamic t [Event t T.Text])```

We then use the function *switchPromptlyDyn* to unwrap the list of events out of the Dynamic. 
The function *leftmost* then returns the event of the *view* button the user clicked.
We use this event in the function *body* to fetch the detailed (and possibly changed!) data of this station. 

Here is the code:

~~~ { .haskell }
    -- list stations
    el "table" $ do                                              -- put everything into a HTML table
      dynList :: Dynamic t [SmnRecord] <- holdDyn [] evList      -- prepare the first argument for simpleList
      evRowsDyn <- simpleList dynList displayStationRow          -- render all station records
      return $ switchPromptlyDyn $ leftmost <$> evRowsDyn        -- get the correct click event
~~~

Look at *src/xhr03.hs*: Without the comment lines and without the empty lines we have less than 150 lines of code.

**In these 150 LoC we have a SPA application that fetches data from a server,
displays it on 2 data pages. One of the data pages has a tabbed display and we have also some error handling!!**

Of course, there is room for improvment eg sort the station names on the first page, improve error handling,
use CSS to make it easier to look at the screens etc, etc. 


# Appendix - References to Reflex-Dom Resources on the Internet

## Talks / Videos

* Ryan Trinkle: Reflex - Practical Functional Reactive Programming, NYC Haskell User's Group, Part 1 [https://www.youtube.com/watch?v=mYvkcskJbc4](https://www.youtube.com/watch?v=mYvkcskJbc4)
* Ryan Trinkle: Reflex - Practical Functional Reactive Programming, NYC Haskell User's Group, Part 2 [https://www.youtube.com/watch?v=3qfc9XFVo2c](https://www.youtube.com/watch?v=3qfc9XFVo2c)
* Niklas Hambüchen: FRP browser programming with Reflex, HaskellerZ meetup, Zürich [https://www.youtube.com/watch?v=dNGClNsnn24&t=2s](https://www.youtube.com/watch?v=dNGClNsnn24&t=2s)
* Doug Beardsley: Modular Web Snippets with Reflex [https://www.youtube.com/watch?v=8nMC2jL2iUY](https://www.youtube.com/watch?v=8nMC2jL2iUY)
* Greg Hale: On Reflex - Boston Haskell [https://www.youtube.com/watch?v=MfXxuy_CJSk](https://www.youtube.com/watch?v=MfXxuy_CJSk)
* Doug Beardsley: Real Word Reflex [http://mightybyte.net/real-world-reflex/index.html](http://mightybyte.net/real-world-reflex/index.html)

## Main Libraries

* The official reflex repositories: [https://github.com/reflex-frp/](https://github.com/reflex-frp/)

## Documentation

* Reflex [http://reflex-frp.github.io/reflex-frp.org/](http://reflex-frp.github.io/reflex-frp.org/)

## Cheat Sheets

* Cheat Sheet Reflex [https://github.com/reflex-frp/reflex/blob/develop/Quickref.md](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md)
* Cheat Sheet Reflex.Dom [https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md](https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md)

## Reddit

* [https://www.reddit.com/r/reflexfrp/new/](https://www.reddit.com/r/reflexfrp/new/)

## Stackoverflow

* Questions tagged reflex [http://stackoverflow.com/questions/tagged/reflex?sort=newest&pageSize=15](http://stackoverflow.com/questions/tagged/reflex?sort=newest&pageSize=15)

## Tutorials

* Queensland FP Lab blog series: An Introduction to reflex [https://qfpl.io/posts/reflex/basics/introduction/](https://qfpl.io/posts/reflex/basics/introduction/)


## Other Libraries / Projects

* A 2048 clone: [https://github.com/mightybyte/reflex-2048/blob/master/src/Main.hs](https://github.com/mightybyte/reflex-2048/blob/master/src/Main.hs)
* HSnippet: [https://github.com/mightybyte/hsnippet](https://github.com/mightybyte/hsnippet)
* [https://github.com/imalsogreg/my-reflex-recipes](https://github.com/imalsogreg/my-reflex-recipes)
* [https://github.com/themoritz/7guis-reflex](https://github.com/themoritz/7guis-reflex)
* [https://github.com/emmanueltouzery/cigale-timesheet](https://github.com/emmanueltouzery/cigale-timesheet)
* [http://emmanueltouzery.github.io/reflex-presentation]()
