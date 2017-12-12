# Brainf*ck-Haskell

This is an interpreter for the Brainf\*ck language written in Haskell. The framework for this came from [pocmo's Python-Brainf*ck project](https://github.com/pocmo/Python-Brainfuck/blob/master/brainfuck.py "Go see the project").

## Usage

The best way to run this project is to either run it with cabal or to build
it into an executable and then run it.

```zsh
# Test this using the hello_world file

# Using cabal directly
cabal run hello_world.bf

# Building and then running the executable
cabal build
./dist/build/bf-haskell/bf-haskell
```

## Advantages of Haskell

Moving from an interpreted language like Python to a functional language like
Haskell gives a much more "tuned" performance. Although the esoteric BF language
will likely see very little use that requires top-notch performance, just the
fact that it now exists means that it is a possibility. This is a common
advantage brought up when talking about any functional language.

### "Features"

Writing an interpreter for BF in a functional language proved to be fairly
difficult, but there are some cool things to look for in the code.

*Note that many of these may be obvious solutions for a functional programmer,
but these are all things that I really enjoyed solving/implementing.*

#### Bracemap

For control structures, BF has '[' and ']', which work like a `while` loop
in a regular language. To accomodate this, I have a "bracemap" that is created
after the code is cleaned of any non-command symbols. This map gives an index
for each brace and it's partner brace. If the interpreter deems that it is
appropriate to make a conditional jump, the bracemap is referenced for the previous
location in the code.

This idea came from the template project (see link above).

#### Character/Symbol Validation

The template project used a filter to remove non-valid characters. I could have
done this, but chose to filter them one by one as I built the "program code".
In a perfect world, I could build a bracemap simutaneously, but that would have
required a lot more construction code. Regardless, I think the `valid_char` function
is implemented cleverly (or "hackerly").

## Difficulties

There were many difficulties I ran into porting this project from the framework.

### The Haskell Ecosystem

When I start a project, I generally look at what tools are available, specifically
for the command line because I use Emacs, as well as any plugins. I quickly found
a haskell-mode (normal to find these for even smaller languages), and a more
advanced scion-server plugin. 

I attempted to install scion-server using cabal, but quickly found some cryptic
dependencies issues. After Googling it, I found that this is not only an existing
issue, but one that is well known but not quite well understood. Very few "easy"
solutions exist that I was willing to invest my time into. I did discover that
some sandbox functionality exists in cabal, but that did not solve the
dependency issues.

After finding some more tutorials on using cabal's project management and sandbox
tools, I had a "Hello World" project running after about an hour, with Emacs
running with syntax highlighting, autocomplete and flycheck.

### Functional Paradigms

The largest problem I ran into was the restriction that functional programming 
places on the programmer that does not allow imperitive paradigms. My initial
approaches involved many more `data` objects (similar to structs) which all had
their own pseudo-OOP functions. Passing these around quickly proved to be more
trouble than it was worth. Ideally I could reduce my current solution down even
more, but this was the easiest way for me to wrap my mind around it.

### IO and Monads

Monads have a reputation for being one of the hardest concepts in programming to
grasp, even for the most seasoned of developers. I can still say that I barely
understand it. After initially attempting to brute force my solution, I started
to read more about it. Once I started to understand that the compiler would
absolutely not allow you to discard an IO Monad once you've got an object wrapped
around it, I was able to start designing my code to be accomadating to that, and
it started to come together much more quickly.

