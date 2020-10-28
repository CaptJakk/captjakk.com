---
title: My Unexpected Path to Functional Programming
---

Haskell has come an impressive way since I started learning it almost 4 years ago. In January of 2017 when I first
picked up the language, I had resolved that I was going to pick a single programming language that I was going to get
really good at. When I had started programming professionally in the summer of 2015, I was just happy to find out that
I could solve problems with computers outside the confines of contrived undergraduate assignments, something I wasn't
totally convinced of as I graduated college.

By the time I graduated I had really used C++ and Python, and was literate
but unproficient in Java and C. There's a widespread sentiment around programming languages that once you've learned one
you've basically learned them all save for a few tricks here and there, and until fall of my senior year of college I
completely bought into this idea. The most notable difference between any of the programming languages I had touched was
the presence or absence of Garbage Collection.

But something weird happened in fall of my senior year of college. There was a required class titled "Programming
Languages and Compilers" that I had zero interest in taking. I was focusing in security and going to be a 1337 H4x0r,
who had time to study compilers. In the first few weeks of the class I had a professor who was talking about concepts
I neither understood nor cared about, with regards to how closures were represented. I didn't know what a closure was
at all, let alone why they were useful. But there was something that caught my attention in that class regardless. It
was taught in OCaml. Learning how to do the assignments was a painful rewiring of the brain, one that I never actually
faithfully finished until several years later. You didn't have loops, only recursion. I had struggled to get recursion
my freshman year too when it was first introduced. Eventually I got it enough to force my way into a recursive solution
to the problems, but not enough to really think about it naturally.

Nevertheless, after doing a few of these assignments I started looking at OCaml with a confusing fascination that I've
only ever felt when working with Math or when socializing with someone distractingly beautiful.

On the whole, I got my ass handed to me by that class. I did OK by the metrics, but from 2020's point of view, after
working with a [Hindly-Milner]() Type System for many years, I still can't tell you how it infers all of the types, and
I supposedly was able to reproduce the algorithm on a test in that class. I may have graduated school, but I've still
got learning to do. Upon finishing the class I was left with a dual impression of confusion and fascination. In an
alternative history, I would have taken the PL Design class that followed that one, but it was so far off of what I
thought I wanted to do that I ended up forgetting about it. Life got busy, and I moved on.

Around that same time, a friend of mine from my summer internships had put Rust onto my radar. I had been deep into
the operating systems space as it coincided with my goals of being a security professional. Rust was intriguing because
it promised no race conditions and no segfaults...an enticing proposition for someone who had by this time spent many
late night hours chasing race conditions and segfaults. Rust promised that if you could get your code past the compiler,
these issues would be nowhere to be found. But in the spring of 2015, Rust was still pre-1.0 and I didn't have the
patience at the time to be on the cutting edge, and like OCaml I forgot about it as I was preparing for my exit from
school and into industry.

Nervous that I wasn't going to cut it when I got into the larger world I started prepping for the job I was going into
at [Ionic Security](). After a few weeks on the job my fear of not being good at this software development thing started
to subside as I got into a rhythm of solving problems and asking questions of my manager when I got stuck. My role in
the Applied Research office there was to trick regular programs into transparently encrypting people's data with unique
keys that was simultaneously not disruptive to their workflow as well as providing extremely granular data encryption
practices.

Because I was not writing applications from scratch but rather using malware techniques to enhance these
applications, C++ was a rather essential tool for me at that job. Since C++ allowed total control of memory management
we could use it to overwrite sections of the program while they were in live memory, giving us a launchpad for our
"plugins" to the data layer of this software. But that control comes with much danger. On several occasions I was up
'til 3am at the office trying to track down a memory corruption bug that would not have been possible in a garbage
collected language. During some of these debugging episodes, I remembered Rust's promise, and wondered for just a moment
whether it would help here.

As my projects at Ionic evolved I ended up moving to an Objective-C project where we were going to do the same thing we
accomplished on Windows for MacOS. Over the course of the project, the coworker I was working with pushed me into
Swift, since he really disliked Objective-C. I can't say I blame him. In hindsight, Objective-C is the worst language
I've ever worked in, regardless of how much I complain about Javascript. As I was learning Swift, though, I stumbled
across those "closures" again, in the form of the `map` function. For some reason, this time it struck me. I had
probably written hundreds of for loops that all had the same structure of calling some function on an argument in that
list, and never once asked the question why I had done that so many f***ing times and not abstracted over it. Turns out
that in order to do this, you need to be able to take functions as arguments to other functions: something I had never
seen done in C++. Perusing the docs some more I found the `filter` and `reduce` functions as well, reminding me of those
college OCaml exercises again. It hit me like a ton of bricks. The point of that complicated stuff in my OCaml class
was to make it possible to abstract over patterns like this, and this is made possible by turning more things *in the
language* into things that the language could manipulate.

Through sheer happenstance and luck, around the same time I had been reading through some of Paul Graham's old blog
posts and found the famous essay [Beating the Averages](). I cannot overstate the impact that reading this essay had
on the arc of my career, and I'm only 25. I won't try to reproduce the argument here in full, but suffice to say it
convinced me that I needed to go and learn the most powerful language I could possibly find, and then _get really good
at it_. Graham argues that LISP was that language and so you might ask, why did I choose Haskell instead of LISP. Well,
the essay was written in 2001, almost 17 years before I was reading it. So taking the kernel of Graham's advice without
getting too bogged down in his particular language preference, I went to do my own research of what this "most powerful"
language might be. Almost everywhere I looked someone at least mentioned Haskell. With quips like "if it compiles, it is
correct", I was filled with hope at the idea that I wouldn't have to chase down memory corruption bugs any more. The
decision came down to Haskell and Rust and honestly it could have been chance that I chose the other one, but to be
honest I'm glad that I didn't. Having used them both now, I'm glad I invested the time in Haskell.

But that's just where it began.

Learning Haskell in 2017 was not for the faint of heart. It was primarily confined to server software and there was
hardly any map of the territory. The theory was completely foreign, and none of the explanations seemed to really make
sense. None of the intuitions I had built up in my C++ days really applied here. And when you did finally wrap your head
around the theory, translating it into practice was not the easiest thing to do the first few times. Venturing outside
of GHCi into the world of software that did things people hired software developers to do usually invoked a stack of
theory that took several months of dedicated study to learn how to use it as a tool rather than just recognize it in a
book. Navigating the dumpster fire of Cabal vs Stack vs CabalV2 almost made me quit on a couple of occasions...and I'm
lucky enough to have learned in an era where we _had_ stack. In 2017 we had no real IDE to speak of, and the only
productive workflow was ghcid which wasn't bad but is understandably jarring for someone coming from other language
backgrounds. This fueled the narrative that Haskell had terrible tooling, discouraging people from trying it. The
documentation felt dense and ugly compared to the more popular languages. A few members of the community were mean
and condescending. Finally, your boss is telling you to shut the hell up and get back to doing something useful. It was
a _rough life_.

So why did I keep going?

I was in love.

When you first learn the basics of Haskell you immediately get the sense that the solutions to the problems you are
working on could not be more concise yet still understandable. The code you write doesn't usually look too dissimilar
from the notation you would choose to describe ideas to a colleague on a whiteboard. The way it cuts right to the
essence of your problem without all of the accounting and bookkeeping is a breath of fresh air, especially for those of
us who come from a C++ background.

But this beauty isn't purely subjective either. Most of it comes from a ruthless commitment to a couple of principles
and then having the patience to let things take as long as they need to to be right. This ruthless commitment to purity
has certainly slowed it's growth in adoption, but it also means that the language is (mostly) consistent and complete.
It regularly makes apparent _fundamental truths_ and frames them in such a stark manner that you get the sense that
you're peeking behind the curtain and seeing whatever your religion's equivalent of God is. This might seem like a
ridiculous claim, and in some sense it is, but regardless, it was and still is the way I _feel_ about it. Studying
math or physics can give you a lot of those same feelings, but with Haskell we get it in a programming language that we
can do everyday work in, which I think is rather special.

Haskell will rewire your brain, and it has a good chance of "ruining" you as a developer as well. It can be really tough
to go back to some other programming languages when you've gotten a taste of what working in Haskell is like. Most of
the time, it's just that the way you would do something in Haskell is so short in terms of the number of ideas you have
to combine to solve a problem combined with a fairly minimal syntax. But other times you'll realize that the tool you
want to use to solve a problem is not just painful in another language, but sometimes flat out doesn't exist. This
happened to me just the other day when trying to define the [Fixpoint Functor]() in TypeScript.

Finally, the skillcap on Haskell is seemingly endless. This is a double edged sword. It can be very easy to get
distracted by trying to find the most effective way to express your ideas: combining elegance, efficiency, type safety,
and simplicity. I would be lying if I said I didn't frequently try to solve an already solved problem better than I had
before. This isn't wasted effort but good judgement is key here to make sure you don't fall off the deep end. Unless
you work in academia you probably still have customers to satisfy. There's a joke that there's no such thing as an
advanced Haskeller, only varying levels of intermediate. In reality this is false, but the sentiment strikes at the
heart of what I'm talking about. I have been going balls to the wall learning new things about it over the last 4 years
and still feel like I have so much more to learn. Not that I _have to_ learn to be productive. So much I _can_ learn if
I want to. The naive interpretation of this is that Haskell is harder to learn than anything else, and that makes it
worse. I firmly believe this is wrong. There's a lot to learn because there's so much you can do with it.

2017 was a long time ago, though. So what are things like in 2020? I am happy to report that _most_ of the issues I had
when learning Haskell have substantially improved. In some cases the weaknesses have actually turned around 180 into
strengths that surpass even the languages that used to trounce Haskell.

Let's start with the IDE problem. I think that the IDE story in Haskell is not far away from being best in class. This
is primarily due to the fact that it exists now, is fast, and fairly reliable. But on top of that, due to the
fundamental property of being a statically typed pure functional language, the IDE actually knows a lot more than even
a TypeScript IDE knows about your code. The implications of this are becoming more apparent by the week. In editor code
evaluation feels magical, and Tactics will give you a glimpse into the future of programming, a topic I will talk about
in my next post. Haskell's platform support is growing, but this is one of the areas it will likely remain behind the
competition, possibly forever. Haskell is a hard language to implement, and that means it takes the same amount of work
to get it to run at all in one place, let alone in many. Nevertheless, with projects like [Reflex Platform]() and
growing GHC ARM support, getting Haskell to run on the web, phones, or in our case here at [Start9 Labs](), the
Raspberry Pi works pretty well. The maps of the territory have also gotten better. Hands down the best resource for
this is Stephen Diehl's [What I Wish I Knew When Learning Haskell](). There is a wealth of information here that helps
connect all the dots. Cabal and Stack both work well for setting up projects. The community has been significanly more
welcoming. The documentation looks the same as it ever has, but I've found that the documentation that [Hackage]()
provides, while not suitable as _learning_ material, is better than the competition when it comes to actually getting
things done. For learning material you will still probably need to read books instead of blog posts, but the books are
good. [Haskell Programming from First Principles]() was instrumental in getting me from zero to productive in the
language.

So if you're even thinking of learning Haskell in 2020, I highly encourage you give it a try, but most importantly,
don't do it alone. Haskell is _hard_. Doing hard things by yourself is neither fun nor productive. Ask questions on
[Reddit](reddit.com/r/haskell). Join [FPChat](functionalprogramming.slack.com), seriously this one was huge for me.
Contrary to anything you may have heard, Haskell is not dying, it's on a growth trajectory. I promise that not only will
it be worth your time, but you have what it takes to do it.