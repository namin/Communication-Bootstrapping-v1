# Communication Bootstrapping v1.0

(An exercise in software archaeology.)

This is a modernized transcription of the code from the appendix of
[Jake Beal](http://jakebeal.com/)'s
[master's thesis (2002)](https://groups.csail.mit.edu/mac/projects/amorphous/paperlisting.html#beal-masters), which implements [_A Robust Algorithm for Bootstrapping Communications_](https://groups.csail.mit.edu/mac/projects/amorphous/Bootstrap/).
In Jake Beal's [PhD thesis (2007)](http://web.mit.edu/jakebeal/www/Publications/LearningByLearningToCommunicate.pdf), _Learning by Learning to Communicate_, this algorithm is referred to as _Communication Bootstrapping v1.0_. A follow up by Jake Beal and Gerry Sussman is [_Engineered Robustness by Controlled Hallucinations_](http://www.aaai.org/Papers/Symposia/Fall/2008/FS-08-06/FS08-06-002.pdf).

Runs in [Chez Scheme](https://cisco.github.io/ChezScheme/):
```scheme
(load "main.scm")
(test (train))
```

