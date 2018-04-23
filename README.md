# Communication Bootstrapping v1.0

(An exercise in software archaeology.)

This is a modernized transcription of the code from the appendix of
[Jake Beal](http://jakebeal.com/)'s
[master's thesis (2002)](https://dspace.mit.edu/bitstream/handle/1721.1/42056/231621486-MIT.pdf?sequence=2), which implements [_A Robust Algorithm for Bootstrapping Communications_](https://groups.csail.mit.edu/mac/projects/amorphous/Bootstrap/).
In Jake Beal's [PhD thesis (2007)](http://web.mit.edu/jakebeal/www/Publications/LearningByLearningToCommunicate.pdf), _Learning by Learning to Communicate_, this algorithm is referred to as _Communication Bootstrapping v1.0_.

Runs in [Chez Scheme](https://cisco.github.io/ChezScheme/):
```scheme
(load "main.scm")
(test (train))
```

Warning: the code does not work (yet), as there is not a single success after training. :(
