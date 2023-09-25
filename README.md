# Communication Bootstrapping v1.0

(An exercise in software archaeology.)

This is a modernized transcription of the code from the appendix of
[Jake Beal](http://jakebeal.com/)'s
[master's thesis (2002)](https://groups.csail.mit.edu/mac/projects/amorphous/paperlisting.html#beal-masters) and
[AI Memo (2001)](https://dspace.mit.edu/bitstream/handle/1721.1/6082/AIM-2001-016.pdf?sequence=2),
which implements [_A Robust Algorithm for Bootstrapping Communications_](https://groups.csail.mit.edu/mac/projects/amorphous/Bootstrap/).
In Jake Beal's [PhD thesis (2007)](http://web.mit.edu/jakebeal/www/Publications/LearningByLearningToCommunicate.pdf), _Learning by Learning to Communicate_, this algorithm is referred to as _Communication Bootstrapping v1.0_. A follow up by Jake Beal and Gerry Sussman is [_Engineered Robustness by Controlled Hallucinations_](https://web.archive.org/web/20211024190753/http://aaai.org/Papers/Symposia/Fall/2008/FS-08-06/FS08-06-002.pdf) ([code](https://github.com/namin/hallucinations)).

Runs in [Chez Scheme](https://cisco.github.io/ChezScheme/):
```scheme
(load "main.scm")
(test (train))
```

The python script, ran as `python ml_bootcomm.py`, re-creates the problem and solution delegating the learning to multi-layer perceptrons using pytorch. Communication is plugging in the encoder and decoder of two distinct autoencoders. The encoding of the sentence is a multi-hot encoding of the phrase parts, but in the spirit of the problem here, the encoding is dynamically set as the phrase parts are discovered.
