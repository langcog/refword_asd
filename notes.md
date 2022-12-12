# Long discussion notes for later use

\citet{Alhama2019} lay out an agenda for formal rule-learning research. We will now use our theoretical and empirical results to address five of the desiderata on that agenda.\newline



\noindent \textbf{Desideratum 1} Investigate which features should be
encoded in the input representation, and quantify the overlap
of features needed for generalization to occur.
\newline

The method of representation impacts whether there is a natural notion of similarity between entities and the ability of models to generalize to examples unseen in training. These two attributes are deeply related; if there is a natural notion of similarity between vector representations, then models can generalize to inputs with representations that are similar to those seen in training.

In order to discuss how representation impacts generalization, we will need explain some properties of how neural models are trained. Standard neural models, including the models in this paper, begin with a linear layer where no two input units are connected to the same weight. A easily observed fact about the back-propagation learning algorithm is that if a unit of the input vector is always zero during training, then any weights connected to that unit and only that unit will not change from their initialized values during training. This means that when a standard neural model is evaluated on an input vector that has a non-zero value for a unit that was zero throughout training, untrained weights are used and behavior is unpredictable.

Localist representations are orthogonal and equidistant from one another so there is no notion of similarity and consequently standard neural models have no ability to generalize to new examples. No two representations share a non-zero unit, and so when models are presented with inputs unseen in training, untrained weights are used and behavior is unpredictable.

Distributed representations with binary features also limit generalization, though less severely than localist representations. Localist representations prevent generalization to entities unseen during training, while binary feature representations prevent generalization to features unseen during training. If color and shape are represented as binary features, and a red square and blue circle are seen in training, then a model could generalize to the unseen entities of a blue circle or a red square. However, if no entity that is a circle is seen during training, then the binary feature representing the property of being a circle is zero throughout training and untrained weights are used when the model is presented with a entity that is a circle and behavior is unpredictable.

Distributed representations with analog features do not inhibit generalization in the same way. If height is represented as a binary feature, then a single unit represents all height values and is always non-zero. Random distributed representations similarly do not inhibit generalization, because all units for all representations are non-zero.

Pre-trained distributed representations are a type of representation that we do not use here, but they have made waves in the field of natural language processing and have not been explored in this context. These representations are randomly initialized and then updated by a model being trained on a downstream task such as language modelling. In pre-trained distributed representations, no single unit of a vector encodes information about properties, but instead information about properties are encoded across all units, where entities that share real-world properties have vector representations that share geometric properties.\newline

\ref{marcus:1998} claims that neural networks are unable to learn universally quantified one-to-one mappings on certain generalization tasks. Specifically, they claim that neural networks can achieve near perfect performance on training inputs and good performance on inputs inside the training space, but are unable to generalize to inputs outside the training space. They define training space as all inputs that are composed of feature values that occur in the training inputs. In the case of localist representations or representations with binary features, this is certainly true, and amounts to a simple analytic observation about back propagation that we made above.

However, the idea of a training space becomes a bit odd when it comes to random distributed representations, because each unit does not have a feature associated with it. In fact, the random nature of these representations almost guarantees that every representation will have a unique value in every unit, so it seems that, in this representational scheme, the training inputs and the training space are one and the same. Then, because we used disjoint generalization tasks, our results show that if random distributed representations are used, then models are able to learn universally quantified one-to-one mappings and generalize to inputs outside the training space. We do not claim that the idea of a training space is misguided, in fact recent results on adversarial testing in natural language processing show quite the opposite is true. However, the definition of the training space as inputs composed of feature values that occur in the training inputs is too narrow when it comes to random distributed representations.


\noindent\textbf{Desideratum 3} Investigate the role of prior experience.
\newline

We presented a analysis of how prior experience with equality might play a role in the ability to learn the Premack task. We demonstrated that if a model is pre-trained to predict a probability distribution over the two classes equal and unequal, and then trained on the Premack task using the class probabilities as inputs, a perfect solution is achieved after a handful of training examples. This supports the idea that individuals that already possess the concept of equality more easily solve the Premack task. \newline

\noindent\textbf{Desideratum 5} Incorporate independently motivated pressures for learning generalizing solutions.
\newline

By using a disjoint generalization task where the primitive items used in training are different from those seen in testing, we are able to test a model's ability to learn a global solution to the target relations of equality and Premack. Our models show high performance under this metric, so overfitting is not an issue. \newline


\noindent\textbf{Desideratum 7} Bridge the gap between levels of analysis: investigate how neural networks perform apparently symbolic computations.
\newline

We provide analytic solutions for how feed forward networks and recursive networks are able to compute the equality relation. These analytic solutions transparently demonstrate the ability of neural models to implement symbolic computations.\newline


\textbf{Desideratum 8} Models should learn spontaneously from
brief or limited exposure
\newline

For each model and task, we provided results describing how test accuracy changes according to number of training examples and model parameters. While most models require at least 1000 examples to achieve perfect performance, only a few dozen examples were necessary to achieve performance above chance by a statistically significant amount.

The Premack model with equality pre-training learns a perfect solution from only a handful of examples, which indicates that pre-training is a viable route for finding models able to learn from limited data.
