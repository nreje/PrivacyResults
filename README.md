# PrivacyResults
Privacy Results for Membership Inference and Maximum Knowledge

How to read the acronyms:
plt(X)(Y)(Z)(Q)(V)\_(W) -> X is either "" for Adult dataset, P for Polish or A for Avila.
                     Y is either P for Parametric, D for Decision Tree, CC for Cat-All + Cart (CAC) or CP for Cat-All + PMM (CAP).
                     Z is either some specific synthesizing order ("",H,L) for P and D or "" for the only order used for CC and CP.
                     Q is either T for True synthesizing or "" for the opposite.
                     V is either S for smoothed out numerical variables, Se for selective synthesizing or "" for nothing special.
                     W is either Diff for difference of true and false positive, ROC for ROC curve or UI for plot with (u)tility and membership (i)nference together.

The difference plots (Diff) show how the difference varies for the different synthesizer combinations along a threshold span between 10^(-50) to 10^20.
The ROC curve plots (ROC) show ROC curves for the different synthesizer combinations, the false positive rate as the x-axis and the true positive rate as the y-axis.
The plots with utility and membership inference results shown together (UI) are over the different numbers of synthetic datasets used (m). The result from the best performing weight with the best threshold is used to represent the membership inference results while the above 90 % regression fit results are used as utility accuracy.

The weights are actually the standard deviations used in a gaussian function used to see how close original records are to the synthetic records. There are three weights used: Low, Medium and High.
For Adult Low is 1 for all variables, High is 0.1 while Medium uses 1 for all numerical variables while categorical has 0.1.
The same is true for Polish.
Avila instead has 0.1,0.01 and 0.001 for Low to High for all variables.

The Membership Inference method works as follows: For an original record we compute the difference for each variable and apply a gaussian function on each and multiply the values into a similarity score for that original record and one synthetic record. We do the same with each synthetic record in the synthetic datasets and add them all together to get a similarity score for that original record and the synthetic datasets. A threshold is then chosen to separate those with a similarity score below as not used in the generation process and those above as used.

All plots use either Medium or High because Low was either worse or too close to another to be of much interest.

The number of synthetic datasets used together tested are [1,2,3,5,10,20,50,100].

Only Adult has results for other orders than the original order and results for smoothing and selective.
