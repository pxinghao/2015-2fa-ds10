function [predictedClass, predictedLogProbs] = naiveBayesTest(numClasses, prior, likelihood, counts)

predictedLogProbs = log(prior) + sum(repmat(counts, [numClasses 1]) .* log(likelihood),2);
[~, predictedClass] = max(predictedLogProbs);