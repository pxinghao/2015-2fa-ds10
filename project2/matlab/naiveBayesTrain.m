function [prior, likelihood] = naiveBayesTrain(numClasses, class, counts, priorCount)

prior = zeros([numClasses 1]);

likelihood = zeros([numClasses size(counts,2)]);

for ci=1:numClasses
  prior(ci) = sum(class==ci);
  
  p = sum(counts(class==ci,:)) + priorCount;
  likelihood(ci,:) = p / sum(p);
end

prior = prior / sum(prior);
