function predictedClass = kNearestNeighbors(k, x, numClasses, class, counts)

numData = length(class);

% compute distances
dist = sum(abs(repmat(x,[numData 1]) - counts),2);

% pick top k
topk = sortrows([dist (1:numData)']);
% full(topk(1:k,:))
topk = topk(1:k,2);
% full([-1 x; class(topk)' counts(topk,:)])

votes = zeros([1 numClasses]);
for i = 1:numClasses
  votes(i) = sum(class(topk)==i);
end

[~, predictedClass] = max(votes);