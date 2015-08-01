genre = load('../data/genredata.dat') + 1;

counts = spconvert(load('../data/wordcounts.dat'));


tempGenre = genre;
predictions = zeros(size(genre));

for i = 1:length(genre)
  
  tempGenre(i) = -1;
  
  [prior, likelihood] = naiveBayesTrain(2, tempGenre, counts, 1);
  
  [predictedClass, predictedLogProbs] = naiveBayesTest(2, prior, likelihood, counts(i,:));
  
  predictions(i) = predictedClass;
  
  tempGenre(i) = genre(i);
  
end

confusionMatrix = zeros([2 2]);
for i = 1:2
  for j = 1:2
    confusionMatrix(i,j) = sum(predictions==i & genre==j);
  end
end


confusionMatrix

mean(predictions == genre)

