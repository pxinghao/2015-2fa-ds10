% features = [2     3     6     8    10    11    14    18    19    20    25    26    27    30    36    52    60    81   189   455   555];
features = 1:5000;

genre = load('../data/genredata.dat') + 1;

counts = spconvert(load('../data/wordcounts.dat'));
counts = counts(:,features);

fid = fopen('../data/words.dat');
words = textscan(fid,'%s','Delimiter','\n');
words = words{1}(features);
fclose(fid);
nWords = length(words);

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





% Most discriminative features

[prior, likelihood] = naiveBayesTrain(2, genre, counts, 1);

sortedLikelihoods = sortrows([abs(likelihood(1,:) - likelihood(2,:)); 1:nWords]');

features = sort(sortedLikelihoods(end-100:end,2))';